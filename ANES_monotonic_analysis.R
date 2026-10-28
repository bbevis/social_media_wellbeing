#############################################
#
#       ANES: ordinal mediation
# proportional-odds mediator model
# categorical outcome-model effect
# applies nonparametric bootstrap (1,000 replicates)
#
#############################################

library(dplyr)
library(survey)
library(mediation)

##############################################
# Parameters

set.seed(1234)

run_model_types <- c(
  "all",
  "age_moderator"
)

n_replicates <- 1000
ci_level <- 0.95
save_file_name <- "anes_monotonic_mediation_results.csv"
po_from <- 1
po_to <- 7
bootstrap_seed <- 1234L
effect_rows <- list()

start_time <- Sys.time()

##############################################
# Load data

anes_w3 <- read.csv("anes_w3.csv")

##############################################
# Define variables

outcomes <- c(
  "loneliness",
  "anhedonia",
  "depression"
)

mediators <- c(
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency"
)

IV <- "political_orientation"
weight_var <- "w3weight"
full_sample_controls <- c(
  "age",
  "gender",
  "race_ethnicity",
  "income",
  "education"
)

analysis_specs <- list(
  list(
    sample = "all",
    data = anes_w3,
    control_vars = full_sample_controls
  )
)

moderation_controls <- c(
  "gender",
  "race_ethnicity",
  "income",
  "education"
)

##############################################

weighted_r2 <- function(model, outcome, model_data) {
  weights <- model_data[[weight_var]]
  observed <- model_data[[outcome]]
  predicted <- as.numeric(fitted(model))
  residual_ss <- sum(weights * (observed - predicted)^2)
  total_ss <- sum(
    weights * (observed - weighted.mean(observed, weights))^2
  )

  r2 <- ifelse(total_ss > 0, 1 - residual_ss / total_ss, NA_real_)
  adjusted_r2 <- ifelse(
    is.finite(r2) && df.residual(model) > 0,
    1 - (1 - r2) * (nrow(model_data) - 1) / df.residual(model),
    NA_real_
  )

  c(r2 = r2, adjusted_r2 = adjusted_r2)
}

model_term_stats <- function(model, term, outcome, model_data) {
  if (!term %in% names(coef(model))) return(missing_term_stats())
  estimate <- unname(coef(model)[term])
  se <- sqrt(vcov(model)[term, term])
  ordinal <- inherits(model, "polr")
  df <- if (ordinal) Inf else df.residual(model)
  critical <- qt((1 + ci_level) / 2, df)
  r2 <- if (ordinal) c(r2 = NA_real_, adjusted_r2 = NA_real_) else
    weighted_r2(model, outcome, model_data)
  list(estimate = estimate, se = unname(se), statistic = estimate / se,
       p = 2 * pt(abs(estimate / se), df, lower.tail = FALSE), df = df,
       r2 = unname(r2["r2"]), adjusted_r2 = unname(r2["adjusted_r2"]),
       ci_low = estimate - critical * se, ci_high = estimate + critical * se)
}


bootstrap_mediation <- function(model.M, model.Y) {
  set.seed(bootstrap_seed)
  mediation::mediate(
    model.m = model.M, model.y = model.Y, treat = IV,
    mediator = "mediator_value", control.value = po_from, treat.value = po_to,
    boot = TRUE, sims = n_replicates,
    boot.ci.type = "perc", conf.level = ci_level, long = TRUE
  )
}

mediation_effects <- function(result) {
  effects <- data.frame(
    estimate = c(result$d.avg, result$z.avg, result$tau.coef, result$n.avg),
    p = c(result$d.avg.p, result$z.avg.p, result$tau.p, result$n.avg.p),
    ci_low = c(result$d.avg.ci[1], result$z.avg.ci[1],
               result$tau.ci[1], result$n.avg.ci[1]),
    ci_high = c(result$d.avg.ci[2], result$z.avg.ci[2],
                result$tau.ci[2], result$n.avg.ci[2]),
    row.names = c("acme", "ade", "total_effect", "proportion_mediated")
  )
  # With no PO-by-mediator term in the outcome model, the 1-to-7 effect
  # divided by six equals the equal average of the six adjacent 1-point effects.
  per_point <- c("acme", "ade", "total_effect")
  effects[per_point, c("estimate", "ci_low", "ci_high")] <-
    effects[per_point, c("estimate", "ci_low", "ci_high")] / (po_to - po_from)
  effects
}

##############################################
prepare_model_data <- function(data, outcome, mediator, control_vars, moderated) {
  variables <- unique(c(
    outcome,
    mediator,
    IV,
    control_vars,
    weight_var,
    if (moderated) "age"
  ))

  model_data <- data %>%
    dplyr::select(all_of(variables)) %>%
    na.omit()

  if (nrow(model_data) < 50) return(NULL)

  model_data <- model_data[is.finite(model_data[[weight_var]]) &
                             model_data[[weight_var]] > 0, , drop = FALSE]
  if (nrow(model_data) < 50) return(NULL)
  if (!all(model_data[[mediator]] %in% 1:7)) stop("Invalid frequency category")
  model_data$mediator_value <- ordered(model_data[[mediator]], levels = 1:7)
  if (any(table(model_data$mediator_value) == 0))
    stop("Empty frequency category: ", mediator)
  contrasts(model_data$mediator_value) <- contr.treatment(7, base = 1)
  model_data$.analysis_weight <- model_data[[weight_var]]
  model_data$outcome_z <- as.numeric(scale(model_data[[outcome]]))
  if (moderated) {
    model_data$age_z <- as.numeric(scale(model_data$age))
  }

  model_data
}

missing_term_stats <- function() {
  as.list(setNames(rep(NA_real_, 9), c(
    "estimate", "se", "statistic", "p", "df",
    "r2", "adjusted_r2", "ci_low", "ci_high"
  )))
}

fit_analysis_models <- function(model_data, control_vars, moderated = FALSE) {
  controls <- paste(control_vars, collapse = " + ")
  a_interaction_term <- paste0(IV, ":age_z")
  b_interaction_term <- "mediator_value:age_z"
  if (moderated) {
    formula.M <- as.formula(paste("mediator_value ~", IV, "* age_z +", controls))
    formula.Y <- as.formula(paste("outcome_z ~", IV, "+ mediator_value * age_z +", controls))
    formula.c <- as.formula(paste("outcome_z ~", IV, "+ age_z +", controls))
  } else {
    formula.M <- as.formula(paste("mediator_value ~", IV, "+", controls))
    formula.Y <- as.formula(paste("outcome_z ~", IV, "+ mediator_value +", controls))
    formula.c <- as.formula(paste("outcome_z ~", IV, "+", controls))
  }
  ordinal <- is.ordered(model_data$mediator_value)
  model.M <- if (ordinal) MASS::polr(
    formula.M, data = model_data, weights = .analysis_weight,
    method = "logistic", Hess = TRUE,
    control = list(maxit = 1000, reltol = 1e-10)
  ) else lm(formula.M, data = model_data, weights = .analysis_weight)
  if (ordinal && model.M$convergence != 0L) stop("Ordinal mediator did not converge")
  model.Y <- lm(formula.Y, data = model_data, weights = .analysis_weight)
  design <- svydesign(ids = ~1, weights = ~w3weight, data = model_data)
  model.c <- svyglm(formula.c, design = design)
  model.M$call$formula <- formula.M
  model.Y$call$formula <- formula.Y
  mediation_result <- bootstrap_mediation(model.M, model.Y)
  list(
    model.M = model.M, model.Y = model.Y, model.c = model.c,
    a = model_term_stats(model.M, IV, "mediator_value", model_data),
    b = if (ordinal) missing_term_stats() else
      model_term_stats(model.Y, "mediator_value", "outcome_z", model_data),
    c = model_term_stats(model.c, IV, "outcome_z", model_data),
    c_prime = model_term_stats(model.Y, IV, "outcome_z", model_data),
    a_interaction = if (moderated)
      model_term_stats(model.M, a_interaction_term, "mediator_value", model_data)
      else missing_term_stats(),
    b_interaction = if (moderated && !ordinal)
      model_term_stats(model.Y, b_interaction_term, "outcome_z", model_data)
      else missing_term_stats(),
    effects = mediation_effects(mediation_result),
    mediation_result = mediation_result,
    a_interaction_term = a_interaction_term, b_interaction_term = b_interaction_term
  )
}

record_effects <- function(fit, sample_name, model_type, outcome, mediator, model_data) {
  key <- paste(sample_name, outcome, mediator, sep = "__")
  x <- mediation_effects(fit$mediation_result)
  effect_rows[[key]] <<- data.frame(
    sample = sample_name, model_type = model_type,
    mediator = mediator, outcome = outcome, age_scenario = "averaged",
    effect = rownames(x), estimate = x$estimate, p = x$p,
    ci_low = x$ci_low, ci_high = x$ci_high, n = nrow(model_data),
    po_from = po_from, po_to = po_to, replicates = n_replicates,
    method = "mediation nonparametric bootstrap; equal average per 1-point PO increase", stringsAsFactors = FALSE
  )
}

make_result_row <- function(
    fit,
    sample_name,
    model_type,
    outcome,
    mediator,
    n
) {
  a <- fit$a
  b <- fit$b
  c_path <- fit$c
  c_prime <- fit$c_prime
  a_interaction <- fit$a_interaction
  b_interaction <- fit$b_interaction
  effects <- fit$effects

  data.frame(
    sample = sample_name,
    model_type = model_type,
    outcome = outcome,
    mediator = mediator,
    IV = IV,
    n = n,
    a_path_est = a$estimate,
    a_path_se = a$se,
    a_path_t = a$statistic,
    a_path_p = a$p,
    a_path_df = a$df,
    a_path_r2 = a$r2,
    a_path_adj_r2 = a$adjusted_r2,
    a_path_ci_low = a$ci_low,
    a_path_ci_high = a$ci_high,
    a_iv_age_interaction_est = a_interaction$estimate,
    a_iv_age_interaction_se = a_interaction$se,
    a_iv_age_interaction_t = a_interaction$statistic,
    a_iv_age_interaction_p = a_interaction$p,
    a_iv_age_interaction_ci_low = a_interaction$ci_low,
    a_iv_age_interaction_ci_high = a_interaction$ci_high,
    b_path_est = b$estimate,
    b_path_se = b$se,
    b_path_t = b$statistic,
    b_path_p = b$p,
    b_path_df = b$df,
    b_path_r2 = b$r2,
    b_path_adj_r2 = b$adjusted_r2,
    b_path_ci_low = b$ci_low,
    b_path_ci_high = b$ci_high,
    b_mediator_age_interaction_est = b_interaction$estimate,
    b_mediator_age_interaction_se = b_interaction$se,
    b_mediator_age_interaction_t = b_interaction$statistic,
    b_mediator_age_interaction_p = b_interaction$p,
    b_mediator_age_interaction_ci_low = b_interaction$ci_low,
    b_mediator_age_interaction_ci_high = b_interaction$ci_high,
    c_path_est = c_path$estimate,
    c_path_se = c_path$se,
    c_path_t = c_path$statistic,
    c_path_p = c_path$p,
    c_path_df = c_path$df,
    c_path_r2 = c_path$r2,
    c_path_adj_r2 = c_path$adjusted_r2,
    c_path_ci_low = c_path$ci_low,
    c_path_ci_high = c_path$ci_high,
    c_prime_est = c_prime$estimate,
    c_prime_se = c_prime$se,
    c_prime_t = c_prime$statistic,
    c_prime_p = c_prime$p,
    c_prime_df = c_prime$df,
    c_prime_r2 = c_prime$r2,
    c_prime_adj_r2 = c_prime$adjusted_r2,
    c_prime_ci_low = c_prime$ci_low,
    c_prime_ci_high = c_prime$ci_high,
    acme_est = effects["acme", "estimate"],
    acme_ci_low = effects["acme", "ci_low"],
    acme_ci_high = effects["acme", "ci_high"],
    acme_p = effects["acme", "p"],
    ade_est = effects["ade", "estimate"],
    ade_ci_low = effects["ade", "ci_low"],
    ade_ci_high = effects["ade", "ci_high"],
    ade_p = effects["ade", "p"],
    total_effect_est = effects["total_effect", "estimate"],
    total_effect_ci_low = effects["total_effect", "ci_low"],
    total_effect_ci_high = effects["total_effect", "ci_high"],
    total_effect_p = effects["total_effect", "p"],
    prop_mediated_est = effects["proportion_mediated", "estimate"],
    prop_mediated_ci_low = effects["proportion_mediated", "ci_low"],
    prop_mediated_ci_high = effects["proportion_mediated", "ci_high"],
    prop_mediated_p = effects["proportion_mediated", "p"]
  )
}

run_mediation_set <- function(data, sample_name, control_vars) {
  results_list <- list()

  for (Y in outcomes) {
    for (M in mediators) {
      model_data <- prepare_model_data(data, Y, M, control_vars, FALSE)
      if (is.null(model_data)) next

      message(sample_name, ": ", Y, " / ", M)
      fit <- fit_analysis_models(model_data, control_vars)
      record_effects(fit, sample_name, "mediation", Y, M, model_data)
      results_list[[paste(sample_name, Y, M, sep = "_")]] <-
        make_result_row(
          fit,
          sample_name,
          "mediation",
          Y,
          M,
          nrow(model_data)
        )
    }
  }

  bind_rows(results_list)
}

run_age_moderation_set <- function(data, sample_name, control_vars) {
  results_list <- list()
  for (Y in outcomes) {
    for (M in mediators) {
      model_data <- prepare_model_data(data, Y, M, control_vars, TRUE)
      if (is.null(model_data)) next
      message(sample_name, ": ", Y, " / ", M, " (sample-averaged ACME)")
      fit <- fit_analysis_models(model_data, control_vars, TRUE)
      record_effects(fit, sample_name, "age_moderator", Y, M, model_data)
      results_list[[paste(Y, M, sep = "_")]] <- make_result_row(
        fit, sample_name, "age_moderator", Y, M, nrow(model_data)
      )
    }
  }
  list(results = bind_rows(results_list))
}

##############################################
# Mediation models

analysis_specs <- analysis_specs[
  vapply(
    analysis_specs,
    function(spec) spec$sample %in% run_model_types,
    logical(1)
  )
]

anes_w3_mediation_results <- bind_rows(lapply(
  analysis_specs,
  function(spec) {
    run_mediation_set(
      data = spec$data,
      sample_name = spec$sample,
      control_vars = spec$control_vars
    )
  }
))

if ("age_moderator" %in% run_model_types) {
  age_moderation_results <- run_age_moderation_set(
    data = anes_w3,
    sample_name = "age_moderator",
    control_vars = moderation_controls
  )

  anes_w3_mediation_results <- anes_w3_mediation_results %>%
    bind_rows(age_moderation_results$results)
}

# Write only after all requested models have completed successfully.
all_effects <- bind_rows(effect_rows)
all_effects$significant <- all_effects$ci_low > 0 | all_effects$ci_high < 0
write.csv(anes_w3_mediation_results, file = save_file_name, row.names = FALSE)
write.csv(all_effects, file = "anes_monotonic_effects.csv", row.names = FALSE)
write.csv(subset(all_effects, effect == "acme"),
          file = "anes_monotonic_acme_results.csv", row.names = FALSE)

print(subset(all_effects, sample == "all" & age_scenario == "averaged" &
               effect == "acme" & significant,
             select = c(mediator, outcome, estimate, ci_low, ci_high)), row.names = FALSE)

end_time <- Sys.time()
runtime_minutes <- difftime(end_time, start_time, units = "mins")

cat(
  "\nANES analysis runtime:",
  round(as.numeric(runtime_minutes), 2),
  "minutes\n"
)
