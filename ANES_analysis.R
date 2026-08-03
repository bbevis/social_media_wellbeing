#############################################
#
#       ANES Wave 3 mediation analysis
#
#############################################

library(dplyr)
library(survey)

##############################################
# Parameters

set.seed(1234)

run_model_types <- c(
  "all",
  "young_adults",
  "older_adults",
  "age_moderator"
)

n_replicates <- 1000
ci_level <- 0.95
save_file_name <- "anes_w3_mediation_results.csv"
start_time <- Sys.time()

##############################################
# Load data

anes_w3 <- read.csv("anes_w3.csv")

##############################################
# Define variables

outcomes <- c(
  "loneliness",
  "anhedonia",
  "depression",
  "affective_wellbeing",
  "happy",
  "hopeful",
  "afraid",
  "angry",
  "outraged",
  "worried"
)

mediators <- c(
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency",
  "total_platform_frequency"
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

subgroup_controls <- c(
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
  ),
  list(
    sample = "young_adults",
    data = anes_w3 %>% filter(young_adult == 1),
    control_vars = subgroup_controls
  ),
  list(
    sample = "older_adults",
    data = anes_w3 %>% filter(young_adult == 0),
    control_vars = subgroup_controls
  )
)

moderation_controls <- c(
  "gender",
  "race_ethnicity",
  "income",
  "education"
)

age_z_values <- c(-1, 0, 1)

age_z_labels <- c(
  "-1" = "-1 SD",
  "0" = "Mean age",
  "1" = "+1 SD"
)

##############################################

make_survey_designs <- function(model_data) {
  design <- svydesign(
    ids = ~1,
    weights = ~w3weight,
    data = model_data
  )

  list(
    design = design,
    replicate_design = as.svrepdesign(
      design,
      type = "bootstrap",
      replicates = n_replicates,
      mse = TRUE
    )
  )
}

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
  coefficient <- summary(model)$coefficients[term, ]
  interval <- as.numeric(confint(model, parm = term, level = ci_level))
  r2 <- weighted_r2(model, outcome, model_data)

  list(
    estimate = unname(coefficient[1]),
    se = unname(coefficient[2]),
    statistic = unname(coefficient[3]),
    p = unname(coefficient[4]),
    df = df.residual(model),
    r2 = unname(r2["r2"]),
    adjusted_r2 = unname(r2["adjusted_r2"]),
    ci_low = interval[1],
    ci_high = interval[2]
  )
}

replicate_mediation_stats <- function(
    replicate_design,
    formula.M,
    formula.Y,
    mediator_term,
    moderated = FALSE,
    a_interaction_term = NULL,
    b_interaction_term = NULL
) {
  effect_estimator <- function(replicate_weights, data) {
    # These weighted fits are evaluated across the survey replicate weights.
    data$.replicate_weight <- replicate_weights
    model.M <- lm(formula.M, data = data, weights = .replicate_weight)
    model.Y <- lm(formula.Y, data = data, weights = .replicate_weight)

    coefficients.M <- coef(model.M)
    coefficients.Y <- coef(model.Y)

    if (moderated) {
      # Average the age-conditional indirect effect across the weighted sample.
      conditional_a <- coefficients.M[IV] +
        coefficients.M[a_interaction_term] * data$age_z
      conditional_b <- coefficients.Y[mediator_term] +
        coefficients.Y[b_interaction_term] * data$age_z
      acme <- weighted.mean(
        conditional_a * conditional_b,
        replicate_weights,
        na.rm = TRUE
      )
    } else {
      acme <- coefficients.M[IV] * coefficients.Y[mediator_term]
    }

    ade <- coefficients.Y[IV]
    total_effect <- ade + acme
    proportion_mediated <- ifelse(
      abs(total_effect) > .Machine$double.eps,
      acme / total_effect,
      NA_real_
    )

    c(
      acme = unname(acme),
      ade = unname(ade),
      total_effect = unname(total_effect),
      proportion_mediated = unname(proportion_mediated)
    )
  }

  replicate_results <- withReplicates(
    replicate_design,
    effect_estimator
  )

  estimates <- coef(replicate_results)
  standard_errors <- SE(replicate_results)
  df <- degf(replicate_design)
  critical_value <- qt((1 + ci_level) / 2, df)
  statistics <- estimates / standard_errors

  data.frame(
    estimate = as.numeric(estimates),
    p = 2 * pt(abs(statistics), df, lower.tail = FALSE),
    ci_low = as.numeric(estimates - critical_value * standard_errors),
    ci_high = as.numeric(estimates + critical_value * standard_errors),
    row.names = names(estimates)
  )
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

  model_data$mediator_z <- as.numeric(scale(model_data[[mediator]]))
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
  b_interaction_term <- "mediator_z:age_z"

  if (moderated) {
    formula.M <- as.formula(
      paste("mediator_z ~", IV, "* age_z +", controls)
    )
    formula.Y <- as.formula(
      paste("outcome_z ~", IV, "+ mediator_z * age_z +", controls)
    )
    formula.c <- as.formula(
      paste("outcome_z ~", IV, "+ age_z +", controls)
    )
  } else {
    formula.M <- as.formula(paste("mediator_z ~", IV, "+", controls))
    formula.Y <- as.formula(
      paste("outcome_z ~", IV, "+ mediator_z +", controls)
    )
    formula.c <- as.formula(paste("outcome_z ~", IV, "+", controls))
  }

  designs <- make_survey_designs(model_data)
  model.M <- svyglm(formula.M, design = designs$design)
  model.Y <- svyglm(formula.Y, design = designs$design)
  model.c <- svyglm(formula.c, design = designs$design)

  list(
    model.M = model.M,
    model.Y = model.Y,
    a = model_term_stats(model.M, IV, "mediator_z", model_data),
    b = model_term_stats(model.Y, "mediator_z", "outcome_z", model_data),
    c = model_term_stats(model.c, IV, "outcome_z", model_data),
    c_prime = model_term_stats(model.Y, IV, "outcome_z", model_data),
    a_interaction = if (moderated) {
      model_term_stats(model.M, a_interaction_term, "mediator_z", model_data)
    } else {
      missing_term_stats()
    },
    b_interaction = if (moderated) {
      model_term_stats(model.Y, b_interaction_term, "outcome_z", model_data)
    } else {
      missing_term_stats()
    },
    effects = replicate_mediation_stats(
      designs$replicate_design,
      formula.M,
      formula.Y,
      "mediator_z",
      moderated,
      a_interaction_term,
      b_interaction_term
    ),
    a_interaction_term = a_interaction_term,
    b_interaction_term = b_interaction_term
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

      fit <- fit_analysis_models(model_data, control_vars)
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

make_slope_results <- function(
    model,
    main_term,
    interaction_term,
    path,
    outcome,
    mediator
) {
  model_coef <- coef(model)
  model_vcov <- vcov(model)
  model_df <- df.residual(model)
  critical_t <- qt(0.975, model_df)

  make_row <- function(estimate, se, statistic, age_level = NA_character_) {
    t_value <- estimate / se

    data.frame(
      path = path,
      outcome = outcome,
      mediator = mediator,
      statistic = statistic,
      age_level = age_level,
      estimate = estimate,
      se = se,
      t = t_value,
      p = 2 * pt(abs(t_value), model_df, lower.tail = FALSE),
      ci_low = estimate - critical_t * se,
      ci_high = estimate + critical_t * se
    )
  }

  interaction_est <- model_coef[interaction_term]
  interaction_se <- sqrt(model_vcov[interaction_term, interaction_term])

  slope_rows <- lapply(age_z_values, function(age_z) {
    slope_est <- model_coef[main_term] + age_z * interaction_est
    slope_var <- model_vcov[main_term, main_term] +
      age_z^2 * model_vcov[interaction_term, interaction_term] +
      2 * age_z * model_vcov[main_term, interaction_term]

    make_row(
      slope_est,
      sqrt(slope_var),
      "simple_slope",
      unname(age_z_labels[as.character(age_z)])
    )
  })

  bind_rows(
    make_row(interaction_est, interaction_se, "interaction"),
    slope_rows
  )
}

make_prediction_results <- function(model, model_data, path, outcome, mediator) {
  model_frame <- model.frame(model)
  weights <- model_data[[weight_var]]
  reference_data <- model_frame[1, , drop = FALSE]
  reference_data[] <- lapply(model_frame, function(values) {
    if (is.numeric(values)) weighted.mean(values, weights) else values[1]
  })

  predictor <- if (path == "a") IV else "mediator_z"
  predictor_values <- if (path == "a") {
    seq(min(model_data[[IV]]), max(model_data[[IV]]), length.out = 100)
  } else {
    seq(-2, 2, length.out = 100)
  }

  prediction_grid <- expand.grid(
    predictor_value = predictor_values,
    age_z = age_z_values
  )
  new_data <- reference_data[rep(1, nrow(prediction_grid)), , drop = FALSE]
  new_data[[predictor]] <- prediction_grid$predictor_value
  new_data$age_z <- prediction_grid$age_z

  prediction <- predict(model, newdata = new_data, se.fit = TRUE)
  estimate <- as.numeric(coef(prediction))
  prediction_se <- as.numeric(SE(prediction))
  critical_t <- qt((1 + ci_level) / 2, df.residual(model))

  data.frame(
    path = path,
    outcome = outcome,
    mediator = mediator,
    age_level = unname(age_z_labels[as.character(prediction_grid$age_z)]),
    predictor_value = prediction_grid$predictor_value,
    estimate = estimate,
    ci_low = estimate - critical_t * prediction_se,
    ci_high = estimate + critical_t * prediction_se
  )
}

run_age_moderation_set <- function(data, sample_name, control_vars) {
  results_list <- list()
  slope_results_list <- list()
  prediction_results_list <- list()
  
  for (Y in outcomes) {
    for (M in mediators) {
      model_data <- prepare_model_data(data, Y, M, control_vars, TRUE)
      if (is.null(model_data)) next

      fit <- fit_analysis_models(model_data, control_vars, TRUE)

      slope_results_list[[paste(Y, M, sep = "_")]] <- bind_rows(
        make_slope_results(
          fit$model.M,
          IV,
          fit$a_interaction_term,
          "a",
          Y,
          M
        ),
        make_slope_results(
          fit$model.Y,
          "mediator_z",
          fit$b_interaction_term,
          "b",
          Y,
          M
        )
      )

      prediction_results_list[[paste(Y, M, sep = "_")]] <- bind_rows(
        make_prediction_results(fit$model.M, model_data, "a", Y, M),
        make_prediction_results(fit$model.Y, model_data, "b", Y, M)
      )

      results_list[[paste(sample_name, Y, M, sep = "_")]] <-
        make_result_row(
          fit,
          sample_name,
          "age_moderator",
          Y,
          M,
          nrow(model_data)
        )
    }
  }
  
  list(
    results = bind_rows(results_list),
    slope_results = bind_rows(slope_results_list),
    prediction_results = bind_rows(prediction_results_list)
  )
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

  write.csv(
    age_moderation_results$slope_results,
    "anes_age_moderation_results.csv",
    row.names = FALSE
  )

  write.csv(
    age_moderation_results$prediction_results,
    "anes_age_moderation_plot_results.csv",
    row.names = FALSE
  )
}

write.csv(anes_w3_mediation_results, save_file_name, row.names = FALSE)

end_time <- Sys.time()
runtime_minutes <- difftime(end_time, start_time, units = "mins")

cat(
  "\nANES analysis runtime:",
  round(as.numeric(runtime_minutes), 2),
  "minutes\n"
)
