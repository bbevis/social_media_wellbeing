#############################################
#
#       ANES Wave 3 standalone path a models
#
#############################################

library(dplyr)
library(survey)

##############################################
# Parameters

ci_level <- 0.95
save_file_name <- "anes_w3_path_a_results.csv"

##############################################
# Load data

anes_w3 <- read.csv("anes_w3.csv")

##############################################
# Define variables

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
control_vars <- c(
  "age",
  "gender",
  "race_ethnicity",
  "income",
  "education"
)

##############################################
# Helpers

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

run_path_a_model <- function(mediator) {
  model_data <- anes_w3 %>%
    dplyr::select(all_of(c(mediator, IV, control_vars, weight_var))) %>%
    na.omit()

  if (nrow(model_data) < 50) return(NULL)

  model_data$mediator_z <- as.numeric(scale(model_data[[mediator]]))

  design <- svydesign(
    ids = ~1,
    weights = ~w3weight,
    data = model_data
  )

  formula.M <- as.formula(
    paste("mediator_z ~", IV, "+", paste(control_vars, collapse = " + "))
  )

  model.M <- svyglm(formula.M, design = design)
  path_a <- model_term_stats(model.M, IV, "mediator_z", model_data)

  data.frame(
    mediator = mediator,
    IV = IV,
    n = nrow(model_data),
    estimate = path_a$estimate,
    se = path_a$se,
    t = path_a$statistic,
    p = path_a$p,
    df = path_a$df,
    r2 = path_a$r2,
    adjusted_r2 = path_a$adjusted_r2,
    ci_low = path_a$ci_low,
    ci_high = path_a$ci_high
  )
}

##############################################
# Run models

path_a_results <- bind_rows(lapply(mediators, run_path_a_model))

write.csv(path_a_results, save_file_name, row.names = FALSE)

print(path_a_results)
