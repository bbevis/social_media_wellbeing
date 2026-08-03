#############################################
#
#       ANES standalone age-moderated path a models
#
#############################################

library(dplyr)
library(survey)

##############################################
# Parameters

ci_level <- 0.95
results_file_name <- "anes_path_a_age_moderation_results.csv"
plot_results_file_name <- "anes_path_a_age_moderation_plot_results.csv"

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
# Helpers

make_simple_slopes <- function(model, mediator, n) {
  main_term <- IV
  interaction_term <- paste0(IV, ":age_z")
  model_coef <- coef(model)
  model_vcov <- vcov(model)
  model_df <- df.residual(model)
  critical_t <- qt((1 + ci_level) / 2, df = model_df)

  make_row <- function(estimate, se, statistic, age_level = NA_character_) {
    t_value <- estimate / se

    data.frame(
      mediator = mediator,
      n = n,
      statistic = statistic,
      age_level = age_level,
      estimate = estimate,
      se = se,
      t = t_value,
      p = 2 * pt(abs(t_value), model_df, lower.tail = FALSE),
      df = model_df,
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
      estimate = slope_est,
      se = sqrt(slope_var),
      statistic = "simple_slope",
      age_level = unname(age_z_labels[as.character(age_z)])
    )
  })

  bind_rows(
    make_row(
      estimate = interaction_est,
      se = interaction_se,
      statistic = "interaction"
    ),
    slope_rows
  )
}

make_predictions <- function(model, model_data, mediator) {
  model_frame <- model.frame(model)
  model_weights <- model_data[[weight_var]]
  reference_data <- model_frame[1, , drop = FALSE]

  reference_data[] <- lapply(model_frame, function(values) {
    if (is.numeric(values)) {
      weighted.mean(values, model_weights)
    } else {
      values[1]
    }
  })

  prediction_grid <- expand.grid(
    predictor_value = seq(
      min(model_data[[IV]]),
      max(model_data[[IV]]),
      length.out = 100
    ),
    age_z = age_z_values
  )

  new_data <- reference_data[
    rep(1, nrow(prediction_grid)),
    ,
    drop = FALSE
  ]
  new_data[[IV]] <- prediction_grid$predictor_value
  new_data$age_z <- prediction_grid$age_z

  prediction <- predict(model, newdata = new_data, se.fit = TRUE)
  estimate <- as.numeric(coef(prediction))
  prediction_se <- as.numeric(SE(prediction))
  critical_t <- qt((1 + ci_level) / 2, df.residual(model))

  data.frame(
    mediator = mediator,
    n = nrow(model_data),
    age_level = unname(age_z_labels[as.character(prediction_grid$age_z)]),
    predictor_value = prediction_grid$predictor_value,
    estimate = estimate,
    ci_low = estimate - critical_t * prediction_se,
    ci_high = estimate + critical_t * prediction_se
  )
}

run_path_a_age_moderation <- function(mediator) {
  model_data <- anes_w3 %>%
    dplyr::select(
      all_of(c(mediator, IV, "age", control_vars, weight_var))
    ) %>%
    na.omit()

  if (nrow(model_data) < 50) return(NULL)

  model_data$mediator_z <- as.numeric(scale(model_data[[mediator]]))
  model_data$age_z <- as.numeric(scale(model_data$age))

  design <- svydesign(
    ids = ~1,
    weights = ~w3weight,
    data = model_data
  )

  formula.M <- as.formula(
    paste(
      "mediator_z ~",
      IV,
      "* age_z +",
      paste(control_vars, collapse = " + ")
    )
  )
  model.M <- svyglm(formula.M, design = design)

  list(
    slopes = make_simple_slopes(model.M, mediator, nrow(model_data)),
    predictions = make_predictions(model.M, model_data, mediator)
  )
}

##############################################
# Run models

path_a_age_moderation <- lapply(mediators, run_path_a_age_moderation)

path_a_age_moderation_results <- bind_rows(
  lapply(path_a_age_moderation, `[[`, "slopes")
)

path_a_age_moderation_plot_results <- bind_rows(
  lapply(path_a_age_moderation, `[[`, "predictions")
)

write.csv(
  path_a_age_moderation_results,
  results_file_name,
  row.names = FALSE
)

write.csv(
  path_a_age_moderation_plot_results,
  plot_results_file_name,
  row.names = FALSE
)

print(path_a_age_moderation_results)
