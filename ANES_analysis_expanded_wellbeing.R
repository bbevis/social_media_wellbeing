#############################################
#
#       ANES expanded wellbeing mediation
#       Full sample and age moderator models
#
#############################################

library(dplyr)
library(mediation)

##############################################
# Parameters

n_sims <- 1000
ci_level <- 0.95
set.seed(12345)

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
  "total_platform_frequency",
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency"
)

IV <- "political_orientation"

full_sample_controls <- c(
  "age",
  "gender",
  "race_ethnicity",
  "income",
  "education"
)

age_moderator_controls <- c(
  "gender",
  "race_ethnicity",
  "income",
  "education"
)

##############################################
# Full sample mediation

run_full_sample_set <- function(data) {
  results_list <- list()
  
  for (Y in outcomes) {
    for (M in mediators) {
      
      all_vars <- c(Y, M, IV, full_sample_controls)
      
      model_data <- data %>%
        dplyr::select(all_of(all_vars)) %>%
        na.omit()
      
      if (nrow(model_data) < 50) next
      
      Mz <- paste0(M, "_z")
      Yz <- paste0(Y, "_z")
      
      model_data[[Mz]] <- as.numeric(scale(model_data[[M]]))
      model_data[[Yz]] <- as.numeric(scale(model_data[[Y]]))
      
      controls_str <- paste(full_sample_controls, collapse = " + ")
      
      formula.M <- as.formula(paste(Mz, "~", IV, "+", controls_str))
      formula.Y <- as.formula(paste(Yz, "~", IV, "+", Mz, "+", controls_str))
      
      model.M <- lm(formula.M, data = model_data)
      model.Y <- lm(formula.Y, data = model_data)
      
      model.M$call$formula <- formula.M
      model.Y$call$formula <- formula.Y
      
      med.out <- mediation::mediate(
        model.M,
        model.Y,
        treat = IV,
        mediator = Mz,
        sims = n_sims,
        boot = TRUE
      )
      
      sum_Y <- summary(model.Y)
      coef_Y <- sum_Y$coefficients
      ci_Y <- confint(model.Y, level = ci_level)
      
      results_list[[paste("all", Y, M, sep = "_")]] <- data.frame(
        sample = "all",
        model_type = "mediation",
        outcome = Y,
        mediator = M,
        n = nrow(model_data),
        b_path_est = coef_Y[Mz, "Estimate"],
        b_path_se = coef_Y[Mz, "Std. Error"],
        b_path_t = coef_Y[Mz, "t value"],
        b_path_p = coef_Y[Mz, "Pr(>|t|)"],
        b_path_df = sum_Y$df[2],
        b_path_r2 = sum_Y$r.squared,
        b_path_adj_r2 = sum_Y$adj.r.squared,
        b_path_ci_low = ci_Y[Mz, 1],
        b_path_ci_high = ci_Y[Mz, 2],
        b_mediator_age_interaction_est = NA_real_,
        b_mediator_age_interaction_se = NA_real_,
        b_mediator_age_interaction_t = NA_real_,
        b_mediator_age_interaction_p = NA_real_,
        b_mediator_age_interaction_ci_low = NA_real_,
        b_mediator_age_interaction_ci_high = NA_real_,
        acme_est = med.out$d.avg,
        acme_ci_low = med.out$d.avg.ci[1],
        acme_ci_high = med.out$d.avg.ci[2],
        acme_p = med.out$d.avg.p
      )
    }
  }
  
  bind_rows(results_list)
}

##############################################
# Age moderator mediation

run_age_moderator_set <- function(data) {
  results_list <- list()
  
  for (Y in outcomes) {
    for (M in mediators) {
      
      all_vars <- c(Y, M, IV, "age", age_moderator_controls)
      
      model_data <- data %>%
        dplyr::select(all_of(all_vars)) %>%
        na.omit()
      
      if (nrow(model_data) < 50) next
      
      Mz <- paste0(M, "_z")
      Yz <- paste0(Y, "_z")
      
      model_data[[Mz]] <- as.numeric(scale(model_data[[M]]))
      model_data[[Yz]] <- as.numeric(scale(model_data[[Y]]))
      model_data$age_z <- as.numeric(scale(model_data$age))
      
      controls_str <- paste(c("age_z", age_moderator_controls), collapse = " + ")
      interaction_term <- paste0(Mz, ":age_z")
      
      formula.M <- as.formula(paste(Mz, "~", IV, "+", controls_str))
      formula.Y <- as.formula(
        paste(Yz, "~", IV, "+", Mz, "* age_z +", paste(age_moderator_controls, collapse = " + "))
      )
      
      model.M <- lm(formula.M, data = model_data)
      model.Y <- lm(formula.Y, data = model_data)
      
      model.M$call$formula <- formula.M
      model.Y$call$formula <- formula.Y
      
      med.out <- mediation::mediate(
        model.M,
        model.Y,
        treat = IV,
        mediator = Mz,
        sims = n_sims,
        boot = TRUE
      )
      
      sum_Y <- summary(model.Y)
      coef_Y <- sum_Y$coefficients
      ci_Y <- confint(model.Y, level = ci_level)
      
      results_list[[paste("age_moderator", Y, M, sep = "_")]] <- data.frame(
        sample = "age_moderator",
        model_type = "age_moderator",
        outcome = Y,
        mediator = M,
        n = nrow(model_data),
        b_path_est = coef_Y[Mz, "Estimate"],
        b_path_se = coef_Y[Mz, "Std. Error"],
        b_path_t = coef_Y[Mz, "t value"],
        b_path_p = coef_Y[Mz, "Pr(>|t|)"],
        b_path_df = sum_Y$df[2],
        b_path_r2 = sum_Y$r.squared,
        b_path_adj_r2 = sum_Y$adj.r.squared,
        b_path_ci_low = ci_Y[Mz, 1],
        b_path_ci_high = ci_Y[Mz, 2],
        b_mediator_age_interaction_est = coef_Y[interaction_term, "Estimate"],
        b_mediator_age_interaction_se = coef_Y[interaction_term, "Std. Error"],
        b_mediator_age_interaction_t = coef_Y[interaction_term, "t value"],
        b_mediator_age_interaction_p = coef_Y[interaction_term, "Pr(>|t|)"],
        b_mediator_age_interaction_ci_low = ci_Y[interaction_term, 1],
        b_mediator_age_interaction_ci_high = ci_Y[interaction_term, 2],
        acme_est = med.out$d.avg,
        acme_ci_low = med.out$d.avg.ci[1],
        acme_ci_high = med.out$d.avg.ci[2],
        acme_p = med.out$d.avg.p
      )
    }
  }
  
  bind_rows(results_list)
}

##############################################
# Run models

expanded_results <- bind_rows(
  run_full_sample_set(anes_w3),
  run_age_moderator_set(anes_w3)
)

write.csv(
  expanded_results,
  "anes_w3_mediation_results_expanded_wellbeing.csv",
  row.names = FALSE
)

end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "mins")

cat(
  "Expanded ANES mediation complete. Runtime:",
  round(as.numeric(runtime), 2),
  "minutes\n"
)
