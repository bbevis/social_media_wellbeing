#############################################
#
#       ANES Wave 3 mediation analysis
#
#############################################

library(dplyr)
library(mediation)

##############################################
# Parameters

FAST_MODE <- FALSE

run_model_types <- c(
  "all",
  "young_adults",
  "older_adults",
  "age_moderator"
)

n_sims <- ifelse(FAST_MODE, 100, 1000)
ci_level <- 0.95
is_temp_run <- FAST_MODE || length(run_model_types) < 4
save_file_name <- ifelse(
  is_temp_run,
  "anes_w3_mediation_results_temp.csv",
  "anes_w3_mediation_results.csv"
)
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
  "affective_wellbeing"
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

##############################################
run_mediation_set <- function(data, sample_name, control_vars) {
  results_list <- list()
  
  for (Y in outcomes) {
    for (M in mediators) {
    
      all_vars <- c(Y, M, IV, control_vars)
    
      model_data <- data %>%
        dplyr::select(all_of(all_vars)) %>%
        na.omit()
    
      if (nrow(model_data) < 50) next
    
      Mz <- paste0(M, "_z")
      Yz <- paste0(Y, "_z")
    
      model_data[[Mz]] <- as.numeric(scale(model_data[[M]]))
      model_data[[Yz]] <- as.numeric(scale(model_data[[Y]]))
    
      controls_str <- paste(control_vars, collapse = " + ")
    
      formula.M <- as.formula(paste(Mz, "~", IV, "+", controls_str))
      formula.Y <- as.formula(paste(Yz, "~", IV, "+", Mz, "+", controls_str))
      formula.c <- as.formula(paste(Yz, "~", IV, "+", controls_str))
    
      model.M <- lm(formula.M, data = model_data)
      model.Y <- lm(formula.Y, data = model_data)
      model.c <- lm(formula.c, data = model_data)
      
      model.M$call$formula <- formula.M
      model.Y$call$formula <- formula.Y
      model.c$call$formula <- formula.c
    
      med.out <- mediation::mediate(
        model.M,
        model.Y,
        treat = IV,
        mediator = Mz,
        sims = n_sims,
        boot = TRUE
      )
    
      sum_M <- summary(model.M)
      sum_Y <- summary(model.Y)
      sum_c <- summary(model.c)
    
      coef_M <- sum_M$coefficients
      coef_Y <- sum_Y$coefficients
      coef_c <- sum_c$coefficients
    
      ci_M <- confint(model.M, level = ci_level)
      ci_Y <- confint(model.Y, level = ci_level)
      ci_c <- confint(model.c, level = ci_level)
    
      results_list[[paste(sample_name, Y, M, sep = "_")]] <- data.frame(
      sample = sample_name,
      model_type = "mediation",
      outcome = Y,
      mediator = M,
      IV = IV,
      n = nrow(model_data),
      
      a_path_est = coef_M[IV, "Estimate"],
      a_path_se = coef_M[IV, "Std. Error"],
      a_path_t = coef_M[IV, "t value"],
      a_path_p = coef_M[IV, "Pr(>|t|)"],
      a_path_df = sum_M$df[2],
      a_path_r2 = sum_M$r.squared,
      a_path_adj_r2 = sum_M$adj.r.squared,
      a_path_ci_low = ci_M[IV, 1],
      a_path_ci_high = ci_M[IV, 2],
      
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
      
      c_path_est = coef_c[IV, "Estimate"],
      c_path_se = coef_c[IV, "Std. Error"],
      c_path_t = coef_c[IV, "t value"],
      c_path_p = coef_c[IV, "Pr(>|t|)"],
      c_path_df = sum_c$df[2],
      c_path_r2 = sum_c$r.squared,
      c_path_adj_r2 = sum_c$adj.r.squared,
      c_path_ci_low = ci_c[IV, 1],
      c_path_ci_high = ci_c[IV, 2],
      
      c_prime_est = coef_Y[IV, "Estimate"],
      c_prime_se = coef_Y[IV, "Std. Error"],
      c_prime_t = coef_Y[IV, "t value"],
      c_prime_p = coef_Y[IV, "Pr(>|t|)"],
      c_prime_df = sum_Y$df[2],
      c_prime_r2 = sum_Y$r.squared,
      c_prime_adj_r2 = sum_Y$adj.r.squared,
      c_prime_ci_low = ci_Y[IV, 1],
      c_prime_ci_high = ci_Y[IV, 2],
      
      acme_est = med.out$d.avg,
      acme_ci_low = med.out$d.avg.ci[1],
      acme_ci_high = med.out$d.avg.ci[2],
      acme_p = med.out$d.avg.p,
      
      ade_est = med.out$z.avg,
      ade_ci_low = med.out$z.avg.ci[1],
      ade_ci_high = med.out$z.avg.ci[2],
      ade_p = med.out$z.avg.p,
      
      total_effect_est = med.out$tau.coef,
      total_effect_ci_low = med.out$tau.ci[1],
      total_effect_ci_high = med.out$tau.ci[2],
      total_effect_p = med.out$tau.p,
      
      prop_mediated_est = med.out$n.avg,
      prop_mediated_ci_low = med.out$n.avg.ci[1],
      prop_mediated_ci_high = med.out$n.avg.ci[2],
      prop_mediated_p = med.out$n.avg.p
    )
    }
  }
  
  bind_rows(results_list)
}

run_age_moderation_set <- function(data, sample_name, control_vars) {
  results_list <- list()
  
  for (Y in outcomes) {
    for (M in mediators) {
      
      all_vars <- c(Y, M, IV, "age", control_vars)
      
      model_data <- data %>%
        dplyr::select(all_of(all_vars)) %>%
        na.omit()
      
      if (nrow(model_data) < 50) next
      Mz <- paste0(M, "_z")
      Yz <- paste0(Y, "_z")
      
      model_data[[Mz]] <- as.numeric(scale(model_data[[M]]))
      model_data[[Yz]] <- as.numeric(scale(model_data[[Y]]))
      model_data$age_z <- as.numeric(scale(model_data$age))
      
      controls_str <- paste(c("age_z", control_vars), collapse = " + ")
      interaction_term <- paste0(Mz, ":age_z")
      
      formula.M <- as.formula(paste(Mz, "~", IV, "+", controls_str))
      formula.Y <- as.formula(
        paste(Yz, "~", IV, "+", Mz, "* age_z +", paste(control_vars, collapse = " + "))
      )
      formula.c <- as.formula(paste(Yz, "~", IV, "+", controls_str))
      
      model.M <- lm(formula.M, data = model_data)
      model.Y <- lm(formula.Y, data = model_data)
      model.c <- lm(formula.c, data = model_data)
      
      model.M$call$formula <- formula.M
      model.Y$call$formula <- formula.Y
      model.c$call$formula <- formula.c
      
      med.out <- mediation::mediate(
        model.M,
        model.Y,
        treat = IV,
        mediator = Mz,
        sims = n_sims,
        boot = TRUE
      )
      
      sum_M <- summary(model.M)
      sum_Y <- summary(model.Y)
      sum_c <- summary(model.c)
      
      coef_M <- sum_M$coefficients
      coef_Y <- sum_Y$coefficients
      coef_c <- sum_c$coefficients
      
      ci_M <- confint(model.M, level = ci_level)
      ci_Y <- confint(model.Y, level = ci_level)
      ci_c <- confint(model.c, level = ci_level)
      
      results_list[[paste(sample_name, Y, M, sep = "_")]] <- data.frame(
        sample = sample_name,
        model_type = "age_moderator",
        outcome = Y,
        mediator = M,
        IV = IV,
        n = nrow(model_data),
        
        a_path_est = coef_M[IV, "Estimate"],
        a_path_se = coef_M[IV, "Std. Error"],
        a_path_t = coef_M[IV, "t value"],
        a_path_p = coef_M[IV, "Pr(>|t|)"],
        a_path_df = sum_M$df[2],
        a_path_r2 = sum_M$r.squared,
        a_path_adj_r2 = sum_M$adj.r.squared,
        a_path_ci_low = ci_M[IV, 1],
        a_path_ci_high = ci_M[IV, 2],
        
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
        
        c_path_est = coef_c[IV, "Estimate"],
        c_path_se = coef_c[IV, "Std. Error"],
        c_path_t = coef_c[IV, "t value"],
        c_path_p = coef_c[IV, "Pr(>|t|)"],
        c_path_df = sum_c$df[2],
        c_path_r2 = sum_c$r.squared,
        c_path_adj_r2 = sum_c$adj.r.squared,
        c_path_ci_low = ci_c[IV, 1],
        c_path_ci_high = ci_c[IV, 2],
        
        c_prime_est = coef_Y[IV, "Estimate"],
        c_prime_se = coef_Y[IV, "Std. Error"],
        c_prime_t = coef_Y[IV, "t value"],
        c_prime_p = coef_Y[IV, "Pr(>|t|)"],
        c_prime_df = sum_Y$df[2],
        c_prime_r2 = sum_Y$r.squared,
        c_prime_adj_r2 = sum_Y$adj.r.squared,
        c_prime_ci_low = ci_Y[IV, 1],
        c_prime_ci_high = ci_Y[IV, 2],
        
        acme_est = med.out$d.avg,
        acme_ci_low = med.out$d.avg.ci[1],
        acme_ci_high = med.out$d.avg.ci[2],
        acme_p = med.out$d.avg.p,
        
        ade_est = med.out$z.avg,
        ade_ci_low = med.out$z.avg.ci[1],
        ade_ci_high = med.out$z.avg.ci[2],
        ade_p = med.out$z.avg.p,
        
        total_effect_est = med.out$tau.coef,
        total_effect_ci_low = med.out$tau.ci[1],
        total_effect_ci_high = med.out$tau.ci[2],
        total_effect_p = med.out$tau.p,
        
        prop_mediated_est = med.out$n.avg,
        prop_mediated_ci_low = med.out$n.avg.ci[1],
        prop_mediated_ci_high = med.out$n.avg.ci[2],
        prop_mediated_p = med.out$n.avg.p
      )
    }
  }
  
  bind_rows(results_list)
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
  anes_w3_mediation_results <- anes_w3_mediation_results %>%
    bind_rows(
    run_age_moderation_set(
      data = anes_w3,
      sample_name = "age_moderator",
      control_vars = moderation_controls
    )
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
