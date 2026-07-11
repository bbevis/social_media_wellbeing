#############################################
#
#       Monthly level analysis
#
#############################################

library(dplyr)
# library(stringr)
# library(lme4)
library(tidyr)
# install.packages("mediation")
library(mediation)
# install.packages("multilevelmediation")
# library(multilevelmediation)
library(boot)
# library(purrr)
# library(psych)
# library(pwr)

##############################################
# Analysis parameters

analysis_iv <- getOption("coco.analysis_iv", getOption("appusage.analysis_iv", if (exists("analysis_iv", inherits = FALSE)) analysis_iv else "political_orientation")) # political_orientation, authoritarianism, party_affiliation_t1, presidential_support
include_controls <- getOption("coco.include_controls", getOption("appusage.include_controls", if (exists("include_controls", inherits = FALSE)) include_controls else FALSE))
include_Ypre <- getOption("coco.include_Ypre", getOption("appusage.include_Ypre", if (exists("include_Ypre", inherits = FALSE)) include_Ypre else FALSE)) # preregistered monthly model controls for baseline wellbeing
control_vars <- getOption("coco.control_vars", getOption("appusage.control_vars", if (exists("control_vars", inherits = FALSE)) control_vars else c("gender", "ethnicity", "ses")))
n_sims <- getOption("coco.n_sims", getOption("appusage.n_sims", if (exists("n_sims", inherits = FALSE)) n_sims else 1000))
ci_level <- getOption("coco.ci_level", getOption("appusage.ci_level", if (exists("ci_level", inherits = FALSE)) ci_level else 0.95))

# Output filename guide:
# Primary IV = political_orientation
#   include_controls = FALSE, include_Ypre = FALSE:
#     "monthly_results_nocontrols.csv"
#   include_controls = TRUE, include_Ypre = FALSE:
#     "monthly_results_withcontrols.csv"
#
# Alternative IVs, no controls:
#   analysis_iv = "authoritarianism":
#     "monthly_results_nocontrols_authoritarianism.csv"
#   analysis_iv = "party_affiliation_t1":
#     "monthly_results_nocontrols_party_affiliation.csv"
#   analysis_iv = "presidential_support":
#     "monthly_results_nocontrols_presidential_support.csv"
#
# Test runs:
#   Use a temporary filename, e.g. "monthly_results_withcontrols_test20.csv"

save_file_name <- getOption("coco.save_file_name", getOption("appusage.save_file_name", if (exists("save_file_name", inherits = FALSE)) save_file_name else "monthly_results_nocontrols.csv"))

##############################################

# Load data
monthly_df = read.csv("monthly.csv")


# Define variables
outcomes <- c("swls_t2", "uls_t2", "awb_t2")
outcomes_pre <- c("swls_t1", "uls_t1", "awb_t1")
mediators <- c("facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat",
               "information", "social", "productivity", "education", "entertainment", 
               "creativity", "games", "shopping", "utilities", "other", "health", "total_platform", "total_activity")

IV <- analysis_iv


##############################################


results_list <- list()

for (i in seq_along(outcomes)) {
  Y <- outcomes[i]
  Y_pre <- outcomes_pre[i]
  
  for (M in mediators) {
    exclusion_col <- paste0(M, "_exclude")
    
    # Skip only if the mediator truly doesn't exist (not totals)
    if (!M %in% names(monthly_df)) next
    
    # Apply exclusion only if the column exists
    if (exclusion_col %in% names(monthly_df)) {
      model_data <- monthly_df %>% filter(.data[[exclusion_col]] == 0)
    } else {
      model_data <- monthly_df
    }
    
    # Build list of control variables
    control_vec <- character(0)
    if (include_Ypre) control_vec <- c(control_vec, Y_pre)
    if (include_controls) control_vec <- c(control_vec, control_vars)
    
    all_vars <- unique(c(Y, M, IV, "term", control_vec))
    
    model_data <- monthly_df %>%
      filter(!!sym(exclusion_col) == 0) %>%
      dplyr::select(all_of(all_vars)) %>%
      na.omit()
    
    # if (nrow(model_data) < 50) next
    
    # scale mediator and outcome (global z)
    Mz <- paste0(M, "_z")
    Yz <- paste0(Y, "_z")
    model_data[[Mz]] <- as.numeric(scale(model_data[[M]]))
    model_data[[Yz]] <- as.numeric(scale(model_data[[Y]]))
    
    if (length(control_vec) > 0) {
      controls_str <- paste(control_vec, collapse = " + ")
      formula.M <- as.formula(paste(Mz, "~", IV, "+ term +", controls_str))
      formula.Y <- as.formula(paste(Yz, "~", IV, "+ term +", Mz, "+", controls_str))
      formula.c <- as.formula(paste(Yz, "~", IV, "+ term +", controls_str))
    } else {
      formula.M <- as.formula(paste(Mz, "~", IV, "+ term"))
      formula.Y <- as.formula(paste(Yz, "~", IV, "+ term +", Mz))
      formula.c <- as.formula(paste(Yz, "~", IV, "+ term"))
    }
    
    model.M <- lm(formula.M, data = model_data)
    model.Y <- lm(formula.Y, data = model_data)
    model.c <- lm(formula.c, data = model_data)
    
    med.out <- mediation::mediate(model.M, model.Y, treat = IV, mediator = Mz, sims = n_sims, boot = TRUE)
    
    sum_M <- summary(model.M)
    sum_Y <- summary(model.Y)
    sum_c <- summary(model.c)
    
    coef_M <- sum_M$coefficients
    coef_Y <- sum_Y$coefficients
    coef_c <- sum_c$coefficients
    
    ci_M <- confint(model.M, level = ci_level)
    ci_Y <- confint(model.Y, level = ci_level)
    ci_c <- confint(model.c, level = ci_level)
    
    row <- list(
      outcome = as.character(Y),
      mediator = as.character(M),
      n = nrow(model_data),
      
      # Path a
      path_a = coef_M[IV, "Estimate"],
      path_a_se = coef_M[IV, "Std. Error"],
      path_a_t = coef_M[IV, "t value"],
      path_a_p = coef_M[IV, "Pr(>|t|)"],
      path_a_df = sum_M$df[2],
      path_a_r2 = sum_M$r.squared,
      path_a_adj_r2 = sum_M$adj.r.squared,
      path_a_CI_lower = ci_M[IV, 1],
      path_a_CI_upper = ci_M[IV, 2],
      
      # Path b
      path_b = coef_Y[Mz, "Estimate"],
      path_b_se = coef_Y[Mz, "Std. Error"],
      path_b_t = coef_Y[Mz, "t value"],
      path_b_p = coef_Y[Mz, "Pr(>|t|)"],
      path_b_df = sum_Y$df[2],
      path_b_r2 = sum_Y$r.squared,
      path_b_adj_r2 = sum_Y$adj.r.squared,
      path_b_CI_lower = ci_Y[Mz, 1],
      path_b_CI_upper = ci_Y[Mz, 2],
      
      # Path c
      path_c = coef_c[IV, "Estimate"],
      path_c_se = coef_c[IV, "Std. Error"],
      path_c_t = coef_c[IV, "t value"],
      path_c_p = coef_c[IV, "Pr(>|t|)"],
      path_c_df = sum_c$df[2],
      path_c_r2 = sum_c$r.squared,
      path_c_adj_r2 = sum_c$adj.r.squared,
      path_c_CI_lower = ci_c[IV, 1],
      path_c_CI_upper = ci_c[IV, 2],
      
      # Path c'
      path_c_prime = coef_Y[IV, "Estimate"],
      path_c_prime_se = coef_Y[IV, "Std. Error"],
      path_c_prime_t = coef_Y[IV, "t value"],
      path_c_prime_p = coef_Y[IV, "Pr(>|t|)"],
      path_c_prime_df = sum_Y$df[2],
      path_c_prime_r2 = sum_Y$r.squared,
      path_c_prime_adj_r2 = sum_Y$adj.r.squared,
      path_c_prime_CI_lower = ci_Y[IV, 1],
      path_c_prime_CI_upper = ci_Y[IV, 2],
      
      # Mediation
      ACME = med.out$d.avg,
      ACME_CI_lower = med.out$d.avg.ci[1],
      ACME_CI_upper = med.out$d.avg.ci[2],
      ACME_p = med.out$d.avg.p,
      
      ADE = med.out$z.avg,
      ADE_CI_lower = med.out$z.avg.ci[1],
      ADE_CI_upper = med.out$z.avg.ci[2],
      ADE_p = med.out$z.avg.p,
      
      total_effect = med.out$tau.coef,
      total_effect_CI_lower = med.out$tau.ci[1],
      total_effect_CI_upper = med.out$tau.ci[2],
      total_effect_p = med.out$tau.p,
      
      prop_mediated = med.out$n.avg,
      prop_mediated_CI_lower = med.out$n.avg.ci[1],
      prop_mediated_CI_upper = med.out$n.avg.ci[2],
      prop_mediated_p = med.out$n.avg.p
    )
    
    results_list[[length(results_list) + 1]] <- row
  }
}

# Final output
monthly_res <- do.call(rbind, lapply(results_list, as.data.frame))

write.csv(monthly_res, save_file_name, row.names = FALSE)
