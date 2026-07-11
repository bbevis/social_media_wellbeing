#############################################
#
#       Momentary level analysis
#       (2-1-1 MLM mediation)
#
#############################################


# Packages
library(dplyr)
library(stringr)
library(tidyr)
library(lme4)
library(mediation)
library(purrr)
library(readr)
library(lubridate)
library(mlma)
library(multilevelmediation)
# install.packages("mlma")
library(future)
# install.packages("future")
library(future.apply)
# install.packages("future.apply")
library(progressr)
# install.packages("progressr")
# install.packages("furrr")
library(furrr)
# install.packages("tictoc")
library(tictoc)

library(lmerTest)    # For p-values in multilevel models
library(performance) # For R-squared in multilevel models
# install.packages("performance")
library(WebPower)
# install.packages("WebPower")

#############################################
# Analysis parameters

analysis_iv <- getOption("coco.analysis_iv", getOption("appusage.analysis_iv", if (exists("analysis_iv", inherits = FALSE)) analysis_iv else "political_orientation")) # political_orientation, authoritarianism, party_affiliation_t1, presidential_support
include_controls <- getOption("coco.include_controls", getOption("appusage.include_controls", if (exists("include_controls", inherits = FALSE)) include_controls else FALSE))
control_vars <- getOption("coco.control_vars", getOption("appusage.control_vars", if (exists("control_vars", inherits = FALSE)) control_vars else c("gender", "ethnicity", "ses")))
n_sims <- getOption("coco.n_sims", getOption("appusage.n_sims", if (exists("n_sims", inherits = FALSE)) n_sims else 1000))
ci_level <- getOption("coco.ci_level", getOption("appusage.ci_level", if (exists("ci_level", inherits = FALSE)) ci_level else 0.95))

# Output filename guide:
# Primary IV = political_orientation
#   include_controls = FALSE:
#     "momentary_results_nocontrols.csv"
#   include_controls = TRUE:
#     "momentary_results_withcontrols.csv"
#
# Alternative IVs, no controls:
#   analysis_iv = "authoritarianism":
#     "momentary_results_nocontrols_authoritarianism.csv"
#   analysis_iv = "party_affiliation_t1":
#     "momentary_results_nocontrols_party_affiliation.csv"
#   analysis_iv = "presidential_support":
#     "momentary_results_nocontrols_presidential_support.csv"
#

save_file_name <- getOption("coco.save_file_name", getOption("appusage.save_file_name", if (exists("save_file_name", inherits = FALSE)) save_file_name else "momentary_results_nocontrols.csv"))

#############################################

# load data
momentary_df = read.csv("momentary.csv")


# Global variables

dv_vars <- c("awb_momentary", "angry_rev", "worried_rev", "happy", "sad_rev", "stressed_rev", "lonely_rev", "accepted")
mediators <- c(
  "sm_any",       # 0/1 indicator of any social media use in past hour
  "sm_minutes"    # estimated minutes of social media use in past hour
)

IV <- analysis_iv
##############################################
# Momentary-level mediation analysis function
##############################################

run_one_mediation <- function(M, Y, controls_to_include = NULL) {
  
  # Prepare data. The preregistration specifies no momentary-level exclusions.
  all_vars <- unique(c(Y, M, IV, "id", "term", "week", "surveys_per_id", "post_election", controls_to_include))
  
  model_data <- momentary_df %>%
    dplyr::select(all_of(all_vars)) %>%
    drop_na()
  
  if (nrow(model_data) < 50) return(NULL)
  model_data$level <- as.numeric(factor(model_data$id))
  
  # SCALE mediator and outcome (z-scores)
  # Keep original columns; add standardized versions
  Mz <- paste0(M,  "_z")
  Yz <- paste0(Y,  "_z")
  model_data[[Mz]] <- as.numeric(scale(model_data[[M]]))
  model_data[[Yz]] <- as.numeric(scale(model_data[[Y]]))
  
  # Build model formulas
  controls_str <- if (!is.null(controls_to_include) && length(controls_to_include) > 0) paste(controls_to_include, collapse = " + ") else ""
  
  # Path a (X → M): formula_M_str defines the model where the mediator (M) is predicted by political orientation.
  formula_M_str <- paste(Mz, "~", IV, "+ term + week + surveys_per_id + post_election +", if(controls_str != "") paste("+", controls_str), "+ (1|level)")
  
  # Path b and c' (X + M → Y): formula_Y_str defines the model where the outcome (Y)
  # is predicted by both political orientation (path c') and the mediator (path b).
  formula_Y_str <- paste(Yz, "~", IV, "+ term + week + surveys_per_id + post_election +", Mz, if(controls_str != "") paste("+", controls_str), "+ (1|level)")
  
  # Path c (X → Y): formula_c_str defines the model for the total effect, where the outcome (Y)
  # is predicted by political orientation alone.
  formula_c_str <- paste(Yz, "~", IV, "+ term + week + surveys_per_id + post_election +", if(controls_str != "") paste("+", controls_str), "+ (1|level)")
  
  # Run bootstrap mediation model
  covariates_df <- if (controls_str != "") {
    model_data[, controls_to_include, drop = FALSE]
  } else {
    NULL
  }
  
  boot_out <- tryCatch(
    boot.mlma(y = model_data[[Yz]],
              x = model_data[[IV]],
              m = model_data[, Mz, drop = FALSE],
              covariates = covariates_df,
              levelx = 2, levely = 1, level = model_data$level,
              random = "(1|level)", boot = n_sims,
              echo = FALSE, plot.it = FALSE),
    error = function(e) NULL
  )
  if (is.null(boot_out)) return(NULL)
  
  # Manually fit path models
  model.M <- lmerTest::lmer(as.formula(formula_M_str), data = model_data)
  model.Y <- lmerTest::lmer(as.formula(formula_Y_str), data = model_data)
  model.c <- lmerTest::lmer(as.formula(formula_c_str), data = model_data)
  
  # Extract statistics
  sum_M <- summary(model.M)
  sum_Y <- summary(model.Y)
  sum_c <- summary(model.c)
  
  r2_M <- performance::r2(model.M)
  r2_Y <- performance::r2(model.Y)
  r2_c <- performance::r2(model.c)
  
  coef_M <- sum_M$coefficients
  coef_Y <- sum_Y$coefficients
  coef_c <- sum_c$coefficients
  
  # t-based Wald confidence intervals for paths a, b, c', c
  alpha <- 1 - ci_level
  
  # compute CI from a coef table row
  ci_from_row <- function(tab, rowname) {
    est <- tab[rowname, "Estimate"]
    se  <- tab[rowname, "Std. Error"]
    df  <- tab[rowname, "df"]
    tcrit <- stats::qt(1 - alpha/2, df = df)
    c(lower = est - tcrit * se, upper = est + tcrit * se)
  }
  
  ci_a <- ci_from_row(coef_M, IV)
  ci_b <- ci_from_row(coef_Y, Mz)
  ci_cprime <- ci_from_row(coef_Y, IV)
  ci_c <- ci_from_row(coef_c, IV)
  
  boot_summary <- summary(boot_out)
  effect_table <- boot_summary$effect2
  
  ACME <- effect_table["est", Mz]
  total_effect <- effect_table["est", "te"]
  
  tibble(
    outcome = Y, mediator = M,
    n = nrow(model_data),
    n_subjects = length(unique(model_data$id)),
    
    # Path a Stats
    path_a = coef_M[IV, "Estimate"],
    path_a_se = coef_M[IV, "Std. Error"],
    path_a_t = coef_M[IV, "t value"],
    path_a_p = coef_M[IV, "Pr(>|t|)"],
    path_a_df = coef_M[IV, "df"],
    path_a_r2_marginal = r2_M$R2_marginal,
    path_a_r2_conditional = r2_M$R2_conditional,
    path_a_CI_lower = ci_a["lower"],
    path_a_CI_upper = ci_a["upper"],
    
    # Path b Stats
    path_b = coef_Y[Mz, "Estimate"],
    path_b_se = coef_Y[Mz, "Std. Error"],
    path_b_t = coef_Y[Mz, "t value"],
    path_b_p = coef_Y[Mz, "Pr(>|t|)"],
    path_b_df = coef_Y[Mz, "df"],
    path_b_CI_lower = ci_b["lower"],
    path_b_CI_upper = ci_b["upper"],
    
    # Path c' Stats
    path_c_prime = coef_Y[IV, "Estimate"],
    path_c_prime_se = coef_Y[IV, "Std. Error"],
    path_c_prime_t = coef_Y[IV, "t value"],
    path_c_prime_p = coef_Y[IV, "Pr(>|t|)"],
    path_c_prime_df = coef_Y[IV, "df"],
    path_c_prime_CI_lower = ci_cprime["lower"],
    path_c_prime_CI_upper = ci_cprime["upper"],
    
    # R-squared for Model Y
    model_Y_r2_marginal = r2_Y$R2_marginal,
    model_Y_r2_conditional = r2_Y$R2_conditional,
    
    # Path c Stats
    path_c = coef_c[IV, "Estimate"],
    path_c_se = coef_c[IV, "Std. Error"],
    path_c_t = coef_c[IV, "t value"],
    path_c_p = coef_c[IV, "Pr(>|t|)"],
    path_c_df = coef_c[IV, "df"],
    path_c_r2_marginal = r2_c$R2_marginal,
    path_c_r2_conditional = r2_c$R2_conditional,
    path_c_CI_lower = ci_c["lower"],
    path_c_CI_upper = ci_c["upper"],
    
    # Bootstrapped Mediation Effects
    ACME = ACME,
    ACME_CI_lower = effect_table["lwbd.quan", Mz],
    ACME_CI_upper = effect_table["upbd.quan", Mz],
    
    ADE = effect_table["est", "de"],
    ADE_CI_lower = effect_table["lwbd.quan", "de"],
    ADE_CI_upper = effect_table["upbd.quan", "de"],
    
    total_effect = total_effect,
    total_effect_CI_lower = effect_table["lwbd.quan", "te"],
    total_effect_CI_upper = effect_table["upbd.quan", "te"],
    
    prop_mediated = if (total_effect != 0) { ACME / total_effect } else { NA_real_ }
  )
}

# --- EXECUTION ---

# Create the vector of controls to use
controls_to_include <- if (include_controls) control_vars else NULL

# Create all mediator–outcome combinations
med_dv <- expand.grid(M = mediators, Y = dv_vars, stringsAsFactors = FALSE)

# Set up and run the parallel processing plan
plan(multisession, workers = max(1, parallel::detectCores() - 4))

tic() # Start a stopwatch

results_list <- future_pmap(
  .l = list(M = med_dv$M, Y = med_dv$Y),
  .f = run_one_mediation,
  
  # Pass the controls vector as an additional, constant argument
  controls_to_include = controls_to_include,
  
  .options = furrr_options(
    # Export necessary data and variables
    globals = c("momentary_df", "platforms", "activities", "n_sims", "ci_level", "IV"),
    
    # Export all required packages to the workers
    packages = c("dplyr", "tidyr", "tibble", "mlma", "lmerTest", "performance"),
    
    seed = TRUE
  ),
  .progress = TRUE
)

toc() # Stop the stopwatch and print elapsed time

# Combine and view results
momentary_res <- bind_rows(results_list)
print(momentary_res)

write.csv(momentary_res, save_file_name, row.names = FALSE)
