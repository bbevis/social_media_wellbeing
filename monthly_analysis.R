#############################################
#
#       Monthly level analysis
#
#############################################

library(dplyr)
library(stringr)
library(lme4)
library(tidyr)
# install.packages("mediation")
library(mediation)
# install.packages("multilevelmediation")
# library(multilevelmediation)
library(boot)
library(purrr)
library(psych)
library(pwr)


# Read data
appusage_1 <- read.csv("data/coco_ut1_appusage_survey.csv", header = TRUE, sep = ";")
# ema_1 <- read.csv("data/coco_ut1_ema_survey.csv", header = TRUE, sep = ";")
post_1 <- read.csv("data/coco_ut1_postsurvey.csv", header = TRUE, sep = ";")
pre_1 <- read.csv("data/coco_ut1_presurvey.csv", header = TRUE, sep = ";")
appusage_2 <- read.csv("data/coco_ut2_appusage_survey.csv", header = TRUE, sep = ";")
# ema_2 <- read.csv("data/coco_ut2_ema_survey.csv", header = TRUE, sep = ";")
post_2 <- read.csv("data/coco_ut2_postsurvey.csv", header = TRUE, sep = ";")
pre_2 <- read.csv("data/coco_ut2_presurvey.csv", header = TRUE, sep = ";")
demo_fall <- read.csv("data/demographics_fall_anonymized.csv", header = TRUE)
demo_spring <- read.csv("data/demographics_spring_anonymized.csv", header = TRUE)

#############################################
# Build monthly dataset

# DVs. Post-survey well-being (controlling for pre-survey well-being)
# measured using three validated scales:
# the Satisfaction with Life Scale, the UCLA Loneliness Scale, and the Affective Wellbeing Scale.

# IV. Political orientation.
# a one-time survey survey time (coded initially from 1 - extremely conservative to 7 - extremely liberal,
# then reverse-coded so that higher values indicate greater conservatism)



###############################
# Chronback alpha

# Combine post_1 and post_2 again, just in case
post <- bind_rows(post_1, post_2)

# Define item sets for each scale
swls_items <- c("swls_1_t2", "swls_2_t2", "swls_3_t2", "swls_4_t2", "swls_5_t2")
uls_items <- c("uls_1_t2", "uls_2_t2", "uls_3_t2", "uls_4_t2", "uls_5_t2",
               "uls_6_t2", "uls_7_t2", "uls_8_t2", "uls_9_t2")
awb_items <- c("awb_1_t2", "awb_2_t2", "awb_3_t2", "awb_4_t2", "awb_5_t2", "awb_6_t2")

# Reverse-code items in the ULS and AWB scales
post <- post %>%
  mutate(
    uls_1_t2 = 5 - uls_1_t2,
    uls_4_t2 = 5 - uls_4_t2,
    uls_8_t2 = 5 - uls_8_t2,
    awb_1_t2 = 5 - awb_1_t2,
    awb_2_t2 = 5 - awb_2_t2,
    awb_4_t2 = 5 - awb_4_t2,
  )

# Compute Cronbach's alpha for each scale
swls_alpha <- psych::alpha(post[ , swls_items], check.keys=TRUE)
uls_alpha  <- psych::alpha(post[ , uls_items], check.keys=TRUE)
awb_alpha  <- psych::alpha(post[ , awb_items], check.keys=TRUE)

# Display the reliability results
swls_alpha$total$raw_alpha  # SWLS
uls_alpha$total$raw_alpha   # ULS
awb_alpha$total$raw_alpha   # AWB

####################################################


post = bind_rows(post_1, post_2) %>%
  mutate(
    swls_t2 = rowMeans(cbind(swls_1_t2, swls_2_t2, swls_3_t2, swls_4_t2, swls_5_t2), na.rm = TRUE),
    uls_t2 = rowMeans(cbind(5 - uls_1_t2, uls_2_t2, uls_3_t2, 5 - uls_4_t2, uls_5_t2,
                            uls_6_t2, uls_7_t2, 5 - uls_8_t2, uls_9_t2), na.rm = TRUE),
    awb_t2 = rowMeans(cbind(5 - awb_1_t2, 5 - awb_2_t2, awb_3_t2, 5 - awb_4_t2, awb_5_t2, awb_6_t2), na.rm = TRUE),
    political_orientation = 8 - political_orientation_t2
  ) %>%
  dplyr::select(id, political_orientation, swls_t2, uls_t2, awb_t2)

pre = bind_rows(pre_1, pre_2) %>%
  mutate(
    swls_t1 = rowMeans(cbind(swls_1_t1, swls_2_t1, swls_3_t1, swls_4_t1, swls_5_t1), na.rm = TRUE),
    uls_t1 = rowMeans(cbind(5 - uls_1_t1, uls_2_t1, uls_3_t1, 5 - uls_4_t1, uls_5_t1,
                            uls_6_t1, uls_7_t1, 5 - uls_8_t1, uls_9_t1), na.rm = TRUE),
    awb_t1 = rowMeans(cbind(5 - awb_1_t1, 5 - awb_2_t1, awb_3_t1, 5 - awb_4_t1, awb_5_t1, awb_6_t1), na.rm = TRUE)
  ) %>%
  dplyr::select(id, swls_t1, uls_t1, awb_t1, ses = demog_ses_t1, party_affiliation_t1,
         support_election2020_t1, voting_2020_t1)


monthly_df = post %>%
  left_join(pre, by = 'id') %>%
  mutate(term = case_when(
    str_sub(id, 1, 2) == "FA" ~ 0, # create term indicator
    str_sub(id, 1, 2) == "SP" ~ 1,
    TRUE ~ NA_real_)
  )

################################
# Mediator. Social media usage measured from weekly app usage surveys,
# which will be aggregated to the monthly level by taking the mean of weekly values.

appusage = bind_rows(appusage_1, appusage_2)

platforms <- c("facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat") # Platforms
activities <- c("information", "social", "productivity", "education", "entertainment", 
                "creativity", "games", "shopping", "utilities", "other") # Activity categories

# compute monthly average
aggregate_mean_usage <- function(df, var_list) {
  
  # Start with id column only
  result <- data.frame(id = df$id)
  
  # Loop through each variable (platform or activity)
  for (var in var_list) {
    
    # Create lists of column names for hours and minutes across 4 weeks
    hours_cols <- paste0("hours_", var, "_w", 1:4)
    minutes_cols <- paste0("minutes_", var, "_w", 1:4)
    
    # Create a matrix to store weekly totals for each person
    weekly_totals <- matrix(NA, nrow = nrow(df), ncol = 4)
    
    # Loop through each week (w1 to w4)
    for (i in 1:4) {
      h <- df[[hours_cols[i]]]          # hours column for week i
      m <- df[[minutes_cols[i]]]        # minutes column for week i
      weekly_totals[, i] <- h + m / 60  # convert minutes to hours and sum
    }
    
    # Compute average weekly usage, ignoring missing values
    avg_hours <- apply(weekly_totals, 1, function(x) mean(x, na.rm = TRUE))
    
    # Add this new column to the result dataframe, using simple variable name
    result[[var]] <- avg_hours
  }
  
  return(result)
}

# detect if a person used the app for <2 weeks
exclusion_indicator <- function(df, var_list) {
  indicator_df <- data.frame(id = df$id)
  
  for (var in var_list) {
    hours_cols <- paste0("hours_", var, "_w", 1:4)
    minutes_cols <- paste0("minutes_", var, "_w", 1:4)
    
    # A week is missing if both hours and minutes are NA
    week_missing <- mapply(function(h_col, m_col) {
      is.na(df[[h_col]]) & is.na(df[[m_col]])
    }, hours_cols, minutes_cols)
    
    # Count how many weeks are missing
    missing_count <- rowSums(week_missing)
    
    # Exclude if 3 or more weeks missing
    indicator_df[[paste0(var, "_exclude")]] <- ifelse(missing_count >= 3, 1L, 0L)
  }
  
  return(indicator_df)
}

total_exclusion_indicator <- function(df, vars, label) {
  n <- nrow(df)
  missing_week_count <- integer(n)
  
  for (w in 1:4) {
    hours_cols <- paste0("hours_", vars, "_w", w)
    minutes_cols <- paste0("minutes_", vars, "_w", w)
    
    # A week counts as missing if ALL selected vars for that week are NA for BOTH hours and minutes
    week_missing <- mapply(function(h_col, m_col) {
      is.na(df[[h_col]]) & is.na(df[[m_col]])
    }, hours_cols, minutes_cols)
    
    # week_missing is a logical matrix: rows = people, cols = vars → for each row, count if ALL are NA
    all_na_week <- apply(week_missing, 1, all)  # TRUE if ALL platforms/activities are NA that week
    missing_week_count <- missing_week_count + ifelse(all_na_week, 1L, 0L)
  }
  
  df_total <- df %>%
    dplyr::select(id) %>%
    mutate(!!paste0("total_", label, "_exclude") := ifelse(missing_week_count >= 2, 1L, 0L))
  
  return(df_total)
}


# Apply the function separately for platforms and activities
platform_df <- aggregate_mean_usage(appusage, platforms)
activity_df <- aggregate_mean_usage(appusage, activities)


appusage_monthly <- platform_df %>%
  left_join(activity_df, by = "id") %>%
  left_join(exclusion_indicator(appusage, platforms), by = "id") %>%
  left_join(exclusion_indicator(appusage, activities), by = "id") %>%
  left_join(total_exclusion_indicator(appusage, platforms, "platform"), by = "id") %>%
  left_join(total_exclusion_indicator(appusage, activities, "activity"), by = "id") %>%
  rowwise() %>%
  mutate(
    total_platform = sum(c_across(all_of(platforms)), na.rm = TRUE),
    total_activity = sum(c_across(all_of(activities)), na.rm = TRUE)
  ) %>%
  ungroup()



monthly_df = monthly_df %>%
  left_join(appusage_monthly, by = "id")

################################
# Covariates

demo = bind_rows(demo_fall, demo_spring) %>%
  slice(-(1:2)) %>%
  dplyr::select(id = pID,
         age = 'Q24',
         gender = 'Q1',
         ethnicity  = 'Q7') %>%
  mutate(gender = ifelse(gender == 2, 1,
                         ifelse(gender == 1, 0, NA)),
         ethnicity = as.factor(gsub(" ", "", ethnicity)))

monthly_df = monthly_df %>%
  left_join(demo, by = "id") 

# replace missing values with NAs
monthly_df <- monthly_df %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

rm(appusage_1, appusage_2, demo_fall, demo_spring, pre_1, pre_2, post_1, post_2,
   activity_df, platform_df)

write.csv(monthly_df, "monthly.csv", row.names = FALSE)
##############################################
# exclusion criteria
# List of DVs and mediator exclusion vars
DVs <- c("swls_t2", "uls_t2", "awb_t2")
mediator_exclude_vars <- grep("_exclude$", names(monthly_df), value = TRUE)

# Create long-format summary table
exclusion_summary <- monthly_df %>%
  dplyr::select(id, all_of(DVs), all_of(mediator_exclude_vars)) %>%
  pivot_longer(cols = all_of(mediator_exclude_vars), names_to = "Mediator", values_to = "Excluded") %>%
  pivot_longer(cols = all_of(DVs), names_to = "DV", values_to = "DV_value") %>%
  # filter(!is.na(DV_value)) %>%  # Only consider valid DV observations
  group_by(DV, Mediator) %>%
  summarise(
    n_total = n(),
    n_excluded = sum(Excluded, na.rm = TRUE),
    percent_excluded = round(100 * n_excluded / n_total, 1),
    .groups = "drop"
  )

write.csv(exclusion_summary, 'exclusion_summary_monthly.csv', row.names = FALSE)

attrition_results <- monthly_df %>%
  dplyr::select(all_of(DVs), all_of(mediator_exclude_vars)) %>%
  pivot_longer(cols = all_of(mediator_exclude_vars), names_to = "Mediator", values_to = "Excluded") %>%
  pivot_longer(cols = all_of(DVs), names_to = "DV", values_to = "DV_value") %>%
  filter(!is.na(DV_value)) %>%
  group_by(DV, Mediator) %>%
  summarise(
    t_stat = t.test(DV_value ~ Excluded)$statistic,
    p_value = t.test(DV_value ~ Excluded)$p.value,
    .groups = "drop"
  )

write.csv(attrition_results, 'monthly_attrition.csv', row.names = FALSE)

##############################################
# Descriptive statistics

# Define variables
outcomes <- c("swls_t2", "uls_t2", "awb_t2")
outcomes_pre <- c("swls_t1", "uls_t1", "awb_t1")
mediators <- c("facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat",
               "information", "social", "productivity", "education", "entertainment", 
               "creativity", "games", "shopping", "utilities", "other", "total_platform", "total_activity")


# Define all variables used in the models
all_vars <- unique(c("political_orientation", mediators, outcomes, outcomes_pre))

# Filter relevant columns and compute summary statistics
summary_stats <- monthly_df %>%
  dplyr::select(all_of(all_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(Value) & !is.nan(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

write.csv(summary_stats, 'monthly_descstats.csv')


##############################################
# mediation analysis

# Parameters
include_Ypre <- FALSE
include_controls <- FALSE
control_vars <- c("term", "gender", "ethnicity", "ses")
n_sims <- 1000

results_list <- list()

for (i in seq_along(outcomes)) {
  Y <- outcomes[i]
  Y_pre <- outcomes_pre[i]
  
  for (M in mediators) {
    exclusion_col <- paste0(M, "_exclude")
    if (!exclusion_col %in% names(monthly_df)) next
    
    # Build list of control variables
    control_vec <- character(0)
    if (include_Ypre) control_vec <- c(control_vec, Y_pre)
    if (include_controls) control_vec <- c(control_vec, control_vars)
    
    all_vars <- unique(c(Y, M, "political_orientation", control_vec))
    
    model_data <- monthly_df %>%
      filter(!!sym(exclusion_col) == 0) %>%
      dplyr::select(all_of(all_vars)) %>%
      na.omit()
    
    if (nrow(model_data) < 50) next
    
    if (length(control_vec) > 0) {
      controls_str <- paste(control_vec, collapse = " + ")
      formula.M <- as.formula(paste(M, "~ political_orientation +", controls_str))
      formula.Y <- as.formula(paste(Y, "~ political_orientation +", M, "+", controls_str))
      formula.c <- as.formula(paste(Y, "~ political_orientation +", controls_str))
    } else {
      formula.M <- as.formula(paste(M, "~ political_orientation"))
      formula.Y <- as.formula(paste(Y, "~ political_orientation +", M))
      formula.c <- as.formula(paste(Y, "~ political_orientation"))
    }
    
    model.M <- lm(formula.M, data = model_data)
    model.Y <- lm(formula.Y, data = model_data)
    model.c <- lm(formula.c, data = model_data)
    
    med.out <- mediation::mediate(model.M, model.Y, treat = "political_orientation", mediator = M, sims = n_sims, boot = TRUE)
    
    sum_M <- summary(model.M)
    sum_Y <- summary(model.Y)
    sum_c <- summary(model.c)
    
    coef_M <- sum_M$coefficients
    coef_Y <- sum_Y$coefficients
    coef_c <- sum_c$coefficients
    
    row <- list(
      outcome = as.character(Y),
      mediator = as.character(M),
      n = nrow(model_data),
      
      # Path a
      path_a = round(coef_M["political_orientation", "Estimate"], 2),
      path_a_se = round(coef_M["political_orientation", "Std. Error"], 2),
      path_a_t = round(coef_M["political_orientation", "t value"], 2),
      path_a_p = round(coef_M["political_orientation", "Pr(>|t|)"], 3),
      path_a_df = round(sum_M$df[2], 2),
      path_a_r2 = round(sum_M$r.squared, 2),
      path_a_adj_r2 = round(sum_M$adj.r.squared, 2),
      
      # Path b
      path_b = round(coef_Y[M, "Estimate"], 2),
      path_b_se = round(coef_Y[M, "Std. Error"], 2),
      path_b_t = round(coef_Y[M, "t value"], 2),
      path_b_p = round(coef_Y[M, "Pr(>|t|)"], 3),
      path_b_df = round(sum_Y$df[2], 2),
      path_b_r2 = round(sum_Y$r.squared, 2),
      path_b_adj_r2 = round(sum_Y$adj.r.squared, 2),
      
      # Path c
      path_c = round(coef_c["political_orientation", "Estimate"], 2),
      path_c_se = round(coef_c["political_orientation", "Std. Error"], 2),
      path_c_t = round(coef_c["political_orientation", "t value"], 2),
      path_c_p = round(coef_c["political_orientation", "Pr(>|t|)"], 3),
      path_c_df = round(sum_c$df[2], 2),
      path_c_r2 = round(sum_c$r.squared, 2),
      path_c_adj_r2 = round(sum_c$adj.r.squared, 2),
      
      # Path c'
      path_c_prime = round(coef_Y["political_orientation", "Estimate"], 2),
      path_c_prime_se = round(coef_Y["political_orientation", "Std. Error"], 2),
      path_c_prime_t = round(coef_Y["political_orientation", "t value"], 2),
      path_c_prime_p = round(coef_Y["political_orientation", "Pr(>|t|)"], 3),
      path_c_prime_df = round(sum_Y$df[2], 2),
      path_c_prime_r2 = round(sum_Y$r.squared, 2),
      path_c_prime_adj_r2 = round(sum_Y$adj.r.squared, 2),
      
      # Mediation
      ACME = round(med.out$d.avg, 2),
      ACME_CI_lower = round(med.out$d.avg.ci[1], 2),
      ACME_CI_upper = round(med.out$d.avg.ci[2], 2),
      ACME_p = round(med.out$d.avg.p, 3),
      
      ADE = round(med.out$z.avg, 2),
      ADE_CI_lower = round(med.out$z.avg.ci[1], 2),
      ADE_CI_upper = round(med.out$z.avg.ci[2], 2),
      ADE_p = round(med.out$z.avg.p, 3),
      
      total_effect = round(med.out$tau.coef, 2),
      total_effect_CI_lower = round(med.out$tau.ci[1], 2),
      total_effect_CI_upper = round(med.out$tau.ci[2], 2),
      total_effect_p = round(med.out$tau.p, 3),
      
      prop_mediated = round(med.out$n.avg, 2),
      prop_mediated_CI_lower = round(med.out$n.avg.ci[1], 2),
      prop_mediated_CI_upper = round(med.out$n.avg.ci[2], 2),
      prop_mediated_p = round(med.out$n.avg.p, 3)
    )
    
    results_list[[length(results_list) + 1]] <- row
  }
}

# Final output
results_df <- do.call(rbind, lapply(results_list, as.data.frame))

write.csv(results_df, 'monthly_results_nocontrols.csv', row.names = FALSE)


##############################################
# power sensitivity analysis

compute_sensitivity <- function(n, k, power = 0.80, sig.level = 0.05) {
  v <- n - k - 1
  if (v <= 0) return(NA)
  f2 <- pwr.f2.test(u = k, v = v, sig.level = sig.level, power = power)$f2
  round(f2, 3)
}

# Define model complexity
num_controls <- 4  # term, gender, ethnicity, ses
include_Ypre <- TRUE
num_controls_plus_Ypre <- num_controls + ifelse(include_Ypre, 1, 0)

# Apply sensitivity analysis row-wise
results_df <- results_df %>%
  mutate(
    f2_path_a = mapply(compute_sensitivity, n, k = num_controls_plus_Ypre + 1),
    f2_path_b = mapply(compute_sensitivity, n, k = num_controls_plus_Ypre + 2),
    f2_path_c_prime = mapply(compute_sensitivity, n, k = num_controls_plus_Ypre + 1)
  )


