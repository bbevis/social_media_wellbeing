#############################################
# Reverse Causality Analysis
# Do wellbeing states predict later social media use?
# Time-of-day block structure (morning/afternoon/evening/night)
#############################################

library(dplyr)
library(lme4)
library(tidyr)
library(gt)
library(stringr)

# Load data
momentary_df <- read.csv("momentary.csv")



block_df <- momentary_df %>%
  group_by(id, calendar_date, time_of_day) %>%
  summarise(
    sm_minutes = mean(sm_minutes, na.rm = TRUE),
    awb_momentary = mean(awb_momentary, na.rm = TRUE),
    angry_rev = mean(angry_rev, na.rm = TRUE),
    worried_rev = mean(worried_rev, na.rm = TRUE),
    happy = mean(happy, na.rm = TRUE),
    sad_rev = mean(sad_rev, na.rm = TRUE),
    stressed_rev = mean(stressed_rev, na.rm = TRUE),
    lonely_rev = mean(lonely_rev, na.rm = TRUE),
    accepted = mean(accepted, na.rm = TRUE),
    term = first(term),
    week = first(week),
    surveys_per_id = first(surveys_per_id),
    post_election = first(post_election)
  ) %>%
  ungroup()



block_df <- block_df %>%
  arrange(id, calendar_date, time_of_day) %>%
  group_by(id) %>%
  mutate(block_idx = row_number()) %>%
  ungroup()


block_df <- block_df %>%
  group_by(id) %>%
  mutate(sm_wp = sm_minutes - mean(sm_minutes, na.rm = TRUE)) %>%
  ungroup()


dv_vars <- c(
  "awb_momentary", "angry_rev", "worried_rev", "happy",
  "sad_rev", "stressed_rev", "lonely_rev", "accepted"
)

block_df <- block_df %>%
  group_by(id) %>%
  mutate(
    across(all_of(dv_vars), ~ lag(.x, 1), .names = "{.col}_lag1"),
    across(all_of(dv_vars), ~ lag(.x, 2), .names = "{.col}_lag2"),
    across(all_of(dv_vars), ~ lag(.x, 3), .names = "{.col}_lag3")
  ) %>%
  ungroup()


models <- list()

for (dv in dv_vars) {
  
  # Lag 1
  f1 <- as.formula(
    paste0("sm_wp ~ ", dv, "_lag1 + term + week + surveys_per_id + post_election + (1 | id)")
  )
  models[[paste0(dv, "_lag1")]] <- lmer(f1, data = block_df)
  
  # Lag 2
  f2 <- as.formula(
    paste0("sm_wp ~ ", dv, "_lag2 + term + week + surveys_per_id + post_election + (1 | id)")
  )
  models[[paste0(dv, "_lag2")]] <- lmer(f2, data = block_df)
  
  # Lag 3
  f3 <- as.formula(
    paste0("sm_wp ~ ", dv, "_lag3 + term + week + surveys_per_id + post_election + (1 | id)")
  )
  models[[paste0(dv, "_lag3")]] <- lmer(f3, data = block_df)
}


results_list <- lapply(names(models), function(model_name) {
  
  m <- models[[model_name]]
  s <- summary(m)
  coefs <- s$coefficients
  
  predictor <- rownames(coefs)[2]  # wellbeing lag term
  
  pval <- 2 * (1 - pnorm(abs(coefs[predictor, "t value"])))
  
  ci <- confint(m, method = "Wald")
  ci_row <- ci[predictor, ]
  
  data.frame(
    model = model_name,
    predictor = predictor,
    estimate = coefs[predictor, "Estimate"],
    std_error = coefs[predictor, "Std. Error"],
    t_value = coefs[predictor, "t value"],
    p_value = pval,
    ci_lower = ci_row[1],
    ci_upper = ci_row[2],
    n_obs = nobs(m),
    stringsAsFactors = FALSE
  )
})

results_df <- do.call(rbind, results_list)

write.csv(results_df, "reverse_causality_results.csv", row.names = FALSE)

#####################################################
# gt table

results_df <- read.csv("reverse_causality_results.csv")

results <- results_df %>%
  mutate(
    dv = str_replace(model, "_lag[123]", ""),
    lag = str_extract(model, "lag[123]"),
    lag_num = str_extract(lag, "[123]")
  )

results <- results %>%
  mutate(
    EstCI = sprintf(
      "%.3f [%.3f, %.3f]",
      estimate, ci_lower, ci_upper
    ),
    # Remove leading zeros: 0.246 → .246, -0.345 → -.345
    EstCI = gsub("0\\.", ".", EstCI),
    EstCI = gsub("-0\\.", "-.", EstCI),
    
    # p-values: remove leading zero except < .001
    p_fmt = case_when(
      p_value < .001 ~ "< .001",
      TRUE ~ sub("^0\\.", ".", sprintf("%.3f", p_value))
    )
  )


wide <- results %>%
  select(dv, lag_num, EstCI, p_fmt) %>%
  pivot_wider(
    names_from = lag_num,
    values_from = c(EstCI, p_fmt),
    names_glue = "{.value}_Lag{lag_num}"
  )

dv_labels <- c(
  "awb_momentary" = "Affective Wellbeing",
  "accepted"      = "Accepted",
  "happy"         = "Happy",
  "angry_rev"     = "Angry (r)",
  "lonely_rev"    = "Lonely (r)",
  "sad_rev"       = "Sad (r)",
  "stressed_rev"  = "Stressed (r)",
  "worried_rev"   = "Worried (r)"
)

wide <- wide %>%
  mutate(dv = dv_labels[dv])

outcome_order <- c(
  "Affective Wellbeing",
  "Accepted",
  "Happy",
  "Angry (r)",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

wide <- wide %>%
  slice(match(outcome_order, dv))

gt_table <- wide %>%
  gt(rowname_col = "dv") %>%
  
  tab_spanner(label = md("**Lag 1**"), columns = c(EstCI_Lag1, p_fmt_Lag1)) %>%
  tab_spanner(label = md("**Lag 2**"), columns = c(EstCI_Lag2, p_fmt_Lag2)) %>%
  tab_spanner(label = md("**Lag 3**"), columns = c(EstCI_Lag3, p_fmt_Lag3)) %>%
  
  cols_label(
    EstCI_Lag1 = "Estimate [95% CI]",
    p_fmt_Lag1 = md("*p*"),
    EstCI_Lag2 = "Estimate [95% CI]",
    p_fmt_Lag2 = md("*p*"),
    EstCI_Lag3 = "Estimate [95% CI]",
    p_fmt_Lag3 = md("*p*")
  ) %>%
  
  cols_align(align = "left", columns = dv) %>%
  cols_align(
    align = "center",
    columns = c(
      EstCI_Lag1, p_fmt_Lag1,
      EstCI_Lag2, p_fmt_Lag2,
      EstCI_Lag3, p_fmt_Lag3
    )
  ) %>%
  
  tab_options(
    table.font.names = "Helvetica, Arial, sans-serif",
    data_row.padding = px(4),
    table.width = pct(72),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    
    column_labels.border.top.style = "solid",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    stub.border.style = "none",
    table_body.hlines.style = "none",
    table_body.vlines.style = "none"
  )

gt_table

gtsave(gt_table, "Plots/tables_s9.png", vwidth = 1300)
