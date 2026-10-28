#############################################
# Lagged analysis using averaged time-of-day data
#############################################

library(dplyr)
library(lme4)
library(stringr)
library(gt)
source("supplementary_table_style.R")
library(tidyr)

# Load data
momentary_df <- read.csv("momentary.csv")


# Order time-of-day properly
momentary_df$time_of_day <- factor(
  momentary_df$time_of_day,
  levels = c("morning", "afternoon", "evening", "night"),
  ordered = TRUE
)

# Well-being DVs
dv_vars <- c(
  "awb_momentary", "angry_rev", "worried_rev", "happy",
  "sad_rev", "stressed_rev", "lonely_rev", "accepted"
)


# Aggregate to one row per person × day × time_of_day
agg_df <- momentary_df %>%
  group_by(id, calendar_date, time_of_day) %>%
  summarise(
    sm_mean = mean(sm_minutes, na.rm = TRUE),
    across(all_of(dv_vars), ~ mean(.x, na.rm = TRUE)),
    term = first(term),
    week = first(week),
    surveys_per_id = first(surveys_per_id),
    post_election = first(post_election),
    .groups = "drop"
  )


# Create lag variables (lag 1, lag 2, lag 3)
agg_df <- agg_df %>%
  arrange(id, calendar_date, time_of_day) %>%
  group_by(id, calendar_date) %>%
  mutate(
    across(all_of(dv_vars), ~ lead(.x, 1), .names = "{.col}_lag1"),
    across(all_of(dv_vars), ~ lead(.x, 2), .names = "{.col}_lag2"),
    across(all_of(dv_vars), ~ lead(.x, 3), .names = "{.col}_lag3"),
  ) %>%
  ungroup()

# Remove rows where next period(s) do not exist
agg_df <- agg_df %>% filter(!is.na(awb_momentary_lag1))

models <- list()
lags <- c("lag1", "lag2", "lag3")

for (dv in dv_vars) {
  for (lag in lags) {
    
    # DV at future time
    dv_future <- paste0(dv, "_", lag)
    
    formula_str <- paste0(
      dv_future, " ~ sm_mean + ", dv,
      " + term + week + surveys_per_id + post_election + (1 | id)"
    )
    
    model_name <- paste0(dv, "_", lag)
    models[[model_name]] <- lmer(as.formula(formula_str), data = agg_df)
  }
}

# Extract model results into CSV

results_list <- lapply(names(models), function(dv) {
  
  m <- models[[dv]]
  s <- summary(m)
  coefs <- s$coefficients

  pvals <- 2 * (1 - pnorm(abs(coefs[, "t value"])))
  
  ci <- confint(m, method = "Wald")
  
  ci_fixed <- ci[rownames(coefs), , drop = FALSE]
  
  data.frame(
    model     = dv,
    term      = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    t_value   = coefs[, "t value"],
    p_value   = pvals,
    ci_lower  = ci_fixed[, 1],
    ci_upper  = ci_fixed[, 2],
    n_obs     = nobs(m),
    stringsAsFactors = FALSE
  )
})

results_df <- do.call(rbind, results_list)


write.csv(results_df, "lagged_results.csv", row.names = FALSE)

##################################################
# gt table

results_df <- read.csv("lagged_results.csv")

dv_labels <- c(
  "awb_momentary" = "Affective Wellbeing",
  "angry_rev"     = "Angry (r)",
  "worried_rev"   = "Worried (r)",
  "happy"         = "Happy",
  "sad_rev"       = "Sad (r)",
  "stressed_rev"  = "Stressed (r)",
  "lonely_rev"    = "Lonely (r)",
  "accepted"      = "Accepted"
)


# Filter for sm_mean effects only and prepare formatting

sm <- results_df %>%
  filter(term == "sm_mean") %>%
  mutate(
    dv = sub("_lag[0-9]+$", "", model),   # clean DV name
    lag_num = as.numeric(sub(".*_lag", "", model))
  )



# Format estimate, CI, p-values, remove leading zeros

sm <- sm %>%
  mutate(
    est_fmt   = sprintf("%.4f", estimate),
    lower_fmt = sprintf("%.4f", ci_lower),
    upper_fmt = sprintf("%.4f", ci_upper),
    p_fmt     = sprintf("%.3f", p_value),
    
    # Remove leading zeros
    est_fmt   = str_replace(est_fmt,   "(?<=^|-)0\\.", "."),
    lower_fmt = str_replace(lower_fmt, "(?<=^|-)0\\.", "."),
    upper_fmt = str_replace(upper_fmt, "(?<=^|-)0\\.", "."),
    p_fmt     = str_replace(p_fmt,     "(?<=^|-)0\\.", "."),
    
    # final formatted columns
    EstCI = paste0(est_fmt, " [", lower_fmt, ", ", upper_fmt, "]"),
    p_val = paste0(p_fmt),
    sig = p_value <= 0.05
  )

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


# Convert to wide format with Estimate + p per lag

sm_wide <- sm %>%
  select(dv, lag_num, EstCI, p_val, sig) %>%
  pivot_wider(
    names_from = lag_num,
    values_from = c(EstCI, p_val, sig),
    names_glue = "{.value}_Lag{lag_num}"
  ) %>%
  mutate(dv = dv_labels[dv]) %>%   # Apply readable DV names
  select(
    dv,
    EstCI_Lag1, p_val_Lag1,
    EstCI_Lag2, p_val_Lag2,
    EstCI_Lag3, p_val_Lag3,
    sig_Lag1, sig_Lag2, sig_Lag3
  ) %>%
  # Apply ordering you requested
  mutate(dv = factor(dv, levels = outcome_order)) %>%
  arrange(dv)


gt_table <- sm_wide %>%
  gt(rowname_col = "dv") %>%
  cols_hide(c(sig_Lag1, sig_Lag2, sig_Lag3)) %>%
  cols_label(
    EstCI_Lag1 = "Estimate [95% CI]",
    p_val_Lag1 = md("*p*"),
    EstCI_Lag2 = "Estimate [95% CI]",
    p_val_Lag2 = md("*p*"),
    EstCI_Lag3 = "Estimate [95% CI]",
    p_val_Lag3 = md("*p*")
  )%>%
  tab_spanner("Lag 1", columns = c(EstCI_Lag1, p_val_Lag1)) %>%
  tab_spanner("Lag 2", columns = c(EstCI_Lag2, p_val_Lag2)) %>%
  tab_spanner("Lag 3", columns = c(EstCI_Lag3, p_val_Lag3)) %>%
  cols_align(
    align = "center",
    columns = c(
      EstCI_Lag1, p_val_Lag1,
      EstCI_Lag2, p_val_Lag2,
      EstCI_Lag3, p_val_Lag3
    )
  ) %>%
  cols_align(
    align = "left",
    columns = dv
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(EstCI_Lag1, p_val_Lag1), rows = sig_Lag1)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(EstCI_Lag2, p_val_Lag2), rows = sig_Lag2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(EstCI_Lag3, p_val_Lag3), rows = sig_Lag3)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = "Lag 1")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = "Lag 2")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = "Lag 3")
  ) %>%
  fmt_markdown(columns = c(p_val_Lag1, p_val_Lag2, p_val_Lag3)) %>%
  tab_options(
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
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    stub.border.style = "none",
    row.striping.background_color = NULL
  ) %>%
  apply_supplementary_table_style(table_width = pct(95))


gt_table

gtsave(gt_table, "Plots/table_lagged_social_media_to_wellbeing.png", vwidth = 2000)
