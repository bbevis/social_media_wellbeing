#############################################
#
# H1 results table - Political orientation on Wellbeing
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
library(gt)
source("supplementary_table_style.R")
library(tidyr)
library(webshot2)

#############################################
# Load data

monthly_wc = read.csv('monthly_results_withcontrols.csv')
weekly_wc = read.csv('weekly_results_withcontrols.csv')
momentary_wc = read.csv('momentary_results_withcontrols.csv')


#############################################
# H1 table: combine and summarise

h1_df = monthly_wc %>%
  filter(mediator%in%c('total_activity')) %>%
  dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
  mutate(level = 'Monthly') %>%
  bind_rows(
    weekly_wc %>%
      filter(mediator%in%c('total_activity')) %>%
      dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
      mutate(level = 'Weekly')) %>%
  bind_rows(
    momentary_wc %>%
      dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
      mutate(level = 'Momentary')) %>%
  group_by(outcome,level) %>%
  summarise(
    m   = mean(path_c),
    lwr = mean(path_c_CI_lower, na.rm = TRUE),
    upr = mean(path_c_CI_upper, na.rm = TRUE),
    p   = mean(path_c_p),
    .groups = "drop"
  ) %>%
  mutate(outcome = dplyr::recode(outcome,
                          "awb_momentary" = "Affective Wellbeing",
                          "awb_week"      = "Affective Wellbeing",
                          "awb_t2"        = "Affective Wellbeing",
                          "swls_t2"       = "Life Satisfaction",
                          "uls_t2"        = "Lonely (r)",
                          "angry_rev"         = "Angry (r)",
                          "worried_rev"       = "Worried (r)",
                          "happy"         = "Happy",
                          "sad_rev"           = "Sad (r)",
                          "stressed_rev"      = "Stressed (r)",
                          "lonely_rev"        = "Lonely (r)",
                          "accepted"      = "Accepted"
  )) %>%
  # Make "Affective Wellbeing" come first, followed by alphabetical ordering
  mutate(
    outcome = factor(
      outcome,
      levels = c("Affective Wellbeing", sort(setdiff(unique(outcome), "Affective Wellbeing")))
    )
  )
  
#############################################
# Prepare data for gt table

h1_for_gt <- h1_df %>%
  mutate(
    level = factor(level, levels = c("Momentary", "Weekly", "Monthly")),
    est_ci = sprintf("%.3f [%.3f, %.3f]", m, lwr, upr), # Format estimate and CI
    p_fmt  = sprintf("%.3f", p), # Format p value
    sig = p <= 0.05,
    est_ci = gsub("0\\.", ".", est_ci),
    p_fmt  = gsub("0\\.", ".", p_fmt)
  ) %>%
  dplyr::select(outcome, level, est_ci, p_fmt, sig) %>%
  arrange(outcome, level)

#############################################
# Create gt table

h1_gt <- h1_for_gt %>%
  gt(rowname_col = "outcome", groupname_col = "level") %>%
  cols_hide(sig) %>%
  cols_label(
    est_ci = "Estimate [95% CI]",
    p_fmt  = md("*p*")
  ) %>%
  cols_align(
    align = "center"
  )%>%
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_row_groups(groups = c("Momentary", "Weekly", "Monthly"))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(est_ci, p_fmt), rows = sig)
  ) %>%
  tab_options(
    table.border.top.style    = "none",
    table.border.bottom.style = "none",
    data_row.padding          = px(4),
    table.width               = pct(60),
    
    heading.border.bottom.style = "solid",      # line below title/subtitle (if any)
    column_labels.border.top.style = "solid",   # line between spanners and column labels
    column_labels.border.bottom.style = "solid",# line under column labels
    
    row.striping.include_table_body = FALSE,    # no striping
    row.striping.include_stub = FALSE,
    
    table_body.hlines.style = "none",           # no horizontal lines between rows
    table_body.vlines.style = "none",           # no vertical lines between columns
    column_labels.vlines.style = "none",         # no vertical lines in column headers
    stub.border.style = "none",
    
    row_group.border.top.style        = "none",
    row_group.border.bottom.style     = "none"
  ) %>%
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_stub()
  ) %>%
  apply_supplementary_table_style()

h1_gt

gtsave(h1_gt, "Plots/tables_s1.png", vwidth = SUPPLEMENTARY_TABLE_VWIDTH, vheight = 0)
