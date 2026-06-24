#############################################
#
# H1 results table - Political orientation on Wellbeing
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
library(gt)
library(tidyr)
library(webshot2)

#############################################
# Load data

monthly_nc = read.csv('monthly_results_nocontrols.csv')
weekly_nc = read.csv('weekly_results_nocontrols.csv')
momentary_nc = read.csv('momentary_results_nocontrols.csv')

monthly_wc = read.csv('monthly_results_withcontrols.csv')
weekly_wc = read.csv('weekly_results_withcontrols.csv')
momentary_wc = read.csv('momentary_results_withcontrols.csv')


#############################################
# H1 table: combine and summarise

h1_df = monthly_nc %>%
  filter(mediator%in%c('total_activity')) %>%
  dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
  mutate(level = 'Monthly',
         controls = FALSE) %>%
  bind_rows(
    weekly_nc %>%
      filter(mediator%in%c('total_activity')) %>%
      dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
      mutate(level = 'Weekly',
             controls = FALSE)) %>%
  bind_rows(
    momentary_nc %>%
      dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
      mutate(level = 'Momentary',
             controls = FALSE)) %>%
  bind_rows(
    monthly_wc %>%
      filter(mediator%in%c('total_activity')) %>%
      dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
      mutate(level = 'Monthly',
             controls = TRUE)) %>%
      bind_rows(
        weekly_wc %>%
          filter(mediator%in%c('total_activity')) %>%
          dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
          mutate(level = 'Weekly',
                 controls = TRUE)) %>%
      bind_rows(
        momentary_wc %>%
          dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
          mutate(level = 'Momentary',
                 controls =TRUE)) %>%
  group_by(outcome,level, controls) %>%
  summarise(
    m   = mean(path_c),
    lwr = mean(path_c_CI_lower, na.rm = TRUE),
    upr = mean(path_c_CI_upper, na.rm = TRUE),
    p   = mean(path_c_p),
    controls = controls,
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
    est_ci = gsub("0\\.", ".", est_ci),
    p_fmt  = gsub("0\\.", ".", p_fmt),
    controls_label = ifelse(controls, "with_controls", "without_controls")
  ) %>%
  dplyr::select(outcome, level, controls_label, est_ci, p_fmt) %>%
  pivot_wider(
    names_from  = controls_label,
    values_from = c(est_ci, p_fmt),
    names_sep   = "_"
  ) %>%
  arrange(outcome, level)

#############################################
# Create gt table

h1_gt <- h1_for_gt %>%
  gt(rowname_col = "outcome", groupname_col = "level") %>%
  cols_label(
    est_ci_without_controls    = "Estimate [95% CI]",
    p_fmt_without_controls     = md("*p*"),
    est_ci_with_controls = "Estimate [95% CI]",
    p_fmt_with_controls  = md("*p*"),
  ) %>%
  tab_spanner(
    label   = "Without Covariates",
    columns = c(est_ci_without_controls, p_fmt_without_controls)
  ) %>%
  tab_spanner(
    label   = "With Covariates",
    columns = c(est_ci_with_controls, p_fmt_with_controls)
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
    locations = cells_column_spanners(spanners = c("Without Covariates", "With Covariates"))
  ) %>%
  tab_options(
    table.font.names          = "Helvetica, Arial, sans-serif",
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
  )

h1_gt

gtsave(h1_gt, "Plots/tables_s1.png")
