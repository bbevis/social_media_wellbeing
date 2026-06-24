#############################################
#
# H2 results table - Political Orientation on App Category Use
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

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other", "total_activity")
minutes <- c("sm_minutes")

#############################################
# H2 table: combine and summarise

h2_df = monthly_nc %>%
  filter(outcome == 'awb_t2') %>%
  dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
  mutate(level = 'Monthly',
         controls = FALSE) %>%
  bind_rows(
    weekly_nc %>%
      filter(outcome == 'awb_week') %>%
      dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
      mutate(level = 'Weekly',
             controls = FALSE)) %>%
  bind_rows(
    momentary_nc %>%
      filter(outcome == 'awb_momentary') %>%
      dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
      mutate(level = 'Momentary',
             controls = FALSE)) %>%
  bind_rows(
    monthly_wc %>%
      filter(outcome == 'awb_t2') %>%
      dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
      mutate(level = 'Monthly',
             controls = TRUE)) %>%
  bind_rows(
    weekly_wc %>%
      filter(outcome == 'awb_week') %>%
      dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
      mutate(level = 'Weekly',
             controls = TRUE)) %>%
  bind_rows(
    momentary_wc %>%
      filter(outcome == 'awb_momentary') %>%
      dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
      mutate(level = 'Momentary',
             controls =TRUE)) %>%
  filter(mediator %in% c(platforms, activities, minutes)) %>%
  group_by(mediator, level, controls) %>%
  summarise(
    m   = mean(path_a),
    lwr = mean(path_a_CI_lower, na.rm = TRUE),
    upr = mean(path_a_CI_upper, na.rm = TRUE),
    p   = mean(path_a_p),
    controls = controls,
    .groups = "drop"
  ) %>%
  mutate(
    mediator_raw = mediator,
    level = factor(level, levels = c("Momentary","Weekly","Monthly")),
    type  = case_when(
      mediator %in% platforms  ~ "Platform",
      mediator %in% activities ~ "Activity",
      TRUE ~ "Rest"
    ),
    mediator = case_when(
      mediator == "total_platform"  ~ "Social Media",
      mediator == "total_activity"  ~ "All Apps",
      mediator == "sm_minutes"  ~ "Social Media",
      TRUE ~ mediator
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(mediator, "Tiktok" = "TikTok", "Youtube" = "YouTube")
  )


mediator_order <- c(
  "Social Media",
  "All Apps",
  "Creativity", "Education", "Entertainment", "Games", "Information",
  "Other", "Productivity", "Shopping", "Social", "Utilities",
  "Social Media",
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)
mediator_order <- unique(mediator_order)

h2_df <- h2_df %>%
  mutate(mediator = factor(mediator, levels = mediator_order)) %>%
  filter(!mediator_raw %in% c(
    "total_platform",
    "facebook", "instagram", "reddit", "snapchat", "tiktok", "twitter", "youtube"
  ))

#############################################
# Prepare data for gt table

h2_for_gt <- h2_df %>%
  mutate(
    level = factor(level, levels = c("Momentary", "Weekly", "Monthly")),
    est_ci = sprintf("%.3f [%.3f, %.3f]", m, lwr, upr), # Format estimate and CI
    p_fmt  = sprintf("%.3f", p), # Format p value
    est_ci = gsub("0\\.", ".", est_ci),
    p_fmt  = gsub("0\\.", ".", p_fmt),
    controls_label = ifelse(controls, "with_controls", "without_controls")
  ) %>%
  dplyr::select(mediator_raw, mediator, level, controls_label, est_ci, p_fmt) %>%
  pivot_wider(
    names_from  = controls_label,
    values_from = c(est_ci, p_fmt),
    names_sep   = "_"
  ) %>%
  arrange(level, mediator)

#############################################
# Create gt table

h2_gt <- h2_for_gt %>%
  gt(rowname_col = "mediator", groupname_col = "level") %>%
  cols_hide(mediator_raw) %>%
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
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = mediator == "All Apps" |
                             (mediator == "Social Media" & level != "Momentary"))
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

h2_gt

gtsave(h2_gt, "Plots/tables_s3.png")
