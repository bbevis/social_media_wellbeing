#############################################
#
# H2 results table - Political Orientation on App Category Use
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

path_a_res <- read.csv("study1_path_a_results.csv")

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other","health", "total_activity")
minutes <- c("sm_minutes")

#############################################
# H2 table: combine and summarise

h2_df <- path_a_res %>%
  filter(mediator %in% c(platforms, activities, minutes)) %>%
  transmute(
    mediator,
    level,
    m = estimate,
    lwr = ci_low,
    upr = ci_high,
    p
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
      mediator == "total_activity"  ~ "Overall Screentime",
      mediator == "sm_minutes"  ~ "Social Media",
      TRUE ~ mediator
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(mediator, "Tiktok" = "TikTok", "Youtube" = "YouTube")
  )


mediator_order <- c(
  "Social Media",
  "Overall Screentime",
  "Creativity", "Education", "Entertainment", "Games", "Health", "Information",
  "Other", "Productivity", "Shopping", "Social", "Utilities",
  "Social Media",
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)
mediator_order <- unique(mediator_order)

h2_df <- h2_df %>%
  mutate(mediator = factor(mediator, levels = mediator_order)) %>%
  filter(!mediator_raw %in% c(
    "total_platform", "sm_minutes",
    "facebook", "instagram", "reddit", "snapchat", "tiktok", "twitter", "youtube"
  )) %>%
  filter(level != "Momentary") %>%
  filter(!(mediator_raw == "social" & level %in% c("Weekly", "Monthly")))

#############################################
# Prepare data for gt table

h2_for_gt <- h2_df %>%
  mutate(
    level = factor(level, levels = c("Weekly", "Monthly")),
    est_ci = sprintf("%.3f [%.3f, %.3f]", m, lwr, upr), # Format estimate and CI
    p_fmt  = sprintf("%.3f", p), # Format p value
    sig = p <= 0.05,
    est_ci = gsub("0\\.", ".", est_ci),
    p_fmt  = gsub("0\\.", ".", p_fmt)
  ) %>%
  dplyr::select(mediator_raw, mediator, level, est_ci, p_fmt, sig) %>%
  arrange(level, mediator)

#############################################
# Create gt table

h2_gt <- h2_for_gt %>%
  gt(rowname_col = "mediator", groupname_col = "level") %>%
  cols_hide(c(mediator_raw, sig)) %>%
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
    locations = cells_row_groups(groups = c("Weekly", "Monthly"))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(est_ci, p_fmt), rows = sig)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = mediator == "Overall Screentime")
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

h2_gt

gtsave(h2_gt, "Plots/tables_s4.png", vwidth = SUPPLEMENTARY_TABLE_VWIDTH, vheight = 0)
