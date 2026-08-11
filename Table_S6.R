#############################################
#
# H4 results table - Mediation
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
library(gt)
source("supplementary_table_style.R")
library(tidyr)
library(webshot2)
library(magick)

#############################################
# Load data

monthly_wc = read.csv('monthly_results_withcontrols.csv')
weekly_wc = read.csv('weekly_results_withcontrols.csv')
momentary_wc = read.csv('momentary_results_withcontrols.csv')

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other","health", "total_activity")
minutes <- c("sm_minutes")

#############################################
# H4 table

h4_df = monthly_wc %>%
  dplyr::select(mediator, outcome, ACME, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
  mutate(level = 'Monthly') %>%
  bind_rows(
    weekly_wc %>%
      dplyr::select(mediator, outcome, ACME, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = 'Weekly')) %>%
  bind_rows(
    momentary_wc %>%
      dplyr::select(mediator, outcome, ACME, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = 'Momentary')) %>%
  filter(mediator %in% c(platforms, minutes)) %>%
  group_by(mediator, level, outcome) %>%
  summarise(
    m   = mean(ACME),
    lwr = mean(ACME_CI_lower, na.rm = TRUE),
    upr = mean(ACME_CI_upper, na.rm = TRUE),
    p   = mean(prop_mediated),
    .groups = "drop"
  ) %>%
  mutate(
    mediator_raw = mediator,
    level = factor(level, levels = c("Momentary","Weekly","Monthly")),
    sig   = !(lwr < 0 & upr > 0) & is.finite(lwr) & is.finite(upr),
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
    outcome = dplyr::recode(
      outcome,
      "awb_momentary" = "Affective Wellbeing",
      "awb_t2"        = "Affective Wellbeing",
      "awb_week"      = "Affective Wellbeing",
      "swls_t2"       = "Life Satisfaction",
      "uls_t2"        = "Lonely (r)",
      "angry_rev"     = "Angry (r)",
      "worried_rev"   = "Worried (r)",
      "happy"         = "Happy",
      "sad_rev"       = "Sad (r)",
      "stressed_rev"  = "Stressed (r)",
      "lonely_rev"    = "Lonely (r)",
      "accepted"      = "Accepted"
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(mediator, "Tiktok" = "TikTok", "Youtube" = "YouTube")
  )


outcome_order <- c(
  "Affective Wellbeing",
  "Life Satisfaction",
  "Accepted",
  "Angry (r)",
  "Happy",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

mediator_order <- c(
  "Social Media",
  "All Apps",
  "Creativity", "Education", "Entertainment", "Games", "Health", "Information",
  "Other", "Productivity", "Shopping", "Social", "Utilities",
  "Social Media",
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)
mediator_order <- unique(mediator_order)

h4_df = h4_df %>%
  mutate(mediator = factor(mediator, levels = mediator_order)) %>%
  mutate(outcome = factor(outcome, levels = outcome_order)) %>%
  filter(!mediator%in%c("All Apps",
                        "Creativity", "Education", "Entertainment", "Games", "Health", "Information",
                        "Other", "Productivity", "Shopping", "Social", "Utilities"))


#############################################
# Prepare data for gt table

h4_for_gt <- h4_df %>%
  mutate(
    est_ci = sprintf("%.3f [%.3f, %.3f]", m, lwr, upr),
    p_fmt  = sprintf("%.2f%%", p * 100),
    sig = sig
  ) %>%
  dplyr::select(mediator_raw, mediator, outcome, level, est_ci, p_fmt, sig) %>%
  # Remove leading zeros everywhere (ACME + CI + p-values)
  mutate(across(where(is.character),
                ~ gsub("(^|\\s|-|\\[|,)0\\.", "\\1.", .x, perl = TRUE))) %>%
  arrange(level, mediator, outcome)



###############################################################
# DEFINE COMPOSITE VS SINGLE OUTCOMES


composite_outcomes <- c(
  "Affective Wellbeing",
  "Life Satisfaction"
)

single_outcomes <- c(
  "Accepted", "Angry (r)", "Happy", "Lonely (r)",
  "Sad (r)", "Stressed (r)", "Worried (r)"
)

h4_left  <- h4_for_gt %>% filter(outcome %in% composite_outcomes)
h4_right <- h4_for_gt %>% filter(outcome %in% single_outcomes)

#############################################
# Create gt table

make_h4_table <- function(df_input) {
  
  df_input %>%
    # ensure ordering
    mutate(
      level    = factor(level,    levels = c("Momentary","Weekly","Monthly")),
      mediator = factor(mediator, levels = mediator_order),
      outcome  = factor(outcome,  levels = outcome_order)
    ) %>%
    arrange(level, mediator, outcome) %>%
    
    # ----- add one mediator "header" row per level × mediator -----
  {
    base <- .
    
    headers <- base %>%
      distinct(level, mediator) %>%
      mutate(
        outcome = NA,
        est_ci = "",
        p_fmt = "",
        sig = FALSE,
        row_type = "mediator"
      )
    
    rows <- base %>%
      mutate(row_type = "outcome")
    
    bind_rows(headers, rows)
  } %>%
    
    arrange(level, mediator, row_type, outcome) %>%
    
    # ----- single stub column: mediator vs outcome label -----
  mutate(
    stub = ifelse(
      row_type == "mediator",
      as.character(mediator),   # mediator label
      as.character(outcome)     # outcome label
    )
  ) %>%
    
    # mediator rows: make stats blank (not "NA")
    mutate(
      est_ci = ifelse(row_type == "mediator", "", est_ci),
      p_fmt = ifelse(row_type == "mediator", "", p_fmt),
      sig = ifelse(row_type == "mediator", FALSE, sig)
    ) %>%
    
    dplyr::select(
      level, mediator, outcome, row_type, stub, est_ci, p_fmt, sig
    ) %>%
    
    # ----- build gt table -----
  gt(rowname_col = "stub", groupname_col = "level") %>%
    
    # hide helper columns – still usable in `rows =` conditions
    cols_hide(c(mediator, outcome, row_type, sig)) %>%
    
    # column labels
    cols_label(
      est_ci = "ACME [95% CI]",
      p_fmt = "Prop. mediated"
    ) %>%
    
    # stub left, numbers center
    cols_align("center") %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_stub()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = c(est_ci, p_fmt), rows = sig)
    ) %>%
    
    # bold level row-group labels
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) %>%
    
    # INDENT + BOLD MEDIATORS ONLY
    tab_style(
      style = cell_text(weight = "bold", indent = px(20)),
      locations = cells_stub(rows = row_type == "mediator" &
                               !(stub == "Social Media" & level == "Momentary"))
    ) %>%
    
    # INDENT OUTCOMES (NO BOLD)
    tab_style(
      style = cell_text(indent = px(40)),
      locations = cells_stub(rows = row_type == "outcome")
    ) %>%
    
    # general table options
    tab_options(
      table.font.size  = px(25),
      data_row.padding = px(4),
      table.width      = pct(45),
      heading.border.bottom.style       = "solid",
      column_labels.border.top.style    = "solid",
      column_labels.border.bottom.style = "solid",
      table.border.top.style            = "none",
      table.border.bottom.style         = "none",
      table_body.hlines.style           = "none",
      table_body.vlines.style           = "none",
      stub.border.style                 = "none",
      row_group.border.top.style        = "none",
      row_group.border.bottom.style     = "none"
    ) %>%
    apply_supplementary_table_style(table_width = pct(45))
}



###############################################################
# BUILD TABLE A & B


h4_gt_left  <- make_h4_table(h4_left)  %>%
  tab_header(title = md("**A. Composite Wellbeing Measures**"))

h4_gt_right <- make_h4_table(h4_right) %>%
  tab_header(title = md("**B. Single-Item Wellbeing Measures**"))

# SAVE BOTH TABLES
gtsave(h4_gt_left,  "Plots/table_h4_A.png", vwidth = SUPPLEMENTARY_WIDE_TABLE_VWIDTH, vheight = 0)
gtsave(h4_gt_right, "Plots/table_h4_B.png", vwidth = SUPPLEMENTARY_WIDE_TABLE_VWIDTH, vheight = 0)

# COMBINE IMAGES INTO ONE PNG
img_A <- image_read("Plots/table_h4_A.png")
img_B <- image_read("Plots/table_h4_B.png")

combined_img <- image_append(c(img_A, img_B), stack = FALSE)

image_write(combined_img, "Plots/tables_s6.png")
