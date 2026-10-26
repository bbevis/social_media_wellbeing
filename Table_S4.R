#############################################
#
# H3 results table - Social/Digital Media Use on Wellbeing
#
#############################################



library(dplyr)
library(tidyr)
library(gt)
source("supplementary_table_style.R")
library(tools)

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

h3_df <- monthly_wc %>%
  dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
  mutate(level = 'Monthly') %>%
  bind_rows(
    weekly_wc %>%
      dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
      mutate(level = 'Weekly')) %>%
  bind_rows(
    momentary_wc %>%
      dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
      mutate(level = 'Momentary')) %>%
  filter(mediator %in% c(platforms, minutes)) %>%
  group_by(mediator, level, outcome) %>%
  summarise(
    m   = mean(path_b),
    lwr = mean(path_b_CI_lower, na.rm = TRUE),
    upr = mean(path_b_CI_upper, na.rm = TRUE),
    p   = mean(path_b_p),
    .groups = "drop"
  ) %>%
  mutate(
    mediator_raw = mediator,
    level = factor(level, levels = c("Momentary","Weekly","Monthly")),
    sig   = p <= 0.05,
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
  "Accepted",
  "Angry (r)",
  "Happy",
  "Life Satisfaction",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

mediator_order <- c(
  "Social Media",
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)

h3_df <- h3_df %>%
  mutate(mediator = factor(mediator, levels = mediator_order)) %>%
  mutate(outcome = factor(outcome, levels = outcome_order)) %>%
  filter(!is.na(mediator), !is.na(outcome))


#############################################
# Prepare data for gt table

h3_for_gt <- h3_df %>%
  mutate(
    est_ci = sprintf("%.3f [%.3f, %.3f]", m, lwr, upr),
    p_fmt  = sprintf("%.3f", p),
    sig = p <= 0.05
  ) %>%
  dplyr::select(mediator, outcome, level, est_ci, p_fmt, sig) %>%
  # Remove leading zeros everywhere (ACME + CI + p-values)
  mutate(across(where(is.character),
                ~ gsub("(^|\\s|-|\\[|,)0\\.", "\\1.", .x, perl = TRUE))) %>%
  arrange(outcome, mediator, level)

#############################################
# Create one table with shared time and social-media rows

level_order <- c("Momentary", "Weekly", "Monthly")

outcome_columns <- tibble::tribble(
  ~outcome,                ~key,
  "Affective Wellbeing",  "affective_wellbeing",
  "Accepted",             "accepted",
  "Angry (r)",            "angry",
  "Happy",                "happy",
  "Life Satisfaction",    "life_satisfaction",
  "Lonely (r)",           "lonely",
  "Sad (r)",              "sad",
  "Stressed (r)",         "stressed",
  "Worried (r)",          "worried"
)

h3_wide <- h3_for_gt %>%
  mutate(
    outcome = as.character(outcome),
    outcome_key = outcome_columns$key[match(outcome, outcome_columns$outcome)],
    mediator = factor(mediator, levels = mediator_order),
    level = factor(level, levels = level_order)
  ) %>%
  dplyr::select(level, mediator, outcome_key, est_ci, p_fmt, sig) %>%
  pivot_wider(
    names_from = outcome_key,
    values_from = c(est_ci, p_fmt, sig),
    names_glue = "{.value}_{outcome_key}"
  ) %>%
  mutate(
    across(starts_with("est_ci_"), ~ coalesce(.x, "—")),
    across(starts_with("p_fmt_"), ~ coalesce(.x, "—")),
    across(starts_with("sig_"), ~ coalesce(.x, FALSE))
  ) %>%
  arrange(level, mediator)

estimate_cols <- paste0("est_ci_", outcome_columns$key)
p_cols <- paste0("p_fmt_", outcome_columns$key)
sig_cols <- paste0("sig_", outcome_columns$key)

column_labels <- c(
  stats::setNames(as.list(rep("Estimate [95% CI]", nrow(outcome_columns))), estimate_cols),
  stats::setNames(rep(list(md("*p*")), nrow(outcome_columns)), p_cols)
)

h3_gt <- h3_wide %>%
  gt(rowname_col = "mediator", groupname_col = "level", id = "table_s4") %>%
  cols_hide(all_of(sig_cols)) %>%
  cols_label(.list = column_labels) %>%
  cols_width(
    mediator ~ px(210),
    all_of(estimate_cols) ~ px(330),
    all_of(p_cols) ~ px(80)
  ) %>%
  cols_align(align = "center", columns = all_of(c(estimate_cols, p_cols))) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_stub()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = mediator == "Social Media" & level != "Momentary")
  )

for (i in seq_len(nrow(outcome_columns))) {
  estimate_col <- estimate_cols[[i]]
  p_col <- p_cols[[i]]
  sig_col <- sig_cols[[i]]

  h3_gt <- h3_gt %>%
    tab_spanner(
      label = md(paste0("**", outcome_columns$outcome[[i]], "**")),
      columns = all_of(c(estimate_col, p_col))
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = all_of(c(estimate_col, p_col)),
        rows = !!rlang::sym(sig_col)
      )
    )
}

h3_gt <- h3_gt %>%
  apply_supplementary_table_style(table_width = px(3900)) %>%
  tab_options(table.font.size = px(34)) %>%
  opt_css(
    css = paste0(
      "#table_s4 th[id^='p_fmt_'], ",
      "#table_s4 td[headers*='p_fmt_'] { ",
      "padding-left: 1px !important; ",
      "padding-right: 29px !important; ",
      "text-align: center !important; }"
    )
  )

h3_gt

gtsave(
  h3_gt,
  "Plots/tables_s4.png",
  vwidth = 4200,
  vheight = 0
)
