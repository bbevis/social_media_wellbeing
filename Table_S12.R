#############################################
#
#       Table S12
#       Weekly mediation with explicit nonuse coded as zero
#       Social media use between political orientation and wellbeing
#
#############################################

library(dplyr)
library(gt)
source("supplementary_table_style.R")
library(tidyr)

#############################################
# Load data

weekly_wc_nonuse_zero <- read.csv("weekly_results_withcontrols_nonuse_zero.csv")

platforms <- c(
  "total_platform",
  "facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat"
)

#############################################
# Prepare data

path_a_df <- weekly_wc_nonuse_zero %>%
  filter(mediator %in% platforms, outcome == "awb_week") %>%
  group_by(mediator) %>%
  summarise(path_a_p = mean(path_a_p), .groups = "drop")

h4_df <- weekly_wc_nonuse_zero %>%
  dplyr::select(
    mediator, outcome, path_a_p, path_b_p,
    ACME, prop_mediated, ACME_CI_lower, ACME_CI_upper
  ) %>%
  filter(mediator %in% platforms) %>%
  group_by(mediator, outcome) %>%
  summarise(
    path_a_p_model = mean(path_a_p),
    path_b_p = mean(path_b_p),
    acme = mean(ACME),
    acme_lwr = mean(ACME_CI_lower, na.rm = TRUE),
    acme_upr = mean(ACME_CI_upper, na.rm = TRUE),
    prop_mediated = mean(prop_mediated),
    .groups = "drop"
  ) %>%
  left_join(path_a_df, by = "mediator") %>%
  mutate(
    level = factor("Weekly", levels = "Weekly"),
    path_a_sig = path_a_p <= 0.05,
    path_b_sig = path_b_p <= 0.05,
    acme_sig = (acme_lwr > 0 | acme_upr < 0) &
      is.finite(acme_lwr) & is.finite(acme_upr),
    mediation_sig = path_a_p_model <= 0.05 & path_b_sig & acme_sig,
    mediator = case_when(
      mediator == "total_platform" ~ "Social Media",
      TRUE ~ mediator
    ),
    outcome = dplyr::recode(
      outcome,
      "awb_week" = "Affective Wellbeing",
      "angry_rev" = "Angry (r)",
      "worried_rev" = "Worried (r)",
      "happy" = "Happy",
      "sad_rev" = "Sad (r)",
      "stressed_rev" = "Stressed (r)",
      "lonely_rev" = "Lonely (r)",
      "accepted" = "Accepted"
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(mediator, "Tiktok" = "TikTok", "Youtube" = "YouTube")
  )

outcome_order <- c(
  "Affective Wellbeing",
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
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)

h4_df <- h4_df %>%
  mutate(
    mediator = factor(mediator, levels = mediator_order),
    outcome = factor(outcome, levels = outcome_order)
  ) %>%
  filter(!is.na(mediator), !is.na(outcome))

stopifnot(
  !anyNA(h4_df$path_a_p),
  !anyNA(h4_df$path_a_p_model),
  !anyNA(h4_df$path_b_p)
)

#############################################
# Prepare data for gt table

h4_for_gt <- h4_df %>%
  mutate(
    path_a_p_fmt = sprintf("%.3f", path_a_p),
    path_b_p_fmt = sprintf("%.3f", path_b_p),
    acme_ci_prop = sprintf(
      "%.3f [%.3f, %.3f]\n(%.2f%%)",
      acme, acme_lwr, acme_upr, prop_mediated * 100
    )
  ) %>%
  dplyr::select(
    level, mediator, outcome,
    path_a_p_fmt, path_b_p_fmt, acme_ci_prop,
    path_a_sig, mediation_sig
  ) %>%
  mutate(across(where(is.character),
                ~ gsub("(^|\\s|-|\\[|,)0\\.", "\\1.", .x, perl = TRUE))) %>%
  arrange(outcome, mediator)

#############################################
# Create one table with shared weekly and social-media rows

outcome_columns <- tibble::tribble(
  ~outcome,                ~key,
  "Affective Wellbeing",  "affective_wellbeing",
  "Accepted",             "accepted",
  "Angry (r)",            "angry",
  "Happy",                "happy",
  "Lonely (r)",           "lonely",
  "Sad (r)",              "sad",
  "Stressed (r)",         "stressed",
  "Worried (r)",          "worried"
)

h4_wide <- h4_for_gt %>%
  mutate(
    outcome = as.character(outcome),
    outcome_key = outcome_columns$key[match(outcome, outcome_columns$outcome)],
    mediator = factor(mediator, levels = mediator_order)
  ) %>%
  dplyr::select(
    level, mediator, path_a_p_fmt, path_a_sig,
    outcome_key, path_b_p_fmt, acme_ci_prop, mediation_sig
  ) %>%
  pivot_wider(
    names_from = outcome_key,
    values_from = c(path_b_p_fmt, acme_ci_prop, mediation_sig),
    names_glue = "{.value}_{outcome_key}"
  ) %>%
  mutate(
    across(starts_with("path_b_p_fmt_"), ~ coalesce(.x, "—")),
    across(starts_with("acme_ci_prop_"), ~ coalesce(.x, "—")),
    across(starts_with("mediation_sig_"), ~ coalesce(.x, FALSE))
  ) %>%
  arrange(mediator)

path_b_cols <- paste0("path_b_p_fmt_", outcome_columns$key)
mediation_cols <- paste0("acme_ci_prop_", outcome_columns$key)
sig_cols <- paste0("mediation_sig_", outcome_columns$key)

column_labels <- c(
  list(path_a_p_fmt = md("Path a *p*")),
  stats::setNames(rep(list(md("Path b *p*")), nrow(outcome_columns)), path_b_cols),
  stats::setNames(
    rep(list(html("ACME [95% CI]<br>(Prop. mediated)")), nrow(outcome_columns)),
    mediation_cols
  )
)

h4_gt <- h4_wide %>%
  gt(rowname_col = "mediator", groupname_col = "level", id = "table_s18") %>%
  cols_hide(c(path_a_sig, all_of(sig_cols))) %>%
  cols_label(.list = column_labels) %>%
  cols_width(
    mediator ~ px(210),
    path_a_p_fmt ~ px(110),
    all_of(path_b_cols) ~ px(110),
    all_of(mediation_cols) ~ px(400)
  ) %>%
  cols_align(
    align = "center",
    columns = c(path_a_p_fmt, all_of(c(path_b_cols, mediation_cols)))
  ) %>%
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
    locations = cells_stub(rows = mediator == "Social Media")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = path_a_p_fmt, rows = path_a_sig)
  )

for (i in seq_len(nrow(outcome_columns))) {
  path_b_col <- path_b_cols[[i]]
  mediation_col <- mediation_cols[[i]]
  sig_col <- sig_cols[[i]]

  h4_gt <- h4_gt %>%
    tab_spanner(
      label = md(paste0("**", outcome_columns$outcome[[i]], "**")),
      columns = all_of(c(path_b_col, mediation_col))
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = all_of(c(path_b_col, mediation_col)),
        rows = !!rlang::sym(sig_col)
      )
    )
}

h4_gt <- h4_gt %>%
  apply_supplementary_table_style(table_width = px(4400)) %>%
  tab_options(table.font.size = px(34)) %>%
  opt_css(
    css = paste0(
      "#table_s18 th[id^='acme_ci_prop_'], ",
      "#table_s18 td[headers*='acme_ci_prop_'] { ",
      "padding-left: 1px !important; ",
      "padding-right: 10px !important; ",
      "white-space: pre-line !important; ",
      "text-align: center !important; }"
    )
  )

h4_gt

gtsave(
  h4_gt,
  "Plots/tables_s12.png",
  vwidth = 4700,
  vheight = 0
)
