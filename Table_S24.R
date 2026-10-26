#############################################
#
#       Table S15
#       Indirect Effects Through Social Media Use
#
#############################################

library(dplyr)
library(tidyr)
library(gt)
library(magick)

source("supplementary_table_style.R")

#############################################
# Load data

anes_res <- read.csv("anes_w3_mediation_results.csv")
path_a_res <- read.csv("anes_w3_path_a_results.csv")

#############################################
# Format helpers

format_decimal <- function(x, digits = 3) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  gsub("(^|\\s|-|\\[|,)0\\.", "\\1.", out, perl = TRUE)
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < .001 ~ "<.001",
    TRUE ~ format_decimal(p, 3)
  )
}

format_est_ci <- function(est, low, high) {
  paste0(
    format_decimal(est, 3),
    " [",
    format_decimal(low, 3),
    ", ",
    format_decimal(high, 3),
    "]"
  )
}

#############################################
# Prepare data

outcome_order <- c(
  "Loneliness (r)",
  "Anhedonia (r)",
  "Depression (r)"
)

mediator_order <- c(
  "Social Media",
  "Facebook",
  "Instagram",
  "Reddit",
  "TikTok",
  "Twitter",
  "YouTube"
)

mediator_labels <- c(
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "twitter_frequency" = "Twitter",
  "youtube_frequency" = "YouTube"
)

outcome_labels <- c(
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)"
)

path_a_df <- path_a_res %>%
  filter(
    IV == "political_orientation",
    mediator %in% names(mediator_labels)
  ) %>%
  transmute(
    mediator_raw = mediator,
    path_a_p = p
  )

table_long <- anes_res %>%
  filter(
    sample == "all",
    model_type == "mediation",
    outcome %in% names(outcome_labels),
    mediator %in% names(mediator_labels)
  ) %>%
  transmute(
    mediator_raw = mediator,
    outcome_raw = outcome,
    path_b_p = b_path_p,
    acme_est,
    acme_ci_low,
    acme_ci_high,
    prop_mediated_est
  ) %>%
  left_join(path_a_df, by = "mediator_raw") %>%
  mutate(
    outcome = dplyr::recode(outcome_raw, !!!outcome_labels),
    mediator = dplyr::recode(mediator_raw, !!!mediator_labels),
    outcome = factor(outcome, levels = outcome_order),
    mediator = factor(mediator, levels = mediator_order),
    path_a_sig = path_a_p <= 0.05,
    path_b_sig = path_b_p <= 0.05,
    acme_sig = (acme_ci_low > 0 | acme_ci_high < 0) &
      is.finite(acme_ci_low) & is.finite(acme_ci_high),
    mediation_sig = path_a_sig & path_b_sig & acme_sig,
    path_a_p_fmt = format_p(path_a_p),
    path_b_p_fmt = format_p(path_b_p),
    acme_ci_prop = paste0(
      format_est_ci(acme_est, acme_ci_low, acme_ci_high),
      "\n(",
      format_decimal(prop_mediated_est * 100, 2),
      "%)"
    )
  ) %>%
  dplyr::select(
    mediator,
    outcome,
    path_a_p_fmt,
    path_a_sig,
    path_b_p_fmt,
    acme_ci_prop,
    mediation_sig
  ) %>%
  arrange(mediator, outcome)

#############################################
# Create one row per social-media measure

outcome_columns <- tibble::tribble(
  ~outcome,          ~key,
  "Loneliness (r)", "loneliness",
  "Anhedonia (r)",  "anhedonia",
  "Depression (r)", "depression"
)

table_wide <- table_long %>%
  mutate(
    outcome = as.character(outcome),
    outcome_key = outcome_columns$key[match(outcome, outcome_columns$outcome)]
  ) %>%
  dplyr::select(
    mediator,
    path_a_p_fmt,
    path_a_sig,
    outcome_key,
    path_b_p_fmt,
    acme_ci_prop,
    mediation_sig
  ) %>%
  pivot_wider(
    names_from = outcome_key,
    values_from = c(path_b_p_fmt, acme_ci_prop, mediation_sig),
    names_glue = "{.value}_{outcome_key}"
  ) %>%
  arrange(mediator)

path_b_cols <- paste0("path_b_p_fmt_", outcome_columns$key)
mediation_cols <- paste0("acme_ci_prop_", outcome_columns$key)
sig_cols <- paste0("mediation_sig_", outcome_columns$key)

column_labels <- c(
  list(path_a_p_fmt = html("Path a<br><em>p</em>")),
  stats::setNames(
    rep(list(html("Path b<br><em>p</em>")), nrow(outcome_columns)),
    path_b_cols
  ),
  stats::setNames(
    rep(list(html("ACME [95% CI]<br>(Prop. mediated)")), nrow(outcome_columns)),
    mediation_cols
  )
)

stopifnot(
  nrow(table_wide) == length(mediator_order),
  !anyNA(table_wide$path_a_p_fmt),
  !anyNA(table_wide$path_a_sig),
  !anyNA(table_wide[, path_b_cols]),
  !anyNA(table_wide[, mediation_cols]),
  !anyNA(table_wide[, sig_cols])
)

#############################################
# Create gt table

table_s15 <- table_wide %>%
  gt(rowname_col = "mediator", id = "table_s15") %>%
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
    locations = cells_stub(rows = mediator == "Social Media")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = path_a_p_fmt, rows = path_a_sig)
  ) %>%
  apply_supplementary_table_style(table_width = px(1850)) %>%
  opt_css(
    css = paste0(
      "#table_s15 th[id^='acme_ci_prop_'], ",
      "#table_s15 td[headers*='acme_ci_prop_'] { ",
      "padding-left: 1px !important; ",
      "padding-right: 10px !important; ",
      "white-space: pre-line !important; ",
      "text-align: center !important; }"
    )
  )

for (i in seq_len(nrow(outcome_columns))) {
  path_b_col <- path_b_cols[[i]]
  mediation_col <- mediation_cols[[i]]
  sig_col <- sig_cols[[i]]

  table_s15 <- table_s15 %>%
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

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s15,
  "Plots/tables_s15.png",
  vwidth = 2150,
  vheight = 0
)
