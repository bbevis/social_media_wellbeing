#############################################
#
#       Table S23
#       Social Media Use and Wellbeing
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

table_df <- anes_res %>%
  filter(
    sample == "all",
    model_type == "mediation",
    outcome %in% c("loneliness", "anhedonia", "depression")
  ) %>%
  mutate(
    outcome = dplyr::recode(
      outcome,
      "loneliness" = "Loneliness (r)",
      "anhedonia" = "Anhedonia (r)",
      "depression" = "Depression (r)"
    ),
    mediator = dplyr::recode(
      mediator,
      "facebook_frequency" = "Facebook",
      "instagram_frequency" = "Instagram",
      "reddit_frequency" = "Reddit",
      "tiktok_frequency" = "TikTok",
      "youtube_frequency" = "YouTube",
      "twitter_frequency" = "Twitter",
      "total_platform_frequency" = "Social Media"
    ),
    outcome = factor(outcome, levels = outcome_order),
    mediator = factor(mediator, levels = mediator_order),
    est_ci = format_est_ci(b_path_est, b_path_ci_low, b_path_ci_high),
    p_fmt = format_p(b_path_p),
    sig = b_path_p <= 0.05
  ) %>%
  dplyr::select(mediator, outcome, est_ci, p_fmt, sig) %>%
  arrange(mediator, outcome)

#############################################
# Add platform header rows

headers <- table_df %>%
  distinct(mediator) %>%
  mutate(
    outcome = NA,
    est_ci = "",
    p_fmt = "",
    sig = FALSE,
    row_type = "platform"
  )

rows <- table_df %>%
  mutate(row_type = "outcome")

table_for_gt <- bind_rows(headers, rows) %>%
  mutate(row_type = factor(row_type, levels = c("platform", "outcome"))) %>%
  arrange(mediator, row_type, outcome) %>%
  mutate(
    stub = ifelse(
      row_type == "platform",
      as.character(mediator),
      as.character(outcome)
    ),
    across(
      c(est_ci, p_fmt),
      ~ ifelse(is.na(.x), "", .x)
    ),
    sig = ifelse(is.na(sig), FALSE, sig)
  ) %>%
  dplyr::select(
    mediator,
    outcome,
    row_type,
    stub,
    est_ci,
    p_fmt,
    sig
  )

#############################################
# Create gt table

table_s23 <- table_for_gt %>%
  gt(rowname_col = "stub") %>%
  cols_hide(c(mediator, outcome, row_type, sig)) %>%
  cols_label(
    est_ci = "Estimate [95% CI]",
    p_fmt = md("*p*")
  ) %>%
  cols_align("center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(est_ci, p_fmt), rows = sig)
  ) %>%
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", indent = px(20)),
    locations = cells_stub(rows = row_type == "platform")
  ) %>%
  tab_style(
    style = cell_text(indent = px(40)),
    locations = cells_stub(rows = row_type == "outcome")
  ) %>%
  apply_supplementary_table_style()

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s23,
  "Plots/tables_s23.png",
  vwidth = SUPPLEMENTARY_TABLE_VWIDTH,
  vheight = 0
)
