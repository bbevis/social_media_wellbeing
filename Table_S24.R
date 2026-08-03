#############################################
#
#       Table S24
#       Indirect Effects Through Social Media Use
#
#############################################

library(dplyr)
library(tidyr)
library(gt)
library(magick)

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
    est_ci = format_est_ci(acme_est, acme_ci_low, acme_ci_high),
    p_fmt = format_p(acme_p),
    sig = acme_p <= 0.05
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

table_s24 <- table_for_gt %>%
  gt(rowname_col = "stub") %>%
  cols_hide(c(mediator, outcome, row_type, sig)) %>%
  cols_label(
    est_ci = "ACME [95% CI]",
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
  tab_options(
    table.font.size = px(22),
    table.font.names = "Helvetica, Arial",
    data_row.padding = px(3),
    table.width = pct(52),
    heading.border.bottom.style = "solid",
    column_labels.border.top.style = "solid",
    column_labels.border.bottom.style = "solid",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    stub.border.style = "none",
    column_labels.padding = px(3)
  )

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(table_s24, "Plots/tables_s24.png", vwidth = 2200)
