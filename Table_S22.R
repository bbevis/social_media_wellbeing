#############################################
#
#       Table S22
#       Political Orientation and Social Media Use
#
#############################################

library(dplyr)
library(tidyr)
library(gt)
library(magick)

source("supplementary_table_style.R")

#############################################
# Load data

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

mediator_order <- c(
  "Social Media",
  "Facebook",
  "Instagram",
  "Reddit",
  "TikTok",
  "Twitter",
  "YouTube"
)

table_df <- path_a_res %>%
  mutate(
    mediator = dplyr::recode(
      mediator,
      "facebook_frequency" = "Facebook",
      "instagram_frequency" = "Instagram",
      "reddit_frequency" = "Reddit",
      "tiktok_frequency" = "TikTok",
      "youtube_frequency" = "YouTube",
      "twitter_frequency" = "Twitter",
      "total_platform_frequency" = "Social Media"
    )
  ) %>%
  mutate(
    mediator = factor(mediator, levels = mediator_order),
    est_ci = format_est_ci(estimate, ci_low, ci_high),
    p_fmt = format_p(p),
    sig = p <= 0.05
  ) %>%
  dplyr::select(mediator, est_ci, p_fmt, sig) %>%
  arrange(mediator)

#############################################
# Create gt table

table_s22 <- table_df %>%
  gt(rowname_col = "mediator") %>%
  cols_hide(sig) %>%
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
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = mediator == "Social Media")
  ) %>%
  apply_supplementary_table_style()

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s22,
  "Plots/tables_s22.png",
  vwidth = SUPPLEMENTARY_TABLE_VWIDTH,
  vheight = 0
)
