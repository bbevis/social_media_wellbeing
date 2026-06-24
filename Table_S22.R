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
    model_type == "mediation"
  ) %>%
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
  group_by(mediator) %>%
  summarise(
    est = mean(a_path_est, na.rm = TRUE),
    low = mean(a_path_ci_low, na.rm = TRUE),
    high = mean(a_path_ci_high, na.rm = TRUE),
    p = mean(a_path_p, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mediator = factor(mediator, levels = mediator_order),
    est_ci = format_est_ci(est, low, high),
    p_fmt = format_p(p)
  ) %>%
  dplyr::select(mediator, est_ci, p_fmt) %>%
  arrange(mediator)

#############################################
# Create gt table

table_s22 <- table_df %>%
  gt(rowname_col = "mediator") %>%
  cols_label(
    est_ci = "Estimate [95% CI]",
    p_fmt = md("*p*")
  ) %>%
  cols_align("center") %>%
  tab_style(
    style = cell_text(align = "left", indent = px(20)),
    locations = cells_stub()
  ) %>%
  tab_options(
    table.font.size = px(23),
    table.font.names = "Helvetica, Arial",
    data_row.padding = px(2),
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

gtsave(table_s22, "Plots/tables_s22.png", vwidth = 2200)
