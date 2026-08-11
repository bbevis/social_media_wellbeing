#############################################
#
#       Table S21
#       Political Orientation and Wellbeing
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

outcome_lookup <- c(
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)"
)

outcome_order <- c(
  "Loneliness (r)",
  "Anhedonia (r)",
  "Depression (r)"
)

table_df <- anes_res %>%
  filter(
    sample == "all",
    model_type == "mediation",
    mediator == "total_platform_frequency",
    outcome %in% names(outcome_lookup)
  ) %>%
  transmute(
    outcome,
    est = c_path_est,
    low = c_path_ci_low,
    high = c_path_ci_high,
    p = c_path_p
  ) %>%
  mutate(
    outcome = dplyr::recode(outcome, !!!outcome_lookup),
    outcome = factor(outcome, levels = outcome_order),
    est_ci = format_est_ci(est, low, high),
    p_fmt = format_p(p),
    sig = p <= 0.05
  ) %>%
  dplyr::select(outcome, est_ci, p_fmt, sig) %>%
  arrange(outcome)

#############################################
# Create gt table

table_s21 <- table_df %>%
  gt(rowname_col = "outcome") %>%
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
  apply_supplementary_table_style()

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s21,
  "Plots/tables_s21.png",
  vwidth = SUPPLEMENTARY_TABLE_VWIDTH,
  vheight = 0
)
