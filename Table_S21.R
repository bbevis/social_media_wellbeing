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
  "affective_wellbeing" = "Affective Wellbeing",
  "happy" = "Happy",
  "hopeful" = "Hopeful",
  "afraid" = "Afraid (r)",
  "angry" = "Angry (r)",
  "outraged" = "Outraged (r)",
  "worried" = "Worried (r)",
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)"
)

outcome_order <- c(
  "Loneliness (r)",
  "Anhedonia (r)",
  "Depression (r)",
  "Affective Wellbeing",
  "Happy",
  "Hopeful",
  "Afraid (r)",
  "Angry (r)",
  "Outraged (r)",
  "Worried (r)"
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
    row_type = case_when(
      outcome == "Affective Wellbeing" ~ "overall",
      outcome %in% c(
        "Happy",
        "Hopeful",
        "Afraid (r)",
        "Angry (r)",
        "Outraged (r)",
        "Worried (r)"
      ) ~ "affective_item",
      TRUE ~ "symptom_item"
    ),
    est_ci = format_est_ci(est, low, high),
    p_fmt = format_p(p),
    sig = p <= 0.05
  ) %>%
  dplyr::select(outcome, row_type, est_ci, p_fmt, sig) %>%
  arrange(outcome)

#############################################
# Create gt table

table_s21 <- table_df %>%
  gt(rowname_col = "outcome") %>%
  cols_hide(c(row_type, sig)) %>%
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
    style = cell_text(align = "left", indent = px(20)),
    locations = cells_stub()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = outcome == "Affective Wellbeing")
  ) %>%
  tab_style(
    style = cell_text(indent = px(40)),
    locations = cells_stub(rows = row_type == "affective_item")
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

gtsave(table_s21, "Plots/tables_s21.png", vwidth = 2200)
