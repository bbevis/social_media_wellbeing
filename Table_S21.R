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

anes_w3 <- read.csv("anes_w3.csv")

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

full_sample_controls <- c(
  "age",
  "gender",
  "race_ethnicity",
  "income",
  "education"
)

run_c_path <- function(data, outcome) {
  all_vars <- c(
    outcome,
    "political_orientation",
    "total_platform_frequency",
    full_sample_controls
  )
  
  model_data <- data %>%
    dplyr::select(all_of(all_vars)) %>%
    na.omit()
  
  model_data$Yz <- as.numeric(scale(model_data[[outcome]]))
  
  formula_c <- as.formula(
    paste(
      "Yz ~ political_orientation +",
      paste(full_sample_controls, collapse = " + ")
    )
  )
  
  model_c <- lm(formula_c, data = model_data)
  sum_c <- summary(model_c)
  ci_c <- confint(model_c, level = 0.95)
  coef_c <- sum_c$coefficients
  
  data.frame(
    outcome = outcome,
    est = coef_c["political_orientation", "Estimate"],
    low = ci_c["political_orientation", 1],
    high = ci_c["political_orientation", 2],
    p = coef_c["political_orientation", "Pr(>|t|)"]
  )
}

table_df <- data.frame(
  outcome = names(outcome_lookup),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  do(run_c_path(anes_w3, .$outcome)) %>%
  ungroup() %>%
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
    p_fmt = format_p(p)
  ) %>%
  dplyr::select(outcome, row_type, est_ci, p_fmt) %>%
  arrange(outcome)

#############################################
# Create gt table

table_s21 <- table_df %>%
  gt(rowname_col = "outcome") %>%
  cols_hide(row_type) %>%
  cols_label(
    est_ci = "Estimate [95% CI]",
    p_fmt = md("*p*")
  ) %>%
  cols_align("center") %>%
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
