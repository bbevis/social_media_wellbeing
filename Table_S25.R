#############################################
#
#       Table S16
#       Age correlations with ANES variables
#
#############################################

library(dplyr)
library(tidyr)
library(gt)
library(survey)

source("supplementary_table_style.R")

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

#############################################
# Define variables

cor_vars <- c(
  "political_orientation",
  "total_platform_frequency",
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency",
  "loneliness",
  "anhedonia",
  "depression"
)

var_labels <- c(
  "political_orientation" = "Political Orientation",
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "twitter_frequency" = "Twitter",
  "youtube_frequency" = "YouTube",
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)"
)

var_sections <- c(
  "political_orientation" = "Political Orientation",
  "total_platform_frequency" = "Social Media Use",
  "facebook_frequency" = "Social Media Use",
  "instagram_frequency" = "Social Media Use",
  "reddit_frequency" = "Social Media Use",
  "tiktok_frequency" = "Social Media Use",
  "twitter_frequency" = "Social Media Use",
  "youtube_frequency" = "Social Media Use",
  "loneliness" = "Wellbeing Outcomes",
  "anhedonia" = "Wellbeing Outcomes",
  "depression" = "Wellbeing Outcomes"
)

section_order <- c(
  "Political Orientation",
  "Social Media Use",
  "Wellbeing Outcomes"
)

#############################################
#############################################
# Survey-weighted associations with age

weighted_z <- function(x, w) {
  mean_x <- weighted.mean(x, w, na.rm = TRUE)
  sd_x <- sqrt(sum(w * (x - mean_x)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
  (x - mean_x) / sd_x
}

age_association <- function(variable) {
  model_data <- anes_w3 %>%
    dplyr::select(age, value = all_of(variable), w3weight) %>%
    na.omit()

  model_data <- model_data %>%
    mutate(
      age_z = weighted_z(age, w3weight),
      value_z = weighted_z(value, w3weight)
    )

  design <- svydesign(
    ids = ~1,
    weights = ~w3weight,
    data = model_data
  )

  model <- svyglm(value_z ~ age_z, design = design)
  coefficient <- summary(model)$coefficients["age_z", ]
  interval <- as.numeric(confint(model, parm = "age_z"))

  tibble(
    variable = variable,
    n = nrow(model_data),
    estimate = unname(coefficient[1]),
    se = unname(coefficient[2]),
    t = unname(coefficient[3]),
    p = unname(coefficient[4]),
    df = df.residual(model),
    ci_low = interval[1],
    ci_high = interval[2]
  )
}

age_results <- bind_rows(lapply(cor_vars, age_association))

write.csv(
  age_results,
  "anes_age_association_results.csv",
  row.names = FALSE
)

cor_df <- age_results %>%
  mutate(
    section = factor(unname(var_sections[variable]), levels = section_order),
    variable = factor(unname(var_labels[variable]), levels = unname(var_labels[cor_vars])),
    est_ci = paste0(
      format_decimal(estimate, 3),
      " [",
      format_decimal(ci_low, 3),
      ", ",
      format_decimal(ci_high, 3),
      "]"
    ),
    p_fmt = format_p(p),
    sig = p <= 0.05,
    row_type = "measure",
    row_label = as.character(variable),
    row_order = as.integer(variable)
  ) %>%
  arrange(section, variable) %>%
  dplyr::select(section, row_type, row_label, row_order, est_ci, p_fmt, sig)

headers <- cor_df %>%
  distinct(section) %>%
  mutate(
    row_type = "section",
    row_label = "\u00a0",
    row_order = 0L,
    est_ci = "\u00a0",
    p_fmt = "\u00a0",
    sig = FALSE
  )

table_df <- bind_rows(headers, cor_df) %>%
  mutate(row_type = factor(row_type, levels = c("section", "measure"))) %>%
  arrange(section, row_type, row_order)

#############################################
# Create gt table

table_s25 <- table_df %>%
  gt() %>%
  cols_hide(c(section, row_type, row_order, sig)) %>%
  cols_label(
    row_label = "",
    est_ci = "Estimate [95% CI]",
    p_fmt = md("*p*")
  ) %>%
  cols_align("center", columns = c(est_ci, p_fmt)) %>%
  cols_align("left", columns = row_label) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = sig, columns = c(est_ci, p_fmt))
  ) %>%
  tab_style(
    style = cell_text(indent = px(30)),
    locations = cells_body(rows = row_type == "measure", columns = row_label)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = row_label == "Social Media", columns = row_label)
  ) %>%
  tab_style(
    style = cell_text(size = px(18)),
    locations = cells_body(rows = row_type == "section")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  apply_supplementary_table_style()

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s25,
  "Plots/tables_s25.png",
  vwidth = SUPPLEMENTARY_TABLE_VWIDTH,
  vheight = 0
)
