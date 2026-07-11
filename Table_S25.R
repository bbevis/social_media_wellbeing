#############################################
#
#       Table S25
#       Age correlations with ANES variables
#
#############################################

library(dplyr)
library(tidyr)
library(gt)

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
  "affective_wellbeing",
  "loneliness",
  "anhedonia",
  "depression",
  "happy",
  "hopeful",
  "afraid",
  "angry",
  "outraged",
  "worried"
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
  "affective_wellbeing" = "Affective Wellbeing",
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)",
  "happy" = "Happy",
  "hopeful" = "Hopeful",
  "afraid" = "Afraid (r)",
  "angry" = "Angry (r)",
  "outraged" = "Outraged (r)",
  "worried" = "Worried (r)"
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
  "affective_wellbeing" = "Wellbeing Outcomes",
  "loneliness" = "Wellbeing Outcomes",
  "anhedonia" = "Wellbeing Outcomes",
  "depression" = "Wellbeing Outcomes",
  "happy" = "Wellbeing Outcomes",
  "hopeful" = "Wellbeing Outcomes",
  "afraid" = "Wellbeing Outcomes",
  "angry" = "Wellbeing Outcomes",
  "outraged" = "Wellbeing Outcomes",
  "worried" = "Wellbeing Outcomes"
)

section_order <- c(
  "Political Orientation",
  "Social Media Use",
  "Wellbeing Outcomes"
)

#############################################
# Spearman correlations with age

cor_df <- anes_w3 %>%
  dplyr::select(age, all_of(cor_vars)) %>%
  pivot_longer(
    cols = all_of(cor_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(age), !is.na(value)) %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    rho = suppressWarnings(
      cor(age, value, method = "spearman", use = "complete.obs")
    ),
    p = suppressWarnings(
      cor.test(age, value, method = "spearman", exact = FALSE)$p.value
    ),
    .groups = "drop"
  ) %>%
  mutate(
    section = factor(unname(var_sections[variable]), levels = section_order),
    variable = factor(unname(var_labels[variable]), levels = unname(var_labels[cor_vars])),
    rho_fmt = format_decimal(rho, 3),
    p_fmt = format_p(p),
    sig = p <= 0.05,
    row_type = "measure",
    row_label = as.character(variable),
    row_order = as.integer(variable)
  ) %>%
  arrange(section, variable) %>%
  dplyr::select(section, row_type, row_label, row_order, rho_fmt, p_fmt, sig)

headers <- cor_df %>%
  distinct(section) %>%
  mutate(
    row_type = "section",
    row_label = "\u00a0",
    row_order = 0L,
    rho_fmt = "\u00a0",
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
    rho_fmt = "Spearman rho",
    p_fmt = md("*p*")
  ) %>%
  cols_align("center", columns = c(rho_fmt, p_fmt)) %>%
  cols_align("left", columns = row_label) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = sig, columns = c(rho_fmt, p_fmt))
  ) %>%
  tab_style(
    style = cell_text(indent = px(30)),
    locations = cells_body(rows = row_type == "measure", columns = row_label)
  ) %>%
  tab_style(
    style = cell_text(size = px(18)),
    locations = cells_body(rows = row_type == "section")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = px(24),
    table.font.names = "Helvetica, Arial",
    data_row.padding = px(2),
    table.width = pct(58),
    heading.border.bottom.style = "solid",
    column_labels.border.top.style = "solid",
    column_labels.border.bottom.style = "solid",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    column_labels.padding = px(3)
  )

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(table_s25, "Plots/tables_s25.png", vwidth = 2200)
