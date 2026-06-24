#############################################
#
#       Table S26
#       ANES age moderation tests
#       Paths a, b, and c without mediation
#
#############################################

library(dplyr)
library(gt)

#############################################
# Load data

age_paths <- read.csv("anes_age_moderation_paths.csv")

age_paths <- age_paths %>%
  mutate(
    outcome_label = dplyr::recode(
      outcome_label,
      "Overall Wellbeing" = "Affective Wellbeing"
    )
  )

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

path_labels <- c(
  "a: political orientation -> social media use" = "Path a: Political Orientation X Social Media Use",
  "b: social media use -> wellbeing" = "Path b: Social Media Use X Wellbeing",
  "c: political orientation -> wellbeing" = "Path c: Political Orientation X Wellbeing"
)

path_order <- c(
  "Path c: Political Orientation X Wellbeing",
  "Path a: Political Orientation X Social Media Use",
  "Path b: Social Media Use X Wellbeing"
)

social_media_labels <- c(
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "youtube_frequency" = "YouTube",
  "twitter_frequency" = "Twitter"
)

#############################################
# Prepare table

path_c_rows <- age_paths %>%
  filter(path == "c: political orientation -> wellbeing") %>%
  mutate(
    path_label = dplyr::recode(path, !!!path_labels),
    path_label = factor(path_label, levels = path_order),
    row_type = "measure",
    row_label = outcome_label,
    est_ci = format_est_ci(
      interaction_est,
      interaction_ci_low,
      interaction_ci_high
    ),
    p_fmt = format_p(interaction_p),
    p_fdr_fmt = format_p(interaction_p_fdr)
  ) %>%
  arrange(outcome_label) %>%
  dplyr::select(
    path_label,
    row_type,
    row_label,
    est_ci,
    p_fmt,
    p_fdr_fmt
  )

path_a_rows <- age_paths %>%
  filter(path == "a: political orientation -> social media use") %>%
  mutate(
    path_label = dplyr::recode(path, !!!path_labels),
    path_label = factor(path_label, levels = path_order),
    row_type = "measure",
    row_label = dplyr::recode(outcome, !!!social_media_labels),
    est_ci = format_est_ci(
      interaction_est,
      interaction_ci_low,
      interaction_ci_high
    ),
    p_fmt = format_p(interaction_p),
    p_fdr_fmt = format_p(interaction_p_fdr)
  ) %>%
  arrange(row_label) %>%
  dplyr::select(
    path_label,
    row_type,
    row_label,
    est_ci,
    p_fmt,
    p_fdr_fmt
  )

path_b_data <- age_paths %>%
  filter(path == "b: social media use -> wellbeing") %>%
  mutate(
    path_label = dplyr::recode(path, !!!path_labels),
    path_label = factor(path_label, levels = path_order),
    platform = dplyr::recode(predictor, !!!social_media_labels),
    platform = factor(
      platform,
      levels = c("Social Media", "Facebook", "Instagram", "Reddit", "TikTok", "Twitter", "YouTube")
    ),
    row_type = "measure",
    row_label = outcome_label,
    est_ci = format_est_ci(
      interaction_est,
      interaction_ci_low,
      interaction_ci_high
    ),
    p_fmt = format_p(interaction_p),
    p_fdr_fmt = format_p(interaction_p_fdr)
  )

path_b_headers <- path_b_data %>%
  distinct(path_label, platform) %>%
  mutate(
    row_type = "platform",
    row_label = as.character(platform),
    est_ci = "",
    p_fmt = "",
    p_fdr_fmt = ""
  )

path_b_rows <- bind_rows(
  path_b_headers,
  path_b_data %>% dplyr::select(path_label, platform, row_type, row_label, est_ci, p_fmt, p_fdr_fmt)
) %>%
  mutate(row_type = factor(row_type, levels = c("platform", "measure"))) %>%
  arrange(platform, row_type, row_label) %>%
  dplyr::select(path_label, row_type, row_label, est_ci, p_fmt, p_fdr_fmt)

table_df <- bind_rows(path_c_rows, path_a_rows, path_b_rows) %>%
  mutate(path_label = factor(path_label, levels = path_order))

#############################################
# Create gt table

table_s26 <- table_df %>%
  gt(groupname_col = "path_label") %>%
  cols_label(
    row_label = "",
    est_ci = "Interaction [95% CI]",
    p_fmt = md("*p*"),
    p_fdr_fmt = md("*p* FDR")
  ) %>%
  cols_align("center", columns = c(est_ci, p_fmt, p_fdr_fmt)) %>%
  cols_align("left", columns = row_label) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = row_type == "platform", columns = row_label)
  ) %>%
  tab_style(
    style = cell_text(indent = px(30)),
    locations = cells_body(rows = row_type == "measure" & path_label == "Path b: Social Media Use X Wellbeing", columns = row_label)
  ) %>%
  cols_hide(row_type) %>%
  tab_options(
    table.font.size = px(24),
    table.font.names = "Helvetica, Arial",
    data_row.padding = px(2),
    row_group.padding = px(4),
    table.width = pct(82),
    heading.border.bottom.style = "solid",
    column_labels.border.top.style = "solid",
    column_labels.border.bottom.style = "solid",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    row_group.border.top.style = "none",
    row_group.border.bottom.style = "none",
    column_labels.padding = px(3)
  )

#############################################
# Save table

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(table_s26, "Plots/tables_s26.png", vwidth = 2600)
