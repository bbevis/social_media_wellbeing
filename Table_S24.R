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

anes_res <- read.csv("anes_w3_mediation_results_expanded_wellbeing.csv")

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
  "Depression (r)",
  "Affective Wellbeing",
  "Happy",
  "Hopeful",
  "Afraid (r)",
  "Angry (r)",
  "Outraged (r)",
  "Worried (r)"
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
    (sample == "all" & model_type == "mediation") |
      model_type %in% c("age_moderator", "age_moderation")
  ) %>%
  mutate(
    model = case_when(
      sample == "all" & model_type == "mediation" ~ "model_1",
      model_type %in% c("age_moderator", "age_moderation") ~ "model_2"
    ),
    outcome = dplyr::recode(
      outcome,
      "loneliness" = "Loneliness (r)",
      "anhedonia" = "Anhedonia (r)",
      "depression" = "Depression (r)",
      "affective_wellbeing" = "Affective Wellbeing",
      "happy" = "Happy",
      "hopeful" = "Hopeful",
      "afraid" = "Afraid (r)",
      "angry" = "Angry (r)",
      "outraged" = "Outraged (r)",
      "worried" = "Worried (r)"
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
    p_fmt = format_p(acme_p)
  ) %>%
  dplyr::select(mediator, outcome, model, est_ci, p_fmt) %>%
  pivot_wider(
    names_from = model,
    values_from = c(est_ci, p_fmt),
    names_sep = "_"
  ) %>%
  arrange(mediator, outcome)

#############################################
# Add platform header rows

headers <- table_df %>%
  distinct(mediator) %>%
  mutate(
    outcome = NA,
    est_ci_model_1 = "",
    p_fmt_model_1 = "",
    est_ci_model_2 = "",
    p_fmt_model_2 = "",
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
      c(est_ci_model_1, p_fmt_model_1, est_ci_model_2, p_fmt_model_2),
      ~ ifelse(is.na(.x), "", .x)
    )
  ) %>%
  dplyr::select(
    mediator,
    outcome,
    row_type,
    stub,
    est_ci_model_1,
    p_fmt_model_1,
    est_ci_model_2,
    p_fmt_model_2
  )

#############################################
# Create gt table

table_s24 <- table_for_gt %>%
  gt(rowname_col = "stub") %>%
  cols_hide(c(mediator, outcome, row_type)) %>%
  cols_label(
    est_ci_model_1 = "ACME [95% CI]",
    p_fmt_model_1 = md("*p*"),
    est_ci_model_2 = "ACME [95% CI]",
    p_fmt_model_2 = md("*p*")
  ) %>%
  tab_spanner(
    label = "Model 1",
    columns = c(est_ci_model_1, p_fmt_model_1)
  ) %>%
  tab_spanner(
    label = "Model 2",
    columns = c(est_ci_model_2, p_fmt_model_2)
  ) %>%
  cols_align("center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(spanners = c("Model 1", "Model 2"))
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
    data_row.padding = px(2),
    table.width = pct(62),
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

gtsave(table_s24, "Plots/tables_s24.png", vwidth = 2600)
