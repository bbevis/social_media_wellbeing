#############################################
#
#       ANES age moderation tables
#       Shared construction for Tables S26 and S27
#
#############################################

library(dplyr)
library(tidyr)
library(gt)

source("supplementary_table_style.R")

#############################################
# Load data

path_a_res <- read.csv("anes_path_a_age_moderation_results.csv")

path_b_res <- read.csv("anes_age_moderation_results.csv") %>%
  filter(
    path == "b",
    outcome %in% c("loneliness", "anhedonia", "depression")
  )

#############################################
# Formatting helpers

format_decimal <- function(x) {
  out <- sprintf("%.3f", x)
  gsub("(^|\\s|-|\\[|,)0\\.", "\\1.", out, perl = TRUE)
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < .001 ~ "<.001",
    TRUE ~ format_decimal(p)
  )
}

format_est_ci <- function(est, low, high) {
  paste0(
    format_decimal(est),
    " [",
    format_decimal(low),
    ", ",
    format_decimal(high),
    "]"
  )
}

#############################################
# Labels and ordering

mediator_labels <- c(
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "twitter_frequency" = "Twitter",
  "youtube_frequency" = "YouTube"
)

mediator_order <- unname(mediator_labels)

outcome_labels <- c(
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)"
)

outcome_order <- unname(outcome_labels)

path_order <- c("Path a", "Path b")

#############################################
# Combine path a and path b results

combined_results <- bind_rows(
  path_a_res %>%
    transmute(
      path = "Path a",
      mediator,
      outcome = NA_character_,
      statistic,
      age_level,
      estimate,
      p,
      ci_low,
      ci_high
    ),
  path_b_res %>%
    transmute(
      path = "Path b",
      mediator,
      outcome,
      statistic,
      age_level,
      estimate,
      p,
      ci_low,
      ci_high
    )
) %>%
  mutate(
    path = factor(path, levels = path_order),
    mediator = dplyr::recode(mediator, !!!mediator_labels),
    mediator = factor(mediator, levels = mediator_order),
    outcome = dplyr::recode(outcome, !!!outcome_labels),
    outcome = factor(outcome, levels = outcome_order),
    age_level = case_when(
      statistic == "interaction" ~ "interaction",
      age_level == "-1 SD" ~ "minus_1sd",
      age_level == "Mean age" ~ "mean",
      age_level == "+1 SD" ~ "plus_1sd"
    ),
    est_ci = format_est_ci(estimate, ci_low, ci_high),
    p_fmt = format_p(p),
    sig = !is.na(p) & p < .05
  ) %>%
  select(path, mediator, outcome, age_level, est_ci, p_fmt, sig) %>%
  pivot_wider(
    names_from = age_level,
    values_from = c(est_ci, p_fmt, sig),
    names_sep = "_"
  ) %>%
  arrange(path, mediator, outcome)

stopifnot(
  nrow(combined_results %>% filter(path == "Path a")) == 7,
  nrow(combined_results %>% filter(path == "Path b")) == 21
)

#############################################
# Build table rows

path_a_rows <- combined_results %>%
  filter(path == "Path a") %>%
  mutate(
    row_type = "mediator",
    stub = as.character(mediator)
  )

path_b_data_rows <- combined_results %>%
  filter(path == "Path b") %>%
  mutate(
    row_type = "outcome",
    stub = as.character(outcome)
  )

path_b_headers <- path_b_data_rows %>%
  group_by(path, mediator) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    outcome = factor(NA_character_, levels = outcome_order),
    across(starts_with(c("est_ci_", "p_fmt_")), ~ ""),
    across(starts_with("sig_"), ~ FALSE),
    row_type = "mediator",
    stub = as.character(mediator)
  )

table_for_gt <- bind_rows(
  path_a_rows,
  path_b_headers,
  path_b_data_rows
) %>%
  mutate(
    row_type = factor(row_type, levels = c("mediator", "outcome"))
  ) %>%
  arrange(path, mediator, row_type, outcome) %>%
  select(
    path,
    mediator,
    outcome,
    row_type,
    stub,
    est_ci_interaction,
    p_fmt_interaction,
    est_ci_minus_1sd,
    p_fmt_minus_1sd,
    est_ci_mean,
    p_fmt_mean,
    est_ci_plus_1sd,
    p_fmt_plus_1sd,
    starts_with("sig_")
  )

#############################################
# Create table

make_path_table <- function(table_data, predictor_label) {
  table_data %>%
    gt(rowname_col = "stub") %>%
    cols_hide(c(path, mediator, outcome, row_type, starts_with("sig_"))) %>%
    cols_label(
      est_ci_interaction = "Estimate [95% CI]",
      p_fmt_interaction = md("*p*"),
      est_ci_minus_1sd = "Estimate [95% CI]",
      p_fmt_minus_1sd = md("*p*"),
      est_ci_mean = "Estimate [95% CI]",
      p_fmt_mean = md("*p*"),
      est_ci_plus_1sd = "Estimate [95% CI]",
      p_fmt_plus_1sd = md("*p*")
    ) %>%
    tab_spanner(
      paste("Age ×", predictor_label),
      c(est_ci_interaction, p_fmt_interaction),
      id = "age_by_predictor"
    ) %>%
    tab_spanner(
      "−1 SD",
      c(est_ci_minus_1sd, p_fmt_minus_1sd),
      id = "minus_1sd"
    ) %>%
    tab_spanner(
      "Mean",
      c(est_ci_mean, p_fmt_mean),
      id = "mean_age"
    ) %>%
    tab_spanner(
      "+1 SD",
      c(est_ci_plus_1sd, p_fmt_plus_1sd),
      id = "plus_1sd"
    ) %>%
    tab_spanner(
      "Interaction effect",
      spanners = "age_by_predictor"
    ) %>%
    tab_spanner(
      paste("Simple effects of", predictor_label, "at age"),
      spanners = c("minus_1sd", "mean_age", "plus_1sd")
    ) %>%
    cols_align("center") %>%
    cols_width(
      stub ~ px(220),
      c(
        est_ci_interaction,
        est_ci_minus_1sd,
        est_ci_mean,
        est_ci_plus_1sd
      ) ~ px(235),
      c(
        p_fmt_interaction,
        p_fmt_minus_1sd,
        p_fmt_mean,
        p_fmt_plus_1sd
      ) ~ px(60)
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_column_labels(everything())
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_column_spanners(spanners = everything())
    ) %>%
    tab_style(
      cell_text(align = "left"),
      cells_stub()
    ) %>%
    tab_style(
      cell_text(indent = px(20)),
      cells_stub(rows = row_type == "mediator")
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_stub(
        rows = row_type == "mediator" &
          (path == "Path b" |
            (path == "Path a" & mediator == "Social Media"))
      )
    ) %>%
    tab_style(
      cell_text(indent = px(40)),
      cells_stub(rows = row_type == "outcome")
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_body(
        columns = c(est_ci_interaction, p_fmt_interaction),
        rows = sig_interaction
      )
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_body(
        columns = c(est_ci_minus_1sd, p_fmt_minus_1sd),
        rows = sig_minus_1sd
      )
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_body(
        columns = c(est_ci_mean, p_fmt_mean),
        rows = sig_mean
      )
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_body(
        columns = c(est_ci_plus_1sd, p_fmt_plus_1sd),
        rows = sig_plus_1sd
      )
    ) %>%
    apply_supplementary_table_style()
}

table_path_a <- table_for_gt %>%
  filter(path == "Path a") %>%
  make_path_table("Political Orientation")

table_path_b <- table_for_gt %>%
  filter(path == "Path b") %>%
  make_path_table("Social Media Use")
