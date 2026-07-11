#############################################
#
#       Table S16
#       Weekly 40% EMA threshold mediation
#       Social media use between political orientation and wellbeing
#
#############################################

library(dplyr)
library(gt)
library(tidyr)
library(webshot2)
library(magick)

#############################################
# Load data

weekly_nc_40 <- read.csv("weekly_results_nocontrols_40ema.csv")

platforms <- c(
  "total_platform",
  "facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat"
)

#############################################
# Prepare data

outcome_order <- c(
  "Affective Wellbeing",
  "Life Satisfaction",
  "Accepted",
  "Angry (r)",
  "Happy",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

mediator_order <- c(
  "Social Media",
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)

h4_df <- weekly_nc_40 %>%
  dplyr::select(mediator, outcome, ACME, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
  filter(mediator %in% platforms) %>%
  group_by(mediator, outcome) %>%
  summarise(
    m = mean(ACME),
    lwr = mean(ACME_CI_lower, na.rm = TRUE),
    upr = mean(ACME_CI_upper, na.rm = TRUE),
    p = mean(prop_mediated),
    .groups = "drop"
  ) %>%
  mutate(
    sig = !(lwr < 0 & upr > 0) & is.finite(lwr) & is.finite(upr),
    mediator_raw = mediator,
    mediator = case_when(
      mediator == "total_platform" ~ "Social Media",
      TRUE ~ mediator
    ),
    outcome = dplyr::recode(
      outcome,
      "awb_week" = "Affective Wellbeing",
      "swls_t2" = "Life Satisfaction",
      "uls_t2" = "Lonely (r)",
      "angry_rev" = "Angry (r)",
      "worried_rev" = "Worried (r)",
      "happy" = "Happy",
      "sad_rev" = "Sad (r)",
      "stressed_rev" = "Stressed (r)",
      "lonely_rev" = "Lonely (r)",
      "accepted" = "Accepted"
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(mediator, "Tiktok" = "TikTok", "Youtube" = "YouTube"),
    mediator = factor(mediator, levels = mediator_order),
    outcome = factor(outcome, levels = outcome_order)
  )

#############################################
# Prepare data for gt table

h4_for_gt <- h4_df %>%
  mutate(
    est_ci = sprintf("%.3f [%.3f, %.3f]", m, lwr, upr),
    p_fmt = sprintf("%.2f%%", p * 100)
  ) %>%
  dplyr::select(mediator_raw, mediator, outcome, est_ci, p_fmt, sig) %>%
  mutate(
    across(
      where(is.character),
      ~ gsub("(^|\\s|-|\\[|,)0\\.", "\\1.", .x, perl = TRUE)
    )
  ) %>%
  arrange(mediator, outcome)

#############################################
# Define composite vs single outcomes

composite_outcomes <- c(
  "Affective Wellbeing",
  "Life Satisfaction"
)

single_outcomes <- c(
  "Accepted", "Happy"
)

reverse_coded_single_outcomes <- c(
  "Angry (r)", "Lonely (r)", "Sad (r)", "Stressed (r)", "Worried (r)"
)

h4_left <- h4_for_gt %>% filter(outcome %in% composite_outcomes)
h4_middle <- h4_for_gt %>% filter(outcome %in% single_outcomes)
h4_right <- h4_for_gt %>% filter(outcome %in% reverse_coded_single_outcomes)

#############################################
# Create gt table

make_h4_table <- function(df_input) {
  df_input %>%
    mutate(
      mediator = factor(mediator, levels = mediator_order),
      outcome = factor(outcome, levels = outcome_order)
    ) %>%
    arrange(mediator, outcome) %>%
    {
      base <- .

      headers <- base %>%
        distinct(mediator) %>%
        mutate(
          outcome = NA,
          est_ci = "",
          p_fmt = "",
          sig = FALSE,
          row_type = "mediator"
        )

      rows <- base %>%
        mutate(row_type = "outcome")

      bind_rows(headers, rows)
    } %>%
    arrange(mediator, row_type, outcome) %>%
    mutate(
      stub = ifelse(
        row_type == "mediator",
        as.character(mediator),
        as.character(outcome)
      ),
      est_ci = ifelse(row_type == "mediator", "", est_ci),
      p_fmt = ifelse(row_type == "mediator", "", p_fmt),
      sig = ifelse(row_type == "mediator", FALSE, sig)
    ) %>%
    dplyr::select(mediator, outcome, row_type, stub, est_ci, p_fmt, sig) %>%
    gt(rowname_col = "stub") %>%
    cols_hide(c(mediator, outcome, row_type, sig)) %>%
    cols_label(
      est_ci = "ACME [95% CI]",
      p_fmt = "Prop. mediated"
    ) %>%
    tab_spanner(
      label = "Without Covariates",
      columns = c(est_ci, p_fmt)
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
      style = cell_text(weight = "bold", indent = px(20)),
      locations = cells_stub(rows = row_type == "mediator")
    ) %>%
    tab_style(
      style = cell_text(indent = px(40)),
      locations = cells_stub(rows = row_type == "outcome")
    ) %>%
    tab_options(
      table.font.size = px(25),
      table.font.names = "Helvetica, Arial",
      data_row.padding = px(4),
      table.width = pct(45),
      heading.border.bottom.style = "solid",
      column_labels.border.top.style = "solid",
      column_labels.border.bottom.style = "solid",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table_body.hlines.style = "none",
      table_body.vlines.style = "none",
      stub.border.style = "none"
    )
}

#############################################
# Build and save table panels

h4_gt_left <- make_h4_table(h4_left) %>%
  tab_header(title = md("**A. Composite Wellbeing Measures**"))

h4_gt_middle <- make_h4_table(h4_middle) %>%
  tab_header(title = md("**B. Single-Item Wellbeing Measures**"))

h4_gt_right <- make_h4_table(h4_right) %>%
  tab_header(title = md("**C. Reverse-Coded Single-Item Measures**"))

gtsave(h4_gt_left, "Plots/table_s16_A.png", vwidth = 2400)
gtsave(h4_gt_middle, "Plots/table_s16_B.png", vwidth = 2400)
gtsave(h4_gt_right, "Plots/table_s16_C.png", vwidth = 2400)

img_A <- image_read("Plots/table_s16_A.png")
img_B <- image_read("Plots/table_s16_B.png")
img_C <- image_read("Plots/table_s16_C.png")

combined_img <- image_append(c(img_A, img_B, img_C), stack = FALSE)

image_write(combined_img, "Plots/tables_s16.png")
