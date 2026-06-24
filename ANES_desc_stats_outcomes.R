library(dplyr)
library(gt)
library(tidyr)
library(purrr)
library(psych)
library(webshot2)
library(chromote)

#############################################
#       Table S20
#       ANES descriptive statistics: wellbeing

anes_w3 <- read.csv("anes_w3.csv")

#############################################
# Define variables

symptom_outcomes <- c(
  "loneliness",
  "anhedonia",
  "depression"
)

affective_items <- c(
  "happy",
  "hopeful",
  "afraid",
  "angry",
  "outraged",
  "worried"
)

wellbeing_outcomes <- c(
  "affective_wellbeing",
  symptom_outcomes,
  affective_items
)

outcome_labels <- c(
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)",
  "affective_wellbeing" = "Overall Wellbeing",
  "happy" = "Happy",
  "hopeful" = "Hopeful",
  "afraid" = "Afraid (r)",
  "angry" = "Angry (r)",
  "outraged" = "Outraged (r)",
  "worried" = "Worried (r)"
)

#############################################
# Descriptive statistics

anes_wellbeing_desc <- map_dfr(wellbeing_outcomes, function(v) {
  anes_w3 %>%
    summarise(
      Variable = outcome_labels[[v]],
      `N Participants` = n_distinct(caseid[!is.na(.data[[v]])]),
      `N Observations` = sum(!is.na(.data[[v]])),
      Mean = mean(.data[[v]], na.rm = TRUE),
      SD = sd(.data[[v]], na.rm = TRUE),
      Min = suppressWarnings(min(.data[[v]], na.rm = TRUE)),
      Max = suppressWarnings(max(.data[[v]], na.rm = TRUE))
    )
})

#############################################
# Internal consistency for Overall Wellbeing

overall_wellbeing_alpha <- psych::alpha(
  anes_w3 %>% select(all_of(affective_items)),
  check.keys = FALSE,
  warnings = FALSE
)

overall_wellbeing_alpha_value <- overall_wellbeing_alpha$total$raw_alpha

#############################################
# Save and print

write.csv(
  anes_wellbeing_desc,
  "anes_w3_wellbeing_descriptives.csv",
  row.names = FALSE
)

#############################################
# Create gt table

outcome_order <- c(
  "Overall Wellbeing",
  "Loneliness (r)",
  "Anhedonia (r)",
  "Depression (r)",
  "Happy",
  "Hopeful",
  "Afraid (r)",
  "Angry (r)",
  "Outraged (r)",
  "Worried (r)"
)

table_df <- anes_wellbeing_desc %>%
  mutate(
    Level = "ANES Wave 3",
    Variable = factor(Variable, levels = outcome_order)
  ) %>%
  arrange(Variable) %>%
  relocate(Level, .before = 1)

tbl_anes_outcomes <- table_df %>%
  gt() %>%
  cols_hide("Level") %>%
  cols_label(
    Variable = "Variable",
    `N Participants` = md("**N Participants**"),
    `N Observations` = md("**N Observations**"),
    Mean = md("**Mean**"),
    SD = md("**SD**"),
    Min = md("**Min**"),
    Max = md("**Max**")
  ) %>%
  fmt_number(columns = c("Mean", "SD", "Min", "Max"), decimals = 2) %>%
  fmt_number(columns = c("N Participants", "N Observations"), decimals = 0, use_seps = TRUE) %>%
  tab_style(
    style = cell_text(size = px(18)),
    locations = cells_body()
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = "Variable") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.names = "Helvetica, Arial, sans-serif",
    heading.align = "left",
    data_row.padding = px(4),
    table.width = pct(50),
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    table.border.left.style = "none",
    table.border.right.style = "none",
    row_group.border.top.style = "none",
    row_group.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.hlines.color = "transparent",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "solid"
  )

gtsave(
  tbl_anes_outcomes,
  "Plots/tables_s20.png",
  vwidth = 1600,
  vheight = 0
)

print(anes_wellbeing_desc)

cat(
  "\nOverall Wellbeing Cronbach's alpha:",
  round(overall_wellbeing_alpha_value, 3),
  "\n"
)
