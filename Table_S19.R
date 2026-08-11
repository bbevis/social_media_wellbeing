library(dplyr)
library(gt)
library(purrr)
library(webshot2)
library(chromote)
library(survey)

source("supplementary_table_style.R")

#############################################
#       Table S19
#       ANES descriptive statistics: social media use

anes_w3 <- read.csv("anes_w3.csv")
anes_design <- svydesign(
  ids = ~1,
  weights = ~w3weight,
  data = anes_w3 %>% filter(!is.na(w3weight))
)

#############################################
# Define variables

mediator_vars <- c(
  "total_platform_frequency",
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency"
)

mediator_labels <- c(
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "youtube_frequency" = "YouTube",
  "twitter_frequency" = "Twitter"
)

mediator_order <- c(
  "Social Media",
  "Facebook",
  "Instagram",
  "Reddit",
  "TikTok",
  "YouTube",
  "Twitter"
)

#############################################
# Descriptive statistics

svy_mean_var <- function(var) {
  as.numeric(coef(svymean(as.formula(paste0("~", var)), anes_design, na.rm = TRUE)))
}

svy_sd_var <- function(var) {
  sqrt(as.numeric(coef(svyvar(as.formula(paste0("~", var)), anes_design, na.rm = TRUE))))
}

make_anes_desc <- function(df, vars, labels, id_col = "caseid") {
  map_dfr(vars, function(v) {
    df %>%
      summarise(
        Variable = labels[[v]],
        `N Participants` = n_distinct(.data[[id_col]][!is.na(.data[[v]])]),
        `N Observations` = sum(!is.na(.data[[v]])),
        Mean = svy_mean_var(v),
        SD = svy_sd_var(v),
        Min = suppressWarnings(min(.data[[v]], na.rm = TRUE)),
        Max = suppressWarnings(max(.data[[v]], na.rm = TRUE)),
        .groups = "drop"
      )
  })
}

anes_mediator_desc <- make_anes_desc(
  anes_w3,
  mediator_vars,
  mediator_labels
)

table_df <- bind_rows(
  anes_mediator_desc %>%
    filter(Variable == "Social Media") %>%
    mutate(Level = "ANES Wave 3", .before = 1),
  anes_mediator_desc %>%
    filter(Variable != "Social Media") %>%
    mutate(
      Variable = factor(Variable, levels = mediator_order),
      Level = "ANES Wave 3",
      .before = 1
    ) %>%
    arrange(Variable)
) %>%
  mutate(
    Variable_raw = as.character(Variable),
    Variable = factor(Variable_raw, levels = mediator_order)
  ) %>%
  arrange(Variable)

write.csv(
  table_df %>% select(-Variable_raw),
  "anes_w3_mediator_descriptives.csv",
  row.names = FALSE
)

#############################################
# Create gt table

tbl_anes_mediators <- table_df %>%
  gt() %>%
  tab_header(title = NULL, subtitle = NULL) %>%
  cols_hide(c("Level", "Variable_raw")) %>%
  cols_label(Variable = "Variable") %>%
  sub_missing(
    columns = c("N Participants", "N Observations", "Mean", "SD", "Min", "Max"),
    missing_text = ""
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(
      rows = Variable_raw == "",
      columns = "Variable"
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Variable_raw == "Social Media",
      columns = "Variable"
    )
  ) %>%
  fmt_number(columns = c("Mean", "SD", "Min", "Max"), decimals = 2) %>%
  fmt_number(columns = c("N Participants", "N Observations"), decimals = 0, use_seps = TRUE) %>%
  cols_align("center", columns = everything()) %>%
  cols_align("left", columns = "Variable") %>%
  apply_supplementary_table_style()

gtsave(
  tbl_anes_mediators,
  "Plots/tables_s19.png",
  vwidth = SUPPLEMENTARY_TABLE_VWIDTH,
  vheight = 0
)
