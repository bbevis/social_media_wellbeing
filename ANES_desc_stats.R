library(dplyr)
library(tidyr)
library(purrr)
library(survey)

#############################################
#
#       ANES descriptive statistics
#
#############################################

anes_w3 <- read.csv("anes_w3.csv") %>%
  mutate(
    political_group = factor(
      case_when(
        political_orientation %in% 1:3 ~ "Liberal",
        political_orientation == 4 ~ "Moderate",
        political_orientation %in% 5:7 ~ "Conservative",
        TRUE ~ NA_character_
      ),
      levels = c("Liberal", "Moderate", "Conservative")
    )
  )

#############################################
# Labels

gender_labels <- c(
  "1" = "Male",
  "2" = "Female"
)

race_ethnicity_labels <- c(
  "1" = "White, non-Hispanic",
  "2" = "Black, non-Hispanic",
  "3" = "Other, non-Hispanic",
  "4" = "Hispanic"
)

education_labels <- c(
  "1" = "Less than high school",
  "2" = "High school graduate or equivalent",
  "3" = "Vocational/technical school, some college, or associate degree",
  "4" = "Bachelor's degree",
  "5" = "Postgraduate or professional degree"
)

income_labels <- c(
  "1" = "Less than $5,000",
  "2" = "$5,000 to $9,999",
  "3" = "$10,000 to $14,999",
  "4" = "$15,000 to $19,999",
  "5" = "$20,000 to $24,999",
  "6" = "$25,000 to $29,999",
  "7" = "$30,000 to $34,999",
  "8" = "$35,000 to $39,999",
  "9" = "$40,000 to $49,999",
  "10" = "$50,000 to $59,999",
  "11" = "$60,000 to $74,999",
  "12" = "$75,000 to $84,999",
  "13" = "$85,000 to $99,999",
  "14" = "$100,000 to $124,999",
  "15" = "$125,000 to $149,999",
  "16" = "$150,000 to $174,999",
  "17" = "$175,000 to $199,999",
  "18" = "$200,000 or more"
)

political_group_labels <- c(
  "Liberal" = "Liberal",
  "Moderate" = "Moderate",
  "Conservative" = "Conservative"
)

#############################################
# Helpers

pct <- function(x, denominator) {
  round(100 * x / denominator, 1)
}

svy_mean_var <- function(df, var) {
  df <- df %>% filter(!is.na(w3weight))
  design <- svydesign(ids = ~1, weights = ~w3weight, data = df)
  as.numeric(coef(svymean(as.formula(paste0("~", var)), design, na.rm = TRUE)))
}

svy_sd_var <- function(df, var) {
  df <- df %>% filter(!is.na(w3weight))
  design <- svydesign(ids = ~1, weights = ~w3weight, data = df)
  sqrt(as.numeric(coef(svyvar(as.formula(paste0("~", var)), design, na.rm = TRUE))))
}

format_category_table <- function(df, var, labels, sample_label) {
  design_df <- df %>%
    filter(!is.na(w3weight)) %>%
    mutate(category_var = .data[[var]])
  design <- svydesign(ids = ~1, weights = ~w3weight, data = design_df)
  percent_df <- svymean(~factor(category_var), design, na.rm = TRUE) %>%
    coef() %>%
    {\(x) tibble(
      category_value = sub("^factor\\(category_var\\)", "", names(x)),
      percent = round(100 * as.numeric(x), 1)
    )}()

  df %>%
    count(.data[[var]], name = "n") %>%
    filter(!is.na(.data[[var]])) %>%
    mutate(
      sample = sample_label,
      variable = var,
      category_value = as.character(.data[[var]]),
      category = recode(category_value, !!!labels)
    ) %>%
    left_join(percent_df, by = "category_value") %>%
    select(sample, variable, category, n, percent)
}

describe_sample <- function(df, sample_label) {
  tibble(
    sample = sample_label,
    n = nrow(df),
    n_unique_caseid = n_distinct(df$caseid),
    age_mean = svy_mean_var(df, "age"),
    age_sd = svy_sd_var(df, "age"),
    age_min = min(df$age, na.rm = TRUE),
    age_max = max(df$age, na.rm = TRUE),
    n_young_adult = sum(df$young_adult == 1, na.rm = TRUE),
    pct_young_adult = round(100 * svy_mean_var(df, "young_adult"), 1),
    n_valid_political_orientation = sum(!is.na(df$political_orientation)),
    political_orientation_mean = svy_mean_var(df, "political_orientation"),
    political_orientation_sd = svy_sd_var(df, "political_orientation")
  )
}

#############################################
# Samples

all_w3 <- anes_w3 %>%
  filter(!is.na(w3weight))
valid_political_orientation <- all_w3 %>%
  filter(!is.na(political_orientation))

#############################################
# Descriptive statistics

sample_summary <- bind_rows(
  describe_sample(all_w3, "All Wave 3 respondents"),
  describe_sample(valid_political_orientation, "Valid political orientation")
)

demographic_breakdown <- bind_rows(
  format_category_table(all_w3, "gender", gender_labels, "All Wave 3 respondents"),
  format_category_table(all_w3, "race_ethnicity", race_ethnicity_labels, "All Wave 3 respondents"),
  format_category_table(all_w3, "education", education_labels, "All Wave 3 respondents"),
  format_category_table(all_w3, "income", income_labels, "All Wave 3 respondents"),
  format_category_table(valid_political_orientation, "gender", gender_labels, "Valid political orientation"),
  format_category_table(valid_political_orientation, "race_ethnicity", race_ethnicity_labels, "Valid political orientation"),
  format_category_table(valid_political_orientation, "education", education_labels, "Valid political orientation"),
  format_category_table(valid_political_orientation, "income", income_labels, "Valid political orientation"),
  format_category_table(valid_political_orientation, "political_group", political_group_labels, "Valid political orientation")
)

#############################################
# Save and print

write.csv(sample_summary, "anes_w3_sample_summary.csv", row.names = FALSE)
write.csv(demographic_breakdown, "anes_w3_demographic_breakdown.csv", row.names = FALSE)

print(sample_summary)
print(demographic_breakdown)
