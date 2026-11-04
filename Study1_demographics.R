#############################################
# Study 1: demographics and participation
#############################################

# Reproduces the Study 1 Procedure and Participants summaries and the
# political-orientation comparison in the Participants footnote.
# Inputs:
#   Data/demographics_fall_reduced.csv
#   Data/demographics_spring_reduced.csv
#
# Outputs:
#   sample_description_metrics.csv: participation counts, report averages,
#     demographic characteristics, and the number of distinct reported majors.
#   political_orientation_liberal_conservative.csv: political-group counts
#     and percentages for the demographic summary sample.
#   political_orientation_attrition_metrics.csv: group means, SDs, and tests
#     comparing participants with follow-up data against those without.
#
# Survey-only rows have missing demographics and has_demographics = 0.
# They contribute to survey participation counts, not demographic summaries.
# Report totals refer to each survey's own participants, not necessarily
# subsets of those who completed the demographic survey.

library(dplyr)

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

DEMOGRAPHIC_LEVELS <- list(
  gender = c("male", "female", "other"),
  ses = c("lower", "lower middle", "middle", "upper middle", "upper"),
  smartphone = c("iPhone", "Android", "Non-iOS/Android")
)

term_ids <- function(fall, spring, extract) {
  ids <- list(fall = extract(fall), spring = extract(spring))
  c(ids, list(total = union(ids$fall, ids$spring)))
}

pct_of <- function(numerator, denominator) {
  round(100 * numerator / denominator, 1)
}

one_way_pct <- function(df, var, levels) {
  counts <- table(factor(df[[var]], levels = levels))
  round(100 * counts / sum(counts), 1)
}

multi_select_pct <- function(x) {
  round(100 * mean(x, na.rm = TRUE), 1)
}

has_ethnicity_code <- function(x, code) {
  values <- trimws(as.character(x))
  grepl(paste0("(^|,)", code, "(,|$)"), values)
}

ethnicity_indicators <- function(x) {
  tibble(
    ethnicity_african_american_black = has_ethnicity_code(x, "1"),
    ethnicity_asian_asian_american = has_ethnicity_code(x, "2"),
    ethnicity_hispanic_latino = has_ethnicity_code(x, "3"),
    ethnicity_anglo_white = has_ethnicity_code(x, "4"),
    ethnicity_native_american = has_ethnicity_code(x, "5"),
    ethnicity_pacific_islander = has_ethnicity_code(x, "6"),
    ethnicity_other = has_ethnicity_code(x, "7")
  )
}

smartphone_pct <- function(df, level) {
  one_way_pct(df, "smartphone", DEMOGRAPHIC_LEVELS$smartphone)[[level]]
}

gender_pct <- function(df, level) {
  one_way_pct(df, "gender", DEMOGRAPHIC_LEVELS$gender)[[level]]
}

ses_pct <- function(df, level) {
  one_way_pct(df, "ses", DEMOGRAPHIC_LEVELS$ses)[[level]]
}

metric_row <- function(metric, value) {
  tibble(metric = metric, value = value)
}


# -----------------------------------------------------------------------------
# Inputs: load the two reduced semester files
# -----------------------------------------------------------------------------

# The two survey metadata rows are retained in each reduced demographics file
# and removed here exactly as in the original script.
fall_input <- read.csv("Data/demographics_fall_reduced.csv") %>%
  slice(-(1:2))
spring_input <- read.csv("Data/demographics_spring_reduced.csv") %>%
  slice(-(1:2))

# Keep the original demographic sample separate from survey-only records.
demo_fall <- fall_input %>% filter(has_demographics == 1)
demo_spring <- spring_input %>% filter(has_demographics == 1)


# -----------------------------------------------------------------------------
# Preparation: identify participation at each of the four study steps
# -----------------------------------------------------------------------------

demographics_completed <- term_ids(
  demo_fall, demo_spring,
  function(df) unique(as.character(df$pID[df$Finished == 1]))
)
# Participation columns were calculated from the original survey records before
# exclusions. Record-presence flags preserve the original count denominators;
# response flags distinguish a submitted response from an otherwise empty record.
participation_columns <- c(
  "has_ema_record", "provided_ema", "n_ema_reports",
  "has_appusage_record", "provided_app_response", "provided_app_duration",
  "n_appusage_reports", "has_postsurvey_record", "provided_postsurvey"
)
# Participation totals include survey-only records; demographic summaries do not.
participation_fall <- fall_input %>%
  transmute(id = as.character(pID), semester = "fall",
            across(all_of(participation_columns)))
participation_spring <- spring_input %>%
  transmute(id = as.character(pID), semester = "spring",
            across(all_of(participation_columns)))
participation_all <- bind_rows(participation_fall, participation_spring)

ema_completed <- term_ids(
  participation_fall, participation_spring,
  function(df) unique(df$id[df$provided_ema == 1])
)
app_response <- term_ids(
  participation_fall, participation_spring,
  function(df) unique(df$id[df$provided_app_response == 1])
)
app_platform_duration <- term_ids(
  participation_fall, participation_spring,
  function(df) unique(df$id[df$provided_app_duration == 1])
)
post_completed <- term_ids(
  participation_fall, participation_spring,
  function(df) unique(df$id[df$provided_postsurvey == 1])
)

# The Participants paragraph describes demographic completers who supplied
# EMAs or at least one social-media app duration (1,725 participants).
ema_ids <- ema_completed$total
analysis_ids <- intersect(
  demographics_completed$total,
  union(ema_completed$total, app_platform_duration$total)
)

ema_counts <- participation_all %>%
  filter(has_ema_record == 1) %>%
  transmute(id, num_ema_completed = n_ema_reports)

appusage_counts <- participation_all %>%
  filter(has_appusage_record == 1) %>%
  transmute(id, n_appusage_surveys_completed = n_appusage_reports)


# -----------------------------------------------------------------------------
# Preparation: demographic variables and summary sample
# -----------------------------------------------------------------------------

demographics_df <- bind_rows(demo_fall, demo_spring) %>%
  bind_cols(
    ethnicity_indicators(bind_rows(demo_fall, demo_spring)$Q7)
  ) %>%
  transmute(
    id = as.character(pID),
    age = suppressWarnings(as.numeric(Q24)),
    gender = case_when(
      Q1 == "1" ~ "male",
      Q1 == "2" ~ "female",
      Q1 == "3" ~ "other",
      TRUE ~ NA_character_
    ),
    ses = case_when(
      Q27 == "1" ~ "lower",
      Q27 == "2" ~ "lower middle",
      Q27 == "3" ~ "middle",
      Q27 == "4" ~ "upper middle",
      Q27 == "5" ~ "upper",
      TRUE ~ NA_character_
    ),
    ethnicity_african_american_black,
    ethnicity_asian_asian_american,
    ethnicity_hispanic_latino,
    ethnicity_anglo_white,
    ethnicity_native_american,
    ethnicity_pacific_islander,
    ethnicity_other,
    smartphone = case_when(
      Q26 == "1" ~ "iPhone",
      Q26 == "2" ~ "Android",
      Q26 %in% c("3", "4", "5", "6") ~ "Non-iOS/Android",
      TRUE ~ NA_character_
    ),
    political_orientation = if_else(Q12 %in% as.character(1:7), 8 - as.numeric(Q12), NA_real_),
    political_group = factor(
      case_when(
        political_orientation %in% 1:3 ~ "Liberal",
        political_orientation == 4 ~ "Moderate",
        political_orientation %in% 5:7 ~ "Conservative",
        TRUE ~ NA_character_
      ),
      levels = c("Liberal", "Moderate", "Conservative")
    ),
    # Anonymous codes preserve the number of distinct trimmed major responses
    # without retaining free text. The distinct-major calculation below is unchanged.
    major = as.character(major_group)
  )

# Use this same 1,725-person group for demographic and political-group summaries.
# Age summaries exclude missing ages; the remaining calculations retain their
# existing variable-specific handling of missing values.
analysis_demo <- demographics_df %>% filter(id %in% analysis_ids)


# -----------------------------------------------------------------------------
# 1. Procedure: average completed EMA and Screentime Reports
# -----------------------------------------------------------------------------

procedure_summary <- bind_rows(
  metric_row("ema_reports_per_participant_mean", round(mean(ema_counts$num_ema_completed, na.rm = TRUE), 2)),
  metric_row("appusage_reports_per_participant_mean", round(mean(appusage_counts$n_appusage_surveys_completed, na.rm = TRUE), 2))
)

# -----------------------------------------------------------------------------
# 2. Participants: participation counts
# -----------------------------------------------------------------------------

participant_counts <- bind_rows(
  metric_row("demographics_total", length(demographics_completed$total)),
  metric_row("demographics_fall", length(demographics_completed$fall)),
  metric_row("demographics_spring", length(demographics_completed$spring)),
  metric_row("ema_total", length(ema_completed$total)),
  metric_row("app_response_total", length(app_response$total)),
  metric_row("post_total", length(post_completed$total))
)

# -----------------------------------------------------------------------------
# Participants: age and gender
# -----------------------------------------------------------------------------

age_gender_summary <- bind_rows(
  metric_row("analysis_age_mean", round(mean(analysis_demo$age, na.rm = TRUE), 2)),
  metric_row("analysis_age_sd", round(sd(analysis_demo$age, na.rm = TRUE), 2)),
  metric_row("analysis_pct_male", gender_pct(analysis_demo, "male")),
  metric_row("analysis_pct_female", gender_pct(analysis_demo, "female")),
  metric_row("analysis_pct_other_gender", gender_pct(analysis_demo, "other"))
)

# -----------------------------------------------------------------------------
# Participants: liberal, moderate, and conservative percentages
# -----------------------------------------------------------------------------

political_group_summary <- analysis_demo %>%
  filter(!is.na(political_group)) %>%
  count(political_group) %>%
  mutate(percent = round(100 * n / sum(n), 1))

# -----------------------------------------------------------------------------
# Participants: ethnicity (multiple selections allowed)
# -----------------------------------------------------------------------------

ethnicity_summary <- bind_rows(
  metric_row("analysis_pct_anglo_white", multi_select_pct(analysis_demo$ethnicity_anglo_white)),
  metric_row("analysis_pct_asian_asian_american", multi_select_pct(analysis_demo$ethnicity_asian_asian_american)),
  metric_row("analysis_pct_hispanic_latino", multi_select_pct(analysis_demo$ethnicity_hispanic_latino)),
  metric_row("analysis_pct_african_american_black", multi_select_pct(analysis_demo$ethnicity_african_american_black)),
  metric_row("analysis_pct_native_american", multi_select_pct(analysis_demo$ethnicity_native_american)),
  metric_row("analysis_pct_pacific_islander", multi_select_pct(analysis_demo$ethnicity_pacific_islander)),
  metric_row("analysis_pct_other_ethnicity", multi_select_pct(analysis_demo$ethnicity_other))
)

# -----------------------------------------------------------------------------
# Participants: socioeconomic status
# -----------------------------------------------------------------------------

ses_summary <- bind_rows(
  metric_row("analysis_pct_lower", ses_pct(analysis_demo, "lower")),
  metric_row("analysis_pct_lower_middle", ses_pct(analysis_demo, "lower middle")),
  metric_row("analysis_pct_middle", ses_pct(analysis_demo, "middle")),
  metric_row("analysis_pct_upper_middle", ses_pct(analysis_demo, "upper middle")),
  metric_row("analysis_pct_upper", ses_pct(analysis_demo, "upper"))
)

# -----------------------------------------------------------------------------
# Participants: smartphone type
# -----------------------------------------------------------------------------

smartphone_summary <- bind_rows(
  metric_row("smartphone_pct_iphone", smartphone_pct(analysis_demo, "iPhone")),
  metric_row("smartphone_pct_android", smartphone_pct(analysis_demo, "Android")),
  metric_row("smartphone_pct_non_ios_android", smartphone_pct(analysis_demo, "Non-iOS/Android"))
)

# -----------------------------------------------------------------------------
# 3. Participants, footnote 4: political orientation and follow-up participation
# -----------------------------------------------------------------------------

# Compare demographic completers with EMAs or an app duration (1,725)
# against those with neither (413).
attrition_po <- demographics_df %>%
  filter(id %in% demographics_completed$total, !is.na(political_orientation)) %>%
  mutate(with_followup = id %in% analysis_ids)

po_ttest <- t.test(political_orientation ~ with_followup, data = attrition_po)

followup_po <- attrition_po$political_orientation[attrition_po$with_followup]
no_followup_po <- attrition_po$political_orientation[!attrition_po$with_followup]

footnote_summary <- tibble(
    metric = c(
      "demographics_po_total",
      "po_with_followup_n",
      "po_with_followup_pct",
      "po_without_followup_n",
      "po_without_followup_pct",
      "po_with_followup_mean",
      "po_with_followup_sd",
      "po_without_followup_mean",
      "po_without_followup_sd",
      "po_t_statistic",
      "po_t_df",
      "po_t_p_value"
    ),
    value = c(
      nrow(attrition_po),
      sum(attrition_po$with_followup),
      pct_of(sum(attrition_po$with_followup), nrow(attrition_po)),
      sum(!attrition_po$with_followup),
      pct_of(sum(!attrition_po$with_followup), nrow(attrition_po)),
      round(mean(followup_po), 2),
      round(sd(followup_po), 2),
      round(mean(no_followup_po), 2),
      round(sd(no_followup_po), 2),
      round(unname(po_ttest$statistic), 2),
      round(unname(po_ttest$parameter), 1),
      signif(po_ttest$p.value, 3)
    )
  )

# -----------------------------------------------------------------------------
# 4. Additional statistics not explicitly reported in the manuscript
# -----------------------------------------------------------------------------

# Retained supplementary counts and report-count SDs.
# -----------------------------------------------------------------------------
# Additional sample count: political orientation among post-survey records
# -----------------------------------------------------------------------------

post_ids <- unique(participation_all$id[participation_all$has_postsurvey_record == 1])

post_df <- demographics_df %>%
  filter(id %in% post_ids) %>%
  mutate(with_ema = id %in% ema_ids)

post_po <- post_df %>% filter(!is.na(political_orientation))

additional_sample_summary <- bind_rows(
  metric_row("ema_fall", length(ema_completed$fall)),
  metric_row("ema_spring", length(ema_completed$spring)),
  metric_row("app_response_fall", length(app_response$fall)),
  metric_row("app_response_spring", length(app_response$spring)),
  metric_row("app_platform_duration_total", length(app_platform_duration$total)),
  metric_row("app_platform_duration_fall", length(app_platform_duration$fall)),
  metric_row("app_platform_duration_spring", length(app_platform_duration$spring)),
  metric_row("post_fall", length(post_completed$fall)),
  metric_row("post_spring", length(post_completed$spring)),
  metric_row("post_political_orientation_total", nrow(post_po)),
  metric_row("analysis_total", nrow(analysis_demo)),
  metric_row("ema_reports_per_participant_sd", round(sd(ema_counts$num_ema_completed, na.rm = TRUE), 2)),
  metric_row("appusage_reports_per_participant_sd", round(sd(appusage_counts$n_appusage_surveys_completed, na.rm = TRUE), 2)),
  metric_row("n_unique_reported_majors", n_distinct(na.omit(trimws(analysis_demo$major))))
)

# Additional chi-square test and within-group political percentages.
po_chisq <- chisq.test(table(attrition_po$with_followup, attrition_po$political_group))

po_groups <- attrition_po %>%
  count(with_followup, political_group) %>%
  group_by(with_followup) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(group = if_else(with_followup, "Provided EMA or app duration", "Provided neither")) %>%
  dplyr::select(group, political_group, percent)

chi_square_summary <- tibble(
  metric = c("po_chisq_statistic", "po_chisq_df", "po_chisq_p_value"),
  value = c(
    round(unname(po_chisq$statistic), 2),
    unname(po_chisq$parameter),
    signif(po_chisq$p.value, 3)
  )
)

# -----------------------------------------------------------------------------
# Assemble and save the three summary outputs
# -----------------------------------------------------------------------------

sample_description <- bind_rows(
  procedure_summary, participant_counts, age_gender_summary, ethnicity_summary,
  ses_summary, smartphone_summary, additional_sample_summary
)

po_summary <- bind_rows(
  footnote_summary,
  chi_square_summary,
  po_groups %>%
    mutate(
      metric = paste0(
        "po_",
        if_else(group == "Provided EMA or app duration", "with_followup_", "without_followup_"),
        tolower(gsub(" ", "_", political_group)),
        "_pct"
      )
    ) %>%
    transmute(metric, value = percent)
)

print(sample_description)
print(political_group_summary)
print(po_summary)

write.csv(sample_description, "sample_description_metrics.csv", row.names = FALSE)
write.csv(political_group_summary, "political_orientation_liberal_conservative.csv", row.names = FALSE)
write.csv(po_summary, "political_orientation_attrition_metrics.csv", row.names = FALSE)
