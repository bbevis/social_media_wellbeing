library(dplyr)

DEMOGRAPHIC_LEVELS <- list(
  gender = c("male", "female", "other"),
  ses = c("lower", "lower middle", "middle", "upper middle", "upper"),
  smartphone = c("iPhone", "Android", "Non-iOS/Android")
)

completed_ids <- function(df) {
  answered <- rowSums(!is.na(df[setdiff(names(df), "id")])) > 0
  unique(as.character(df$id[answered]))
}

platform_duration_ids <- function(df) {
  vars <- grep("^(hours|minutes)_(facebook|twitter|tiktok|instagram|youtube|reddit|snapchat)_w[1-4]$", names(df), value = TRUE)
  unique(as.character(df$id[rowSums(!is.na(df[vars])) > 0]))
}

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

count_completed_appusage_reports <- function(df) {
  platform_vars <- c("facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat")
  weekly_counts <- lapply(1:4, function(w) {
    hours_cols <- paste0("hours_", platform_vars, "_w", w)
    minutes_cols <- paste0("minutes_", platform_vars, "_w", w)

    has_platform_data <- rowSums(
      !is.na(df[, intersect(hours_cols, names(df)), drop = FALSE]) |
        !is.na(df[, intersect(minutes_cols, names(df)), drop = FALSE])
    ) > 0

    as.integer(has_platform_data)
  })

  rowSums(do.call(cbind, weekly_counts))
}


# -----------------------------------------------------------------------------
# Load raw files
# -----------------------------------------------------------------------------

pre_1  <- read.csv("Data/coco_ut1_presurvey.csv", sep = ";")
pre_2  <- read.csv("Data/coco_ut2_presurvey.csv", sep = ";")
post_1 <- read.csv("Data/coco_ut1_postsurvey.csv", sep = ";")
post_2 <- read.csv("Data/coco_ut2_postsurvey.csv", sep = ";")
ema_1  <- read.csv("Data/coco_ut1_ema_survey.csv", sep = ";")
ema_2  <- read.csv("Data/coco_ut2_ema_survey.csv", sep = ";")
app_1  <- read.csv("Data/coco_ut1_appusage_survey.csv", sep = ";")
app_2  <- read.csv("Data/coco_ut2_appusage_survey.csv", sep = ";")

demo_fall <- read.csv("Data/demographics_fall_anonymized.csv") %>%
  slice(-(1:2))
demo_spring <- read.csv("Data/demographics_spring_anonymized.csv") %>%
  slice(-(1:2))


# -----------------------------------------------------------------------------
# Core ID sets
# -----------------------------------------------------------------------------

demographics_completed <- term_ids(
  demo_fall, demo_spring,
  function(df) unique(as.character(df$pID[df$Finished == 1]))
)
ema_completed <- term_ids(ema_1, ema_2, completed_ids)
app_response <- term_ids(app_1, app_2, completed_ids)
app_platform_duration <- term_ids(app_1, app_2, platform_duration_ids)
post_completed <- term_ids(post_1, post_2, completed_ids)

ema_ids <- ema_completed$total
analysis_ids <- intersect(
  demographics_completed$total,
  union(ema_completed$total, app_platform_duration$total)
)

ema_counts <- bind_rows(ema_1, ema_2) %>%
  count(id, name = "num_ema_completed")

appusage_combined <- bind_rows(app_1, app_2)

appusage_counts <- tibble(
  id = as.character(appusage_combined$id),
  n_appusage_surveys_completed = count_completed_appusage_reports(appusage_combined)
)


# -----------------------------------------------------------------------------
# Demographics from demographics_fall/spring_anonymized
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
    major = coalesce(
      na_if(as.character(Q46_5_TEXT), ""),
      na_if(as.character(Q46), "")
    )
  )

analysis_demo <- demographics_df %>% filter(id %in% analysis_ids)


# -----------------------------------------------------------------------------
# Political orientation from demographics, restricted to post-survey completers
# -----------------------------------------------------------------------------

post_ids <- unique(as.character(c(post_1$id, post_2$id)))

post_df <- demographics_df %>%
  filter(id %in% post_ids) %>%
  mutate(with_ema = id %in% ema_ids)

post_po <- post_df %>% filter(!is.na(political_orientation))


# -----------------------------------------------------------------------------
# Sample description metrics
# -----------------------------------------------------------------------------

sample_description <- bind_rows(
  metric_row("demographics_total", length(demographics_completed$total)),
  metric_row("demographics_fall", length(demographics_completed$fall)),
  metric_row("demographics_spring", length(demographics_completed$spring)),
  metric_row("ema_total", length(ema_completed$total)),
  metric_row("ema_fall", length(ema_completed$fall)),
  metric_row("ema_spring", length(ema_completed$spring)),
  metric_row("app_response_total", length(app_response$total)),
  metric_row("app_response_fall", length(app_response$fall)),
  metric_row("app_response_spring", length(app_response$spring)),
  metric_row("app_platform_duration_total", length(app_platform_duration$total)),
  metric_row("app_platform_duration_fall", length(app_platform_duration$fall)),
  metric_row("app_platform_duration_spring", length(app_platform_duration$spring)),
  metric_row("post_total", length(post_completed$total)),
  metric_row("post_fall", length(post_completed$fall)),
  metric_row("post_spring", length(post_completed$spring)),
  metric_row("post_political_orientation_total", nrow(post_po)),
  metric_row("analysis_total", nrow(analysis_demo)),
  metric_row("analysis_age_mean", round(mean(analysis_demo$age, na.rm = TRUE), 2)),
  metric_row("analysis_age_sd", round(sd(analysis_demo$age, na.rm = TRUE), 2)),
  metric_row("analysis_pct_male", gender_pct(analysis_demo, "male")),
  metric_row("analysis_pct_female", gender_pct(analysis_demo, "female")),
  metric_row("analysis_pct_other_gender", gender_pct(analysis_demo, "other")),
  metric_row("analysis_pct_lower", ses_pct(analysis_demo, "lower")),
  metric_row("analysis_pct_lower_middle", ses_pct(analysis_demo, "lower middle")),
  metric_row("analysis_pct_middle", ses_pct(analysis_demo, "middle")),
  metric_row("analysis_pct_upper_middle", ses_pct(analysis_demo, "upper middle")),
  metric_row("analysis_pct_upper", ses_pct(analysis_demo, "upper")),
  metric_row("analysis_pct_african_american_black", multi_select_pct(analysis_demo$ethnicity_african_american_black)),
  metric_row("analysis_pct_asian_asian_american", multi_select_pct(analysis_demo$ethnicity_asian_asian_american)),
  metric_row("analysis_pct_hispanic_latino", multi_select_pct(analysis_demo$ethnicity_hispanic_latino)),
  metric_row("analysis_pct_anglo_white", multi_select_pct(analysis_demo$ethnicity_anglo_white)),
  metric_row("analysis_pct_native_american", multi_select_pct(analysis_demo$ethnicity_native_american)),
  metric_row("analysis_pct_pacific_islander", multi_select_pct(analysis_demo$ethnicity_pacific_islander)),
  metric_row("analysis_pct_other_ethnicity", multi_select_pct(analysis_demo$ethnicity_other)),
  metric_row("smartphone_pct_iphone", smartphone_pct(analysis_demo, "iPhone")),
  metric_row("smartphone_pct_android", smartphone_pct(analysis_demo, "Android")),
  metric_row("smartphone_pct_non_ios_android", smartphone_pct(analysis_demo, "Non-iOS/Android")),
  metric_row("ema_reports_per_participant_mean", round(mean(ema_counts$num_ema_completed, na.rm = TRUE), 2)),
  metric_row("ema_reports_per_participant_sd", round(sd(ema_counts$num_ema_completed, na.rm = TRUE), 2)),
  metric_row("appusage_reports_per_participant_mean", round(mean(appusage_counts$n_appusage_surveys_completed, na.rm = TRUE), 2)),
  metric_row("appusage_reports_per_participant_sd", round(sd(appusage_counts$n_appusage_surveys_completed, na.rm = TRUE), 2)),
  metric_row("n_unique_reported_majors", n_distinct(na.omit(trimws(analysis_demo$major))))
)


# -----------------------------------------------------------------------------
# Political-orientation attrition metrics
# -----------------------------------------------------------------------------

po_ttest <- t.test(political_orientation ~ with_ema, data = post_po)
po_chisq <- chisq.test(table(post_po$with_ema, post_po$political_group))

po_groups <- post_po %>%
  count(with_ema, political_group) %>%
  group_by(with_ema) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(group = if_else(with_ema, "Provided EMA", "Did not provide EMA")) %>%
  select(group, political_group, percent)

po_summary <- bind_rows(
  tibble(
    metric = c(
      "post_po_total",
      "post_po_with_ema_n",
      "post_po_with_ema_pct",
      "post_po_without_ema_n",
      "post_po_without_ema_pct",
      "po_t_statistic",
      "po_t_df",
      "po_t_p_value",
      "po_chisq_statistic",
      "po_chisq_df",
      "po_chisq_p_value"
    ),
    value = c(
      nrow(post_po),
      sum(post_po$with_ema),
      pct_of(sum(post_po$with_ema), nrow(post_po)),
      sum(!post_po$with_ema),
      pct_of(sum(!post_po$with_ema), nrow(post_po)),
      round(unname(po_ttest$statistic), 2),
      round(unname(po_ttest$parameter), 1),
      signif(po_ttest$p.value, 3),
      round(unname(po_chisq$statistic), 2),
      unname(po_chisq$parameter),
      signif(po_chisq$p.value, 3)
    )
  ),
  po_groups %>%
    mutate(
      metric = paste0(
        "po_",
        if_else(group == "Provided EMA", "with_ema_", "without_ema_"),
        tolower(gsub(" ", "_", political_group)),
        "_pct"
      )
    ) %>%
    transmute(metric, value = percent)
)

political_group_summary <- analysis_demo %>%
  filter(!is.na(political_group)) %>%
  count(political_group) %>%
  mutate(percent = round(100 * n / sum(n), 1))


# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------

print(sample_description)
print(political_group_summary)
print(po_summary)

write.csv(sample_description, "sample_description_metrics.csv", row.names = FALSE)
write.csv(political_group_summary, "political_orientation_liberal_conservative.csv", row.names = FALSE)
write.csv(po_summary, "political_orientation_attrition_metrics.csv", row.names = FALSE)
