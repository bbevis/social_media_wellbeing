#############################################
#
#       Monthly level dataset build
#       
#
#############################################

library(dplyr)
library(stringr)
library(lme4)
library(tidyr)
# install.packages("mediation")
library(mediation)
# install.packages("multilevelmediation")
# library(multilevelmediation)
library(boot)
library(purrr)
library(psych)
library(pwr)


# Read data
appusage_1 <- read.csv("data/coco_ut1_appusage_survey.csv", header = TRUE, sep = ";")
appusage_health_1 <- read.csv("data/coco_ut1_appusage_survey_health.csv", header = TRUE)
# ema_1 <- read.csv("data/coco_ut1_ema_survey.csv", header = TRUE, sep = ";")
post_1 <- read.csv("data/coco_ut1_postsurvey.csv", header = TRUE, sep = ";")
pre_1 <- read.csv("data/coco_ut1_presurvey.csv", header = TRUE, sep = ";")
appusage_2 <- read.csv("data/coco_ut2_appusage_survey.csv", header = TRUE, sep = ";")
appusage_health_2 <- read.csv("data/coco_ut2_appusage_survey_health.csv", header = TRUE)
# ema_2 <- read.csv("data/coco_ut2_ema_survey.csv", header = TRUE, sep = ";")
post_2 <- read.csv("data/coco_ut2_postsurvey.csv", header = TRUE, sep = ";")
pre_2 <- read.csv("data/coco_ut2_presurvey.csv", header = TRUE, sep = ";")
demo_fall <- read.csv("data/demographics_fall_anonymized.csv", header = TRUE)
demo_spring <- read.csv("data/demographics_spring_anonymized.csv", header = TRUE)

appusage_1 <- appusage_1 %>%
  left_join(appusage_health_1, by = "id")

appusage_2 <- appusage_2 %>%
  left_join(appusage_health_2, by = "id")

platforms <- c("facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat") # Platforms
activities <- c("information", "social", "productivity", "education", "entertainment", 
                "creativity", "games", "shopping", "utilities", "other", "health") # Activity categories
NO_APP_STRATEGY <- "NA"   # primary analysis; use "ZERO" for the no-app-as-zero robustness variant

###############################
# Chronback alpha

# Combine post_1 and post_2 again, just in case
post <- bind_rows(post_1, post_2)

# Define item sets for each scale
swls_items <- c("swls_1_t2", "swls_2_t2", "swls_3_t2", "swls_4_t2", "swls_5_t2")
uls_items <- c("uls_1_t2", "uls_2_t2", "uls_3_t2", "uls_4_t2", "uls_5_t2",
               "uls_6_t2", "uls_7_t2", "uls_8_t2", "uls_9_t2")
awb_items <- c("awb_1_t2", "awb_2_t2", "awb_3_t2", "awb_4_t2", "awb_5_t2", "awb_6_t2")

# Reverse-code items in the ULS and AWB scales
post <- post %>%
  mutate(
    uls_1_t2 = 5 - uls_1_t2,
    uls_4_t2 = 5 - uls_4_t2,
    uls_8_t2 = 5 - uls_8_t2,
    awb_1_t2 = 6 - awb_1_t2,
    awb_2_t2 = 6 - awb_2_t2,
    awb_4_t2 = 6 - awb_4_t2
  )

# Compute Cronbach's alpha for each scale
psych::alpha(post[ , swls_items], check.keys=TRUE)$total$raw_alpha
psych::alpha(post[ , uls_items], check.keys=TRUE)$total$raw_alpha
psych::alpha(post[ , awb_items], check.keys=TRUE)$total$raw_alpha



#############################################
# Build monthly dataset

# DVs. Post-survey well-being (controlling for pre-survey well-being)
# measured using three validated scales:
# the Satisfaction with Life Scale, the UCLA Loneliness Scale, and the Affective Wellbeing Scale.

# IV. Political orientation.
# Demographic survey Q12 is coded 1 = extremely conservative to
# 7 = extremely liberal; reverse-code so higher values indicate conservatism.
political_orientation <- bind_rows(
  demo_fall %>%
    slice(-(1:2)) %>%
    transmute(
      id = pID,
      Q12_num = suppressWarnings(as.numeric(Q12)),
      political_orientation = if_else(Q12_num %in% 1:7, 8 - Q12_num, NA_real_)
    ),
  demo_spring %>%
    slice(-(1:2)) %>%
    transmute(
      id = pID,
      Q12_num = suppressWarnings(as.numeric(Q12)),
      political_orientation = if_else(Q12_num %in% 1:7, 8 - Q12_num, NA_real_)
    )
) %>%
  dplyr::select(id, political_orientation)

authoritarianism <- bind_rows(
  post_1 %>%
    transmute(
      id,
      authoritarianism = rowMeans(cbind(ksa_1_t2, ksa_2_t2, ksa_3_t2))
    ),
  post_2 %>%
    transmute(
      id,
      authoritarianism = rowMeans(cbind(ksa_1_t2, ksa_2_t2, ksa_3_t2))
    )
)
  
post = bind_rows(post_1, post_2) %>%
  mutate(
    swls_t2 = rowMeans(cbind(swls_1_t2, swls_2_t2, swls_3_t2, swls_4_t2, swls_5_t2), na.rm = TRUE),
    uls_t2 = rowMeans(cbind(5 - uls_1_t2, uls_2_t2, uls_3_t2, 5 - uls_4_t2, uls_5_t2,
                            uls_6_t2, uls_7_t2, 5 - uls_8_t2, uls_9_t2), na.rm = TRUE),
    awb_t2 = rowMeans(cbind(5 - awb_1_t2, 5 - awb_2_t2, awb_3_t2, 5 - awb_4_t2, awb_5_t2, awb_6_t2), na.rm = TRUE)
  ) %>%
  dplyr::select(id, swls_t2, uls_t2, awb_t2) %>%
  left_join(political_orientation, by = "id") %>%
  left_join(authoritarianism, by = "id")

pre = bind_rows(pre_1, pre_2) %>%
  mutate(
    swls_t1 = rowMeans(cbind(swls_1_t1, swls_2_t1, swls_3_t1, swls_4_t1, swls_5_t1), na.rm = TRUE),
    uls_t1 = rowMeans(cbind(5 - uls_1_t1, uls_2_t1, uls_3_t1, 5 - uls_4_t1, uls_5_t1,
                            uls_6_t1, uls_7_t1, 5 - uls_8_t1, uls_9_t1), na.rm = TRUE),
    awb_t1 = rowMeans(cbind(5 - awb_1_t1, 5 - awb_2_t1, awb_3_t1, 5 - awb_4_t1, awb_5_t1, awb_6_t1), na.rm = TRUE)
  ) %>%
  mutate(presidential_support = coalesce(support_election2020_t1, 0) + coalesce(voting_2020_t1, 0),
         presidential_support = case_when(
           presidential_support == 1 ~ 1,
           presidential_support == 2 ~ 0,
           TRUE      ~ NA_real_),
         party_affiliation_t1 = case_when(
           party_affiliation_t1 == 1 ~ 1,
           party_affiliation_t1 == 2 ~ 0,
           TRUE      ~ NA_real_)
  ) %>%
  dplyr::select(id, swls_t1, uls_t1, awb_t1, ses = demog_ses_t1, party_affiliation_t1,
                presidential_support)



monthly_df = post %>%
  left_join(pre, by = 'id') %>%
  mutate(term = case_when(
    str_sub(id, 1, 2) == "FA" ~ 0, # create term indicator
    str_sub(id, 1, 2) == "SP" ~ 1,
    TRUE ~ NA_real_)
  )

cor_df = monthly_df %>%
  dplyr::select(swls_t2, uls_t2, awb_t2, swls_t1, uls_t1, awb_t1)

cor_mat = round(cor(cor_df, use = "complete.obs", method = "pearson"), 2)
print(cor_mat)

################################
# Mediator. Social media usage measured from weekly app usage surveys,
# which will be aggregated to the monthly level by taking the mean of weekly values.

appusage = bind_rows(appusage_1, appusage_2)

# A week is NA only if BOTH hours and minutes are NA; otherwise sum with missing -> 0
weekly_value <- function(h, m) {
  h <- suppressWarnings(as.numeric(as.character(h)))
  m <- suppressWarnings(as.numeric(as.character(m)))
  if_else(
    is.na(h) & is.na(m),
    NA_real_,
    coalesce(h, 0) + coalesce(m, 0) / 60
  )
}


# Convert hours/minutes -> hours for each week, keep per-week rows (id × week)
aggregate_weekly_usage <- function(df, var_list) {
  out_list <- vector("list", 4)
  
  for (w in 1:4) {
    result <- data.frame(id = df$id, week = w)
    
    for (var in var_list) {
      h_col <- paste0("hours_",   var, "_w", w)
      m_col <- paste0("minutes_", var, "_w", w)
      
      result[[var]] <- weekly_value(df[[h_col]], df[[m_col]])
    }
    out_list[[w]] <- result
  }
  bind_rows(out_list)
}

weekly_platforms <- aggregate_weekly_usage(appusage, platforms)
weekly_activities <- aggregate_weekly_usage(appusage, activities)

make_no_long <- function(df, var_list) {
  weeks <- 1:4
  bind_rows(lapply(weeks, function(w) {
    tibble(
      id = df$id,
      week = w,
      !!!setNames(lapply(var_list, function(v) {
        col <- paste0("no_", v, "_w", w)
        val <- suppressWarnings(as.integer(as.character(df[[col]])))
        as.integer(dplyr::coalesce(val == 1L, FALSE))
      }), paste0("no_", var_list))
    )
  }))
}

make_accuracy_long <- function(df) {
  bind_rows(lapply(1:4, function(w) {
    col <- paste0("appusage_accuracy_w", w)
    tibble(
      id = df$id,
      week = w,
      appusage_accuracy = if (col %in% names(df)) as.integer(df[[col]]) else NA_integer_
    )
  })) %>%
    mutate(accuracy_exclude = if_else(appusage_accuracy == 1L, 1L, 0L))
}

no_platforms_long <- make_no_long(appusage, platforms)
no_activities_long <- make_no_long(appusage, activities)
accuracy_long <- make_accuracy_long(appusage)

# Apply the weekly app-usage validity screen before aggregating to monthly.
# We retain the upper-bound plausibility exclusion (>20 hours/day) but do not
# apply the preregistered lower-bound exclusion (<1 hour/week), preserving valid
# low-use weeks.
# LOWER_PER_WEEK <- 1
UPPER_PER_DAY <- 20

appusage_weekly_monthly <- weekly_platforms %>%
  left_join(weekly_activities, by = c("id", "week")) %>%
  left_join(no_platforms_long, by = c("id", "week")) %>%
  left_join(no_activities_long, by = c("id", "week")) %>%
  left_join(accuracy_long, by = c("id", "week")) %>%
  mutate(
    term = case_when(
      str_sub(id, 1, 2) == "FA" ~ 0L,
      str_sub(id, 1, 2) == "SP" ~ 1L,
      TRUE ~ NA_integer_
    ),
    days_in_week = case_when(
      week == 4 & term == 0L ~ 6L,
      week == 4 & term == 1L ~ 5L,
      TRUE ~ 7L
    )
  )

for (m in c(platforms, activities)) {
  no_col <- paste0("no_", m)
  if (NO_APP_STRATEGY == "NA") {
    appusage_weekly_monthly[[m]] <- ifelse(
      dplyr::coalesce(appusage_weekly_monthly[[no_col]] == 1L, FALSE),
      NA_real_,
      appusage_weekly_monthly[[m]]
    )
  } else if (NO_APP_STRATEGY == "ZERO") {
    appusage_weekly_monthly[[m]] <- ifelse(
      dplyr::coalesce(appusage_weekly_monthly[[no_col]] == 1L, FALSE),
      0,
      appusage_weekly_monthly[[m]]
    )
  }
}

appusage_weekly_monthly <- appusage_weekly_monthly %>%
  rowwise() %>%
  mutate(
    total_platform_raw = if (all(is.na(c_across(all_of(platforms)))))  NA_real_
    else sum(c_across(all_of(platforms)),  na.rm = TRUE),
    total_activity_raw = if (all(is.na(c_across(all_of(activities))))) NA_real_
    else sum(c_across(all_of(activities)), na.rm = TRUE),
    appusage_response = any(!is.na(c_across(all_of(c(platforms, activities))))),
    total_activity_7day = total_activity_raw * (7 / days_in_week),
    appusage_bounds_exclude = if_else(
      is.na(total_activity_raw),
      0L,
      as.integer(total_activity_raw > days_in_week * UPPER_PER_DAY)
    ),
    appusage_week_exclude = as.integer(
      coalesce(accuracy_exclude, 0L) == 1L |
        appusage_bounds_exclude == 1L
    ),
    valid_appusage_week = appusage_response & appusage_week_exclude == 0L
  ) %>%
  ungroup()

appusage_weekly_monthly <- appusage_weekly_monthly %>%
  mutate(across(
    all_of(c(platforms, activities, "total_platform_raw", "total_activity_raw")),
    ~ ifelse(valid_appusage_week, .x, NA_real_)
  ))

monthly_eligibility <- appusage_weekly_monthly %>%
  group_by(id) %>%
  summarise(
    n_valid_appusage_weeks = sum(valid_appusage_week, na.rm = TRUE),
    monthly_appusage_exclude = as.integer(n_valid_appusage_weeks < 2),
    .groups = "drop"
  )

appusage_monthly <- appusage_weekly_monthly %>%
  group_by(id) %>%
  summarise(
    across(all_of(c(platforms, activities)), ~ mean(.x, na.rm = TRUE)),
    total_platform_raw = if (all(is.na(total_platform_raw))) NA_real_
    else mean(total_platform_raw, na.rm = TRUE),
    total_activity_raw = if (all(is.na(total_activity_raw))) NA_real_
    else mean(total_activity_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(monthly_eligibility, by = "id") %>%
  rowwise() %>%
  mutate(
    total_platform = if (monthly_appusage_exclude == 1L) NA_real_
    else if (all(is.na(c_across(all_of(platforms))))) NA_real_
    else sum(c_across(all_of(platforms)), na.rm = TRUE),
    total_activity = if (monthly_appusage_exclude == 1L) NA_real_
    else if (all(is.na(c_across(all_of(activities))))) NA_real_
    else sum(c_across(all_of(activities)), na.rm = TRUE)
  ) %>%
  ungroup()

# Downstream scripts expect one exclusion column per mediator. These columns are
# aliases of the survey-week app-usage exclusion, not mediator-specific checks.
for (m in c(platforms, activities, "total_platform", "total_activity")) {
  appusage_monthly[[paste0(m, "_exclude")]] <- appusage_monthly$monthly_appusage_exclude
  appusage_monthly[[m]] <- ifelse(
    appusage_monthly$monthly_appusage_exclude == 1L,
    NA_real_,
    appusage_monthly[[m]]
  )
}

write.csv(appusage_monthly, "appusage_monthly.csv", row.names = FALSE)

monthly_df = monthly_df %>%
  left_join(appusage_monthly, by = "id")

################################
# Covariates

demo <- bind_rows(demo_fall, demo_spring) %>%
  slice(-(1:2)) %>%                               # same cleanup as weekly
  transmute(
    id        = pID,
    age       = as.numeric(Q24),
    gender    = case_when(Q1 == 1 ~ 'male',  # male
                          Q1 == 2 ~ 'female',  # female
                          Q1 == 3 ~ 'other',  # other
                          TRUE    ~ NA_character_),
    gender = as.factor(gender),
    ethnicity = as.factor(gsub(" ", "", Q7))
  )


monthly_df = monthly_df %>%
  left_join(demo, by = "id") 

# replace missing values with NAs
monthly_df <- monthly_df %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

rm(list = intersect(
  c("appusage_1", "appusage_2", "appusage_health_1", "appusage_health_2",
    "demo_fall", "demo_spring", "pre_1", "pre_2", "post_1", "post_2",
    "activity_df", "platform_df"),
  ls()
))

write.csv(monthly_df, "monthly.csv", row.names = FALSE)


rm(list = intersect(
  c("appusage", "appusage_monthly",
    "attrition_results", "awb_alpha",
    "cor_df", "cor_mat", "demo",
    "exclusion_summary", "post", "pre",
    "swls_alpha", "uls_alpha"),
  ls()
))
