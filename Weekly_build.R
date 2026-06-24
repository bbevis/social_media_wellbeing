#############################################
#
#       Weekly level dataset build
#       
#
#############################################

# Packages
library(dplyr)
library(stringr)
library(tidyr)
library(mediation)
library(purrr)
library(readr)
library(lubridate)


NO_APP_STRATEGY <- "NA"   # primary analysis; use "ZERO" for the no-app-as-zero robustness variant

#############################################
# Load data

appusage_1 <- read.csv("data/coco_ut1_appusage_survey.csv", header = TRUE, sep = ";")
ema_1      <- read.csv("data/coco_ut1_ema_survey.csv", header = TRUE, sep = ";")
post_1     <- read.csv("data/coco_ut1_postsurvey.csv", header = TRUE, sep = ";")
pre_1      <- read.csv("data/coco_ut1_presurvey.csv", header = TRUE, sep = ";")
daily_1    <- read.csv("data/coco_ut1_daily_survey.csv", header = TRUE, sep = ";")

appusage_2 <- read.csv("data/coco_ut2_appusage_survey.csv", header = TRUE, sep = ";")
ema_2      <- read.csv("data/coco_ut2_ema_survey.csv", header = TRUE, sep = ";")
post_2     <- read.csv("data/coco_ut2_postsurvey.csv", header = TRUE, sep = ";")
pre_2      <- read.csv("data/coco_ut2_presurvey.csv", header = TRUE, sep = ";")
daily_2    <- read.csv("data/coco_ut2_daily_survey.csv", header = TRUE, sep = ";")

demo_fall   <- read.csv("data/demographics_fall_anonymized.csv", header = TRUE)
demo_spring <- read.csv("data/demographics_spring_anonymized.csv", header = TRUE)


mediators <- c(
  "facebook", "twitter", "tiktok", "instagram", "youtube", "reddit", "snapchat",
  "information", "social", "productivity", "education", "entertainment",
  "creativity", "games", "shopping", "utilities", "other",
  "total_platform", "total_activity")

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other")

write.csv(appusage_1, "appusage_raw.csv", row.names = FALSE)

#############################################
# Build Weekly dataset


# IV. Political orientation (Level-2 IV)
# Demographic survey Q12 is coded 1 = extremely conservative to
# 7 = extremely liberal; reverse-code so higher values indicate conservatism.
political_orientation_df <- bind_rows(
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

authoritarianism_df <- bind_rows(
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

pre = bind_rows(pre_1, pre_2) %>%
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
  dplyr::select(id, party_affiliation_t1, presidential_support)

political_orientation_df = political_orientation_df %>%
  left_join(authoritarianism_df, by = 'id') %>%
  left_join(pre, by = 'id')

#############################################
# Weekly DVs from EMA (Well-being)
# DV is participants’ reports of their well-being 
# (i.e.,  anger, worry, happiness, sadness, stress, loneliness, and perceived social acceptance) 
# measured by EMAs that will be aggregated to the weekly level
#############################################

prep_ema_weekly <- function(ema_df) {
  ema_df %>%
    mutate(calendar_date = as.Date(calendar_date)) %>%
    # Election week flag from daily_1 (Fall 2020 only)
    mutate(post_election = if_else(calendar_date > ymd("2020-11-03"), 1L, 0L)) %>%
    mutate(week = case_when(
      between(calendar_date, as.Date("2020-10-25"), as.Date("2020-10-31")) ~ 1L,
      between(calendar_date, as.Date("2020-11-01"), as.Date("2020-11-07")) ~ 2L,
      between(calendar_date, as.Date("2020-11-08"), as.Date("2020-11-14")) ~ 3L,
      between(calendar_date, as.Date("2020-11-15"), as.Date("2020-11-20")) ~ 4L, # 6 days
      between(calendar_date, as.Date("2021-03-07"), as.Date("2021-03-13")) ~ 1L,
      between(calendar_date, as.Date("2021-03-14"), as.Date("2021-03-20")) ~ 2L,
      between(calendar_date, as.Date("2021-03-21"), as.Date("2021-03-27")) ~ 3L,
      between(calendar_date, as.Date("2021-03-28"), as.Date("2021-04-01")) ~ 4L, # 5 days
      TRUE ~ NA_integer_
    )) %>%
    # Pull the 7 items; then recode 0 ("Skip") -> NA
    transmute(
      id, week, post_election,
      angry    = as.numeric(momentary_wellbeing_angry_ema),
      worried  = as.numeric(momentary_wellbeing_worried_ema),
      happy    = as.numeric(momentary_wellbeing_happy_ema),
      sad      = as.numeric(momentary_wellbeing_sad_ema),
      stressed = as.numeric(momentary_wellbeing_stressed_ema),
      lonely   = as.numeric(momentary_wellbeing_lonely_ema),
      accepted = as.numeric(momentary_wellbeing_accepted_ema)
    ) %>%
    # mutate(across(c(angry, worried, happy, sad, stressed, lonely, accepted),
    #               ~ ifelse(.x == 0, NA, .x))) %>% # If the value is 0, replace it with NA, otherwise keep the same
    group_by(id, week) %>%
    summarise(
      n_pings_week = n(),
      # how many EMA pings had ANY item answered this week
      n_ema = sum(!is.na(angry) | !is.na(worried) | !is.na(happy) |
                    !is.na(sad)   | !is.na(stressed) | !is.na(lonely) | !is.na(accepted)),
      # item-level weekly means (keep these for separate models later)
      angry        = mean(angry,    na.rm = TRUE),
      worried      = mean(worried,  na.rm = TRUE),
      happy    = mean(happy,    na.rm = TRUE),
      sad          = mean(sad,      na.rm = TRUE),
      stressed     = mean(stressed, na.rm = TRUE),
      lonely       = mean(lonely,   na.rm = TRUE),
      accepted = mean(accepted, na.rm = TRUE),
      post_election = max(post_election, na.rm = TRUE),
      angry_rev        = mean(5 - angry,    na.rm = TRUE),
      worried_rev      = mean(5 - worried,  na.rm = TRUE),
      sad_rev          = mean(5 - sad,      na.rm = TRUE),
      stressed_rev     = mean(5 - stressed, na.rm = TRUE),
      lonely_rev       = mean(5 - lonely,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # composite using the reversed items for negative emotions
    mutate(
      awb_week = rowMeans(
        cbind(angry_rev, worried_rev, happy, sad_rev,
              stressed_rev, lonely_rev, accepted),
        na.rm = TRUE
      ),
      ema_completion_rate = n_ema / n_pings_week
    )
}

# add exclusion flags to EMA weekly DVs
ema_weekly <- prep_ema_weekly(bind_rows(ema_1, ema_2)) %>%
  mutate(
    ema_40_exclude = as.integer(!is.na(ema_completion_rate) & ema_completion_rate < 0.40),
    ema_50_exclude = as.integer(!is.na(ema_completion_rate) & ema_completion_rate < 0.50),
    ema_60_exclude = as.integer(!is.na(ema_completion_rate) & ema_completion_rate < 0.60)
  )


#############################################
# Weekly mediator: Social media usage measured from weekly app usage surveys
#############################################

# turns appusage from wide to long
# accuracy (id × week) from app usage_*
make_accuracy_long <- function(df) {
  weeks <- 1:4
  acc_list <- lapply(weeks, function(w) {
    col <- paste0("appusage_accuracy_w", w)
    tibble(
      id   = df$id,
      week = w,
      # Keep numeric; coerce to integer if needed
      appusage_accuracy = if (col %in% names(df)) as.integer(df[[col]]) else NA_integer_
    )
  })
  bind_rows(acc_list) %>%
    # Flag as 1 if "Not at all accurate" (1), else 0
    # if_else() automatically converts NA to 0L when the condition is FALSE/NA
    mutate(
      accuracy_exclude = if_else(appusage_accuracy == 1L, 1L, 0L) 
    )
  
  
}


accuracy_long <- make_accuracy_long(bind_rows(appusage_1, appusage_2))

# A week is NA if BOTH hours and minutes are NA; otherwise sum with missing -> 0
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

appusage_combined <- bind_rows(appusage_1, appusage_2)

weekly_platforms  <- aggregate_weekly_usage(appusage_combined, platforms)
weekly_activities <- aggregate_weekly_usage(appusage_combined, activities)

# add not app available exclusion flag:
# Build a long (id × week) table of "no app/category this week" flags
make_no_long <- function(df, var_list) {
  weeks <- 1:4
  out <- lapply(weeks, function(w) {
    tibble(
      id   = df$id,
      week = w,
      !!!setNames(lapply(var_list, function(v) {
        col <- paste0("no_", v, "_w", w)
        # Treat 1 as "doesn't have this app this week"; coerce others to 0/NA
        if (!col %in% names(df)) return(NA_integer_)
        val <- suppressWarnings(as.integer(as.character(df[[col]])))
        as.integer(dplyr::coalesce(val == 1L, FALSE))  # 1 = “no app”, else 0/NA→0
      }), paste0("no_", var_list))
    )
  })
  bind_rows(out)
}

no_platforms_long  <- make_no_long(appusage_combined, platforms)
no_activities_long <- make_no_long(appusage_combined, activities)

# add exclusion flags:
# Weekly app usage survey measures for a given week will be excluded if
# 1) participants report that their screen time was ‘not at all accurate,’ or
# 2) if they reported greater than 20 hours of total app usage on average a day.
# (These numbers would suggest that the phone
# was not in use or that the participants’ app usage was recorded incorrectly).
# We do not apply the preregistered lower-bound exclusion (<1 hour/week);
# this deviation preserves valid low-use weeks.

# Define exclusion criteria bounds as constants for easier changes later
# LOWER_PER_WEEK <- 1  # preregistered < 1 hr/week lower-bound exclusion not applied
UPPER_PER_DAY <- 20  # > 20 hrs/day = exclude

appusage_weekly <- weekly_platforms %>%
  left_join(weekly_activities, by = c("id","week")) %>%
  left_join(no_platforms_long,  by = c("id","week")) %>%
  left_join(no_activities_long, by = c("id","week")) %>%
  left_join(accuracy_long, by = c("id","week")) %>%
  mutate(
    term = case_when(substr(id, 1, 2) == "FA" ~ 0L,   # Fall 2020
                            substr(id, 1, 2) == "SP" ~ 1L,   # Spring 2021
                            TRUE ~ NA_integer_),
    days_in_week = case_when(
      week == 4 & term == 0L ~ 6L,  # Fall W4 = 6 days (Oct 25–31 is 7; Nov 15–20 is 6)
      week == 4 & term == 1L ~ 5L,  # Spring W4 = 5 days (Mar 28–Apr 1)
      TRUE                    ~ 7L
    )
  )


# Apply "no app/category" coding before totals and exclusions.
for (m in c(platforms, activities)) {
  no_col     <- paste0("no_", m)
  
  if (no_col %in% names(appusage_weekly)) {
    if (NO_APP_STRATEGY == "NA") {
      appusage_weekly[[m]] <- ifelse(
        dplyr::coalesce(appusage_weekly[[no_col]] == 1L, FALSE),
        NA_real_,
        appusage_weekly[[m]]
      )
    } else if (NO_APP_STRATEGY == "ZERO") {
      appusage_weekly[[m]] <- ifelse(
        dplyr::coalesce(appusage_weekly[[no_col]] == 1L, FALSE),
        0,
        appusage_weekly[[m]]
      )
    }
  }
}

# Compute survey-level totals and apply preregistered weekly app-usage exclusions.
appusage_weekly <- appusage_weekly %>%
  rowwise() %>%
  mutate(
    total_platform_raw  = if (all(is.na(c_across(all_of(platforms)))))  NA_real_
    else sum(c_across(all_of(platforms)),  na.rm = TRUE),
    total_activity_raw  = if (all(is.na(c_across(all_of(activities))))) NA_real_
    else sum(c_across(all_of(activities)), na.rm = TRUE),
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
    total_platform = {
      vals  <- c_across(all_of(platforms))
      if (appusage_week_exclude == 1L) vals[] <- NA_real_
      if (all(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
    },
    total_activity = {
      vals  <- c_across(all_of(activities))
      if (appusage_week_exclude == 1L) vals[] <- NA_real_
      if (all(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Downstream scripts expect one exclusion column per mediator. These columns are
# aliases of the survey-week app-usage exclusion, not mediator-specific checks.
for (m in c(platforms, activities, "total_platform", "total_activity")) {
  appusage_weekly[[paste0(m, "_exclude")]] <- appusage_weekly$appusage_week_exclude
  appusage_weekly[[m]] <- ifelse(
    appusage_weekly$appusage_week_exclude == 1L,
    NA_real_,
    appusage_weekly[[m]]
  )
}

write.csv(appusage_weekly, "appusage_weekly.csv", row.names = FALSE)

################################
# Covariates

# Participant-level demographics (replicated across weeks)
demo <- bind_rows(demo_fall, demo_spring) %>%
  slice(-(1:2)) %>%
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

# SES from pre-survey
ses <- bind_rows(pre_1, pre_2) %>%
  transmute(
    id,
    ses = demog_ses_t1
  )


# Full covariate table: replicate per week and merge
covariates_weekly <- expand.grid(
  id   = unique(c(ema_weekly$id, appusage_weekly$id)),
  week = 1:4
) %>%
  as_tibble() %>%
  left_join(demo, by = "id") %>%
  left_join(ses, by = "id") #%>%
  # term variable
  # mutate(term = case_when(
  #   str_sub(id, 1, 2) == "FA" ~ 0L,
  #   str_sub(id, 1, 2) == "SP" ~ 1L,
  #   TRUE ~ NA_integer_
  # )


# Count the number of weeks each person provided valid app usage data
completed_surveys_per_person <- appusage_weekly %>%
  # A non-missing total means they answered at least one item
  filter(!is.na(total_platform)) %>% 
  group_by(id) %>%
  summarise(n_appusage_surveys_completed = n())


# Merge IVs, DVs, and covariates together
weekly_df <- ema_weekly %>%
  left_join(appusage_weekly,  by = c("id", "week")) %>%
  left_join(covariates_weekly, by = c("id", "week")) %>%
  left_join(political_orientation_df, by = "id") %>%
  left_join(completed_surveys_per_person, by = "id") #%>%
  #filter(!is.na(week))

write.csv(weekly_df, "weekly.csv", row.names = FALSE)
# write.csv(weekly_df, "weekly_missingisNA.csv", row.names = FALSE)

cor(weekly_df$term, weekly_df$post_election, use = "complete.obs")
