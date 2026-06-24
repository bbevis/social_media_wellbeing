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


#############################################
# Build Momentary dataset

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
# DV (Level-1): Momentary Well-Being
# DV is participants’ reports of their well-being 
# (i.e.,  anger, worry, happiness, sadness, stress, loneliness, and perceived social acceptance) 

momentary_dv <- function(df) {
  df %>%
    mutate(
      calendar_date = as.Date(calendar_date),
      day = wday(calendar_date, label = TRUE, abbr = TRUE),
      week = case_when(
        between(calendar_date, as.Date("2020-10-25"), as.Date("2020-10-31")) ~ 1L,
        between(calendar_date, as.Date("2020-11-01"), as.Date("2020-11-07")) ~ 2L,
        between(calendar_date, as.Date("2020-11-08"), as.Date("2020-11-14")) ~ 3L,
        between(calendar_date, as.Date("2020-11-15"), as.Date("2020-11-20")) ~ 4L,
        between(calendar_date, as.Date("2021-03-07"), as.Date("2021-03-13")) ~ 1L,
        between(calendar_date, as.Date("2021-03-14"), as.Date("2021-03-20")) ~ 2L,
        between(calendar_date, as.Date("2021-03-21"), as.Date("2021-03-27")) ~ 3L,
        between(calendar_date, as.Date("2021-03-28"), as.Date("2021-04-01")) ~ 4L,
        TRUE ~ NA_integer_
      ),
      post_election = if_else(calendar_date > ymd("2020-11-03"), 1L, 0L),
      angry    = as.numeric(momentary_wellbeing_angry_ema),
      worried  = as.numeric(momentary_wellbeing_worried_ema),
      happy    = as.numeric(momentary_wellbeing_happy_ema),
      sad      = as.numeric(momentary_wellbeing_sad_ema),
      stressed = as.numeric(momentary_wellbeing_stressed_ema),
      lonely   = as.numeric(momentary_wellbeing_lonely_ema),
      accepted = as.numeric(momentary_wellbeing_accepted_ema),
      
      angry_rev    = 5 - as.numeric(momentary_wellbeing_angry_ema),
      worried_rev  = 5 - as.numeric(momentary_wellbeing_worried_ema),
      sad_rev    = 5 - as.numeric(momentary_wellbeing_sad_ema),
      stressed_rev = 5 - as.numeric(momentary_wellbeing_stressed_ema),
      lonely_rev   = 5 - as.numeric(momentary_wellbeing_lonely_ema),
      awb_momentary = rowMeans(
        cbind(angry_rev, worried_rev, happy, sad_rev, stressed_rev, lonely_rev, accepted),
        na.rm = TRUE
      )
    )
}



ema_moment <- momentary_dv(bind_rows(ema_1, ema_2))


#############################################
# Mediator (Level-1): Momentary Social Media Use
# 1) momentary_context_activity_ema
#    - Multiple responses, stored as comma-separated numeric codes for past-hour activities.
#    - Code 5 = "Using social media".
#    - Code 0 = "Skip question" (treat as NA).
#
# 2) momentary_context_social_media_ema
#    - Single-response duration category for social media use in the past hour:
#        1 = 1–15 minutes
#        2 = 16–30 minutes
#        3 = 31–45 minutes
#        4 = 46–60 minutes
#        0 = Skip question (treat as NA)
#
# Constructed variables:
#   sm_any:
#     = 1 if activity codes contain 5 ("Using social media")
#     = 0 if activity codes are present but do not include 5
#     = NA if activity column is missing/skip
#
#   sm_minutes:
#     = 0 if sm_any == 0 (explicitly no social media selected)
#     = 15 / 30 / 45 / 60 if sm_any == 1 and duration category is 1/2/3/4, respectively
#       (we use the bin maxima per category label)
#     = NA if sm_any == 1 but duration is missing/skip/invalid
#     = NA if sm_any is NA
#
# Notes:
# - Strings containing "5" are relevant for social media use.
# - We do not coerce missing duration into 0 when sm_any == 1; that remains genuinely missing.
# - These two variables let us run use/no-use analyses (sm_any) and dose analyses (sm_minutes).

momentary_mediator <- function(df) {
  df %>%
    mutate(
      act = str_replace_all(momentary_context_activity_ema, "\\s+", ""),  # trim
      sm_any = case_when(
        is.na(act) ~ NA_real_,
        act == "0" ~ NA_real_,
        str_detect(act, "(^|,)5(,|$)") ~ 1,
        TRUE ~ 0
      ),
      sm_dur_cat = suppressWarnings(as.numeric(momentary_context_social_media_ema)),
      sm_dur_cat = if_else(sm_dur_cat == 0, NA_real_, sm_dur_cat),
      sm_minutes = case_when(
        is.na(sm_any) ~ NA_real_,
        sm_any == 0 ~ 0,
        sm_dur_cat == 1 ~ 15,
        sm_dur_cat == 2 ~ 30,
        sm_dur_cat == 3 ~ 45,
        sm_dur_cat == 4 ~ 60,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(-act, -sm_dur_cat)
}

ema_moment <- momentary_mediator(ema_moment)


################################
# Covariates

# Participant-level demographics (replicated across moments)
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

# SES from pre-survey (replicated across moments)
ses <- bind_rows(pre_1, pre_2) %>%
  transmute(
    id,
    ses = demog_ses_t1
  )


# Create a compact "momentary base" with one row per EMA record
# Tag term by id prefix to match weekly style: FA = Fall (0), SP = Spring (1)
# base + term
moment_base <- ema_moment %>%
  mutate(calendar_date = ymd(calendar_date)) %>%
  transmute(
    id, calendar_date,
    term = case_when(str_sub(id, 1, 2) == "FA" ~ 0L,
                     str_sub(id, 1, 2) == "SP" ~ 1L,
                     TRUE ~ NA_integer_)
  )

# counts per id
num_ema_df <- moment_base %>% count(id, name = "num_ema_completed")

# assemble covariates
covariates_momentary <- moment_base %>%
  left_join(demo, by = "id") %>%
  left_join(ses, by = "id") %>%
  left_join(num_ema_df, by = "id")

momentary_df <- ema_moment %>%
  mutate(calendar_date = ymd(calendar_date)) %>%
  left_join(covariates_momentary, by = c("id","calendar_date"))

# Join IV (political orientation)
# Merge political orientation into momentary dataset
momentary_df <- momentary_df %>%
  left_join(political_orientation_df, by = "id")

write_csv(momentary_df, "momentary.csv")
