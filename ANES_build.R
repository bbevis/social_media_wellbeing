#############################################
#
#       ANES Social Media Study dataset build
#
#############################################

library(dplyr)
library(readr)

#############################################
# Load data

anes_path <- file.path(
  "Data",
  "anes_specialstudy_2020-2022_socialmedia_csv_20230705",
  "anes_specialstudy_2020-2022_socialmedia_csv_20230705.csv"
)

anes_raw <- read_csv(
  anes_path,
  na = c("", "NA", "#NULL!"),
  show_col_types = FALSE
)

#############################################
# Wave 3 IV: political orientation

# w3lcself is the wave 3 liberal-conservative self-placement item:
# 1 = very liberal, 4 = moderate/middle of the road, 7 = very conservative.
# Negative ANES codes are non-substantive responses and are treated as missing.

anes_w3_analysis <- anes_raw %>%
  transmute(
    caseid,
    age = profile_age,
    gender = as.factor(profile_gender),
    race_ethnicity = as.factor(profile_racethnicity),
    income = as.factor(profile_income),
    education = as.factor(profile_educ5),
    # Young adults are defined as ages 18-29 to match the developmental
    # "emerging adulthood" range and approximate the prior student-aged sample.
    young_adult = case_when(
      profile_age >= 18 & profile_age <= 29 ~ 1,
      profile_age >= 30 ~ 0,
      TRUE ~ NA_real_
    ),
    lcself_w3_raw = w3lcself,
    political_orientation = case_when(
      w3lcself %in% 1:7 ~ as.numeric(w3lcself),
      TRUE ~ NA_real_
    ),
    w3weight
  ) %>%
  mutate(
    political_orientation_z = as.numeric(scale(political_orientation))
  )

#############################################
# Quick checks

anes_w3_analysis %>%
  summarise(
    n_rows = n(),
    n_unique_caseid = n_distinct(caseid),
    n_valid_political_orientation = sum(!is.na(political_orientation)),
    n_missing_political_orientation = sum(is.na(political_orientation)),
    mean_political_orientation = mean(political_orientation, na.rm = TRUE),
    sd_political_orientation = sd(political_orientation, na.rm = TRUE),
    n_young_adult_valid_political_orientation = sum(
      young_adult == 1 & !is.na(political_orientation),
      na.rm = TRUE
    )
  ) %>%
  print()

anes_w3_analysis %>%
  count(political_orientation, name = "n") %>%
  arrange(political_orientation) %>%
  print()

#############################################
# Wave 3 mediators: social media use frequency

# Raw ANES frequency coding is 1 = highest frequency and 7 = never.
# Recoded values make higher numbers mean more frequent use:
# 7 = more than once a day, 1 = never.
recode_w3_frequency <- function(x) {
  case_when(
    x %in% 1:7 ~ 8 - as.numeric(x),
    TRUE ~ NA_real_
  )
}

row_sum_if_any <- function(df) {
  out <- rowSums(df, na.rm = TRUE)
  out[rowSums(!is.na(df)) == 0] <- NA_real_
  out
}

row_mean_if_any <- function(df) {
  out <- rowMeans(df, na.rm = TRUE)
  out[rowSums(!is.na(df)) == 0] <- NA_real_
  out
}

anes_w3_mediators <- anes_raw %>%
  transmute(
    caseid,
    facebook_frequency = recode_w3_frequency(w3fb3),
    instagram_frequency = recode_w3_frequency(w3inst),
    reddit_frequency = recode_w3_frequency(w3red),
    tiktok_frequency = recode_w3_frequency(w3tik),
    youtube_frequency = recode_w3_frequency(w3tube),
    twitter_frequency = recode_w3_frequency(w3twit)
  ) %>%
  mutate(
    total_platform_frequency = row_mean_if_any(
      pick(
        facebook_frequency,
        instagram_frequency,
        reddit_frequency,
        tiktok_frequency,
        youtube_frequency,
        twitter_frequency
      )
    )
  )

anes_w3_analysis <- anes_w3_analysis %>%
  left_join(anes_w3_mediators, by = "caseid")

anes_w3_analysis %>%
  summarise(
    n_facebook_frequency = sum(!is.na(facebook_frequency)),
    n_instagram_frequency = sum(!is.na(instagram_frequency)),
    n_reddit_frequency = sum(!is.na(reddit_frequency)),
    n_tiktok_frequency = sum(!is.na(tiktok_frequency)),
    n_youtube_frequency = sum(!is.na(youtube_frequency)),
    n_twitter_frequency = sum(!is.na(twitter_frequency)),
    n_total_platform_frequency = sum(!is.na(total_platform_frequency))
  ) %>%
  print()

#############################################
# Wave 3 DVs: well-being

# Loneliness, anhedonia, and depression are symptom items coded:
# 1 = not at all, 4 = nearly every day.
# They are reverse-coded so higher values indicate better well-being.
recode_symptom_wellbeing <- function(x) {
  case_when(
    x %in% 1:4 ~ 5 - as.numeric(x),
    TRUE ~ NA_real_
  )
}

# Affective well-being uses happy and hopeful as positive emotions, and
# afraid, angry, outraged, and worried as negative emotions. Negative emotions
# are reverse-coded so higher values indicate better affective well-being.
recode_positive_affect <- function(x) {
  case_when(
    x %in% 1:5 ~ as.numeric(x),
    TRUE ~ NA_real_
  )
}

recode_negative_affect <- function(x) {
  case_when(
    x %in% 1:5 ~ 6 - as.numeric(x),
    TRUE ~ NA_real_
  )
}

anes_w3_wellbeing <- anes_raw %>%
  transmute(
    caseid,
    loneliness = recode_symptom_wellbeing(w3lonely),
    anhedonia = recode_symptom_wellbeing(w3phq91),
    depression = recode_symptom_wellbeing(w3phq92),
    happy = recode_positive_affect(w3gehappy),
    hopeful = recode_positive_affect(w3gehopeful),
    afraid = recode_negative_affect(w3geafraid),
    angry = recode_negative_affect(w3geangry),
    outraged = recode_negative_affect(w3geoutraged),
    worried = recode_negative_affect(w3geworried)
  ) %>%
  mutate(
    affective_wellbeing = row_mean_if_any(
      pick(
        happy,
        hopeful,
        afraid,
        angry,
        outraged,
        worried
      )
    )
  )

anes_w3_analysis <- anes_w3_analysis %>%
  left_join(anes_w3_wellbeing, by = "caseid")

anes_w3_analysis %>%
  summarise(
    n_loneliness = sum(!is.na(loneliness)),
    n_anhedonia = sum(!is.na(anhedonia)),
    n_depression = sum(!is.na(depression)),
    n_affective_wellbeing = sum(!is.na(affective_wellbeing))
  ) %>%
  print()

write_csv(anes_w3_analysis, "anes_w3.csv")
