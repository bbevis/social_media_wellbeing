#############################################
# Study 1: descriptive statistics for the predictors
#############################################

# Reported in Study 1 Methods > Measures > Independent Variable:
# Political Orientation: mean and SD (M = 3.13, SD = 1.26).
# These summaries use the monthly dataset

library(dplyr)

monthly_df <- read.csv("monthly.csv")

# Reported political-orientation summary: requires only monthly.csv.
iv_desc <- monthly_df %>%
  summarise(
    political_orientation_mean = mean(political_orientation, na.rm = TRUE),
    political_orientation_sd = sd(political_orientation, na.rm = TRUE)
  )

print(iv_desc)


#############################################
# Optional additional summaries: not reported in the manuscript
#############################################

# To calculate these additional summaries, uncomment the entire block below.
# Original party-affiliation and presidential-support percentages require the
# two raw pre-survey files listed here; these are not shared replication inputs.
# The means/SDs use the corresponding monthly variables.
# Results are printed separately as optional_iv_desc.

# presurvey_raw <- bind_rows(
#   read.csv("Data/coco_ut1_presurvey.csv", sep = ";"),
#   read.csv("Data/coco_ut2_presurvey.csv", sep = ";")
# ) %>%
#   mutate(presidential_support_raw = coalesce(support_election2020_t1, voting_2020_t1)) %>%
#   select(
#     id,
#     party_affiliation_raw = party_affiliation_t1,
#     presidential_support_raw
#   )
#
# optional_iv_desc <- monthly_df %>%
#   left_join(presurvey_raw, by = "id") %>%
#   summarise(
#     party_affiliation_mean = mean(party_affiliation_t1, na.rm = TRUE),
#     party_affiliation_sd = sd(party_affiliation_t1, na.rm = TRUE),
#     party_affiliation_republican_pct = mean(party_affiliation_raw == 1, na.rm = TRUE) * 100,
#     party_affiliation_democrat_pct = mean(party_affiliation_raw == 2, na.rm = TRUE) * 100,
#     party_affiliation_other_pct = mean(party_affiliation_raw == 3, na.rm = TRUE) * 100,
#     party_affiliation_none_pct = mean(party_affiliation_raw == 4, na.rm = TRUE) * 100,
#     authoritarianism_mean = mean(authoritarianism, na.rm = TRUE),
#     authoritarianism_sd = sd(authoritarianism, na.rm = TRUE),
#     presidential_support_mean = mean(presidential_support, na.rm = TRUE),
#     presidential_support_sd = sd(presidential_support, na.rm = TRUE),
#     presidential_support_trump_pct = mean(presidential_support_raw == 1, na.rm = TRUE) * 100,
#     presidential_support_biden_pct = mean(presidential_support_raw == 2, na.rm = TRUE) * 100,
#     presidential_support_other_pct = mean(presidential_support_raw == 3, na.rm = TRUE) * 100
#   )
#
# print(optional_iv_desc)
