library(dplyr)

monthly_df <- read.csv("monthly.csv")

presurvey_raw <- bind_rows(
  read.csv("Data/coco_ut1_presurvey.csv", sep = ";"),
  read.csv("Data/coco_ut2_presurvey.csv", sep = ";")
) %>%
  mutate(presidential_support_raw = coalesce(support_election2020_t1, voting_2020_t1)) %>%
  select(
    id,
    party_affiliation_raw = party_affiliation_t1,
    presidential_support_raw
  )

iv_desc <- monthly_df %>%
  left_join(presurvey_raw, by = "id") %>%
  summarise(
    political_orientation_mean = mean(political_orientation, na.rm = TRUE),
    political_orientation_sd = sd(political_orientation, na.rm = TRUE),
    party_affiliation_mean = mean(party_affiliation_t1, na.rm = TRUE),
    party_affiliation_sd = sd(party_affiliation_t1, na.rm = TRUE),
    party_affiliation_republican_pct = mean(party_affiliation_raw == 1, na.rm = TRUE) * 100,
    party_affiliation_democrat_pct = mean(party_affiliation_raw == 2, na.rm = TRUE) * 100,
    party_affiliation_other_pct = mean(party_affiliation_raw == 3, na.rm = TRUE) * 100,
    party_affiliation_none_pct = mean(party_affiliation_raw == 4, na.rm = TRUE) * 100,
    authoritarianism_mean = mean(authoritarianism, na.rm = TRUE),
    authoritarianism_sd = sd(authoritarianism, na.rm = TRUE),
    presidential_support_mean = mean(presidential_support, na.rm = TRUE),
    presidential_support_sd = sd(presidential_support, na.rm = TRUE),
    presidential_support_trump_pct = mean(presidential_support_raw == 1, na.rm = TRUE) * 100,
    presidential_support_biden_pct = mean(presidential_support_raw == 2, na.rm = TRUE) * 100,
    presidential_support_other_pct = mean(presidential_support_raw == 3, na.rm = TRUE) * 100
  )

print(iv_desc)
