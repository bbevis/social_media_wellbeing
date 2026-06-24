

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
############ Load data

monthly_df = read.csv("monthly.csv")
weekly_df = read.csv("weekly.csv")
momentary_df = read.csv("momentary.csv")


#############################################
# Social media
#############################################

# Political orientation: mean and spread
mean_pol <- mean(monthly_df$political_orientation, na.rm = TRUE)
sd_pol   <- sd(monthly_df$political_orientation, na.rm = TRUE)

# Age
mean_age <- mean(monthly_df$age, na.rm = TRUE)
sd_age   <- sd(monthly_df$age, na.rm = TRUE)

# Gender breakdown
gender_counts <- monthly_df %>% count(gender)

# Ethnicity breakdown
ethnicity_counts <- monthly_df %>% count(ethnicity)

# Quick textual report
cat("Participants had a mean political orientation of", round(mean_pol,2),
    "(SD =", round(sd_pol,2), ").\n")
cat("The average age was", round(mean_age,1), "years (SD =", round(sd_age,1), ").\n")
cat("Gender distribution:\n"); print(gender_counts)
cat("Ethnicity distribution:\n"); print(ethnicity_counts)


####
# Total participants before exclusions
momentary_df %>%
  summarise(parts = n_distinct(id))

monthly_df %>%
  summarise(parts = n_distinct(id))

weekly_df %>%
  summarise(parts = n_distinct(id))



# Total participants after exclusions
weekly_df %>%
  dplyr::filter(ema_50_exclude == 0) %>%
  summarise(parts = n_distinct(id))

####
# DVs
momentary_df %>%
  dplyr::select(id, awb_momentary,angry,worried,happy,sad,stressed,lonely,accepted) %>%
  pivot_longer(
    cols = -id,
    names_to = "DV",
    values_to = "DV_value"
  ) %>%
  group_by(DV) %>%
  summarise(parts = n_distinct(id))

momentary_df %>%
  distinct(id) %>%
  nrow()


weekly_df %>%
  distinct(id) %>%
  nrow()

monthly_df %>%
  distinct(id) %>%
  nrow()

weekly_df %>%
  dplyr::select(id, awb_week,angry,worried,happy,sad,stressed,lonely,accepted) %>%
  pivot_longer(
    cols = -id,
    names_to = "DV",
    values_to = "DV_value"
  ) %>%
  group_by(DV) %>%
  summarise(parts = n_distinct(id))


vars <- c("awb_week","angry","worried","happy","sad","stressed","lonely","accepted")

result <- lapply(vars, function(v) {
  data.frame(
    DV    = v,
    parts = dplyr::n_distinct(weekly_df$id[!is.na(weekly_df[[v]])])
  )
}) %>% dplyr::bind_rows()


monthly_df %>%
  dplyr::select(id, swls_t2,uls_t2,awb_t2) %>%
  pivot_longer(
    cols = -id,
    names_to = "DV",
    values_to = "DV_value"
  ) %>%
  group_by(DV) %>%
  summarise(parts = n_distinct(id))


vars <- c("swls_t2","uls_t2","awb_t2")

result <- lapply(vars, function(v) {
  data.frame(
    DV    = v,
    parts = dplyr::n_distinct(monthly_df$id[!is.na(monthly_df[[v]])])
  )
}) %>% dplyr::bind_rows()
####
# 
political_orientation %>%
  summarise(parts = n_distinct(id))








