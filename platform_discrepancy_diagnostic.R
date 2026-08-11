
library(dplyr)
library(tidyr)

platforms <- c("reddit", "tiktok", "twitter")

s1 <- read.csv("monthly.csv")
s1_raw <- read.csv("appusage_raw.csv")
s2 <- read.csv("anes_w3.csv")

s1_a <- read.csv("study1_path_a_results.csv")
s1_med <- read.csv("monthly_results_withcontrols.csv")
s2_med <- read.csv("anes_w3_mediation_results.csv")

po_group <- function(x) {
  case_when(
    x %in% 1:2 ~ "Liberal",
    x %in% 3:5 ~ "Moderate",
    x %in% 6:7 ~ "Conservative",
    TRUE ~ NA_character_
  )
}

print_p <- function(x) {
  ifelse(x < .001, "< .001", sprintf("%.3f", x))
}

# Existing path-a results

path_a_results <- bind_rows(
  s1_a %>%
    filter(level == "Monthly", mediator %in% platforms) %>%
    transmute(
      sample = "Study 1",
      platform = mediator,
      n,
      beta = estimate,
      ci_low,
      ci_high,
      p
    ),
  s2_med %>%
    filter(
      model_type == "mediation",
      outcome == "loneliness",
      mediator %in% paste0(platforms, "_frequency"),
      sample %in% c("all", "young_adults", "older_adults")
    ) %>%
    transmute(
      sample = recode(
        sample,
        all = "Study 2: all",
        young_adults = "Study 2: 18-29",
        older_adults = "Study 2: 30+"
      ),
      platform = sub("_frequency$", "", mediator),
      n,
      beta = a_path_est,
      ci_low = a_path_ci_low,
      ci_high = a_path_ci_high,
      p = a_path_p
    )
) %>%
  mutate(
    across(c(beta, ci_low, ci_high), ~ round(.x, 3)),
    p = print_p(p)
  ) %>%
  arrange(platform, sample)

print(as_tibble(path_a_results), n = Inf, width = Inf)

# Existing path-b results

path_b_results <- bind_rows(
  s1_med %>%
    filter(
      mediator %in% platforms,
      outcome %in% c("swls_t2", "uls_t2", "awb_t2")
    ) %>%
    transmute(
      sample = "Study 1",
      platform = mediator,
      outcome = recode(
        outcome,
        swls_t2 = "life satisfaction",
        uls_t2 = "loneliness",
        awb_t2 = "affective wellbeing"
      ),
      n,
      beta = path_b,
      ci_low = path_b_CI_lower,
      ci_high = path_b_CI_upper,
      p = path_b_p
    ),
  s2_med %>%
    filter(
      model_type == "mediation",
      mediator %in% paste0(platforms, "_frequency"),
      outcome %in% c("loneliness", "anhedonia", "depression"),
      sample %in% c("all", "young_adults", "older_adults")
    ) %>%
    transmute(
      sample = recode(
        sample,
        all = "Study 2: all",
        young_adults = "Study 2: 18-29",
        older_adults = "Study 2: 30+"
      ),
      platform = sub("_frequency$", "", mediator),
      outcome,
      n = b_path_df + 1,
      beta = b_path_est,
      ci_low = b_path_ci_low,
      ci_high = b_path_ci_high,
      p = b_path_p
    )
) %>%
  mutate(
    across(c(beta, ci_low, ci_high), ~ round(.x, 3)),
    p = print_p(p)
  ) %>%
  arrange(platform, sample, outcome)

print(as_tibble(path_b_results), n = Inf, width = Inf)

# Who uses each platform?

get_s1_adoption <- function(platform_name) {
  no_use <- s1_raw[, paste0("no_", platform_name, "_w", 1:4)] %>%
    mutate(across(everything(), ~ as.numeric(as.character(.x))))
  hours <- s1_raw[, paste0("hours_", platform_name, "_w", 1:4)]
  minutes <- s1_raw[, paste0("minutes_", platform_name, "_w", 1:4)]

  has_use_report <- rowSums(!is.na(hours) | !is.na(minutes)) > 0
  explicitly_no_app <- rowSums(no_use == 1, na.rm = TRUE) > 0
  any_use <- case_when(
    has_use_report ~ 1,
    explicitly_no_app ~ 0,
    TRUE ~ NA_real_
  )

  tibble(id = s1_raw$id, platform = platform_name, any_use)
}

s1_adoption <- bind_rows(lapply(platforms, get_s1_adoption)) %>%
  left_join(
    s1 %>% select(
      id, political_orientation, age, gender, ethnicity, ses, term,
      monthly_appusage_exclude
    ),
    by = "id"
  ) %>%
  filter(monthly_appusage_exclude == 0) %>%
  mutate(po_group = po_group(political_orientation))

s2_long <- bind_rows(
  s2 %>% mutate(sample = "Study 2: all"),
  s2 %>% filter(age >= 18, age <= 29) %>% mutate(sample = "Study 2: 18-29"),
  s2 %>% filter(age >= 30) %>% mutate(sample = "Study 2: 30+")
) %>%
  mutate(
    gender = factor(gender),
    race_ethnicity = factor(race_ethnicity),
    income = factor(income),
    education = factor(education),
    po_group = po_group(political_orientation)
  ) %>%
  pivot_longer(
    all_of(paste0(platforms, "_frequency")),
    names_to = "platform",
    values_to = "use"
  ) %>%
  mutate(
    platform = sub("_frequency$", "", platform),
    any_use = case_when(use == 1 ~ 0, use > 1 ~ 1, TRUE ~ NA_real_)
  )

adoption_results <- bind_rows(
  s1_adoption %>%
    filter(!is.na(any_use), !is.na(po_group)) %>%
    group_by(platform, po_group) %>%
    summarise(
      sample = "Study 1",
      n = n(),
      percent_users = 100 * mean(any_use),
      .groups = "drop"
    ),
  s2_long %>%
    filter(!is.na(any_use), !is.na(po_group)) %>%
    group_by(sample, platform, po_group) %>%
    summarise(
      n = n(),
      percent_users = 100 * weighted.mean(any_use, w3weight, na.rm = TRUE),
      .groups = "drop"
    )
) %>%
  mutate(percent_users = round(percent_users, 1)) %>%
  arrange(platform, sample, factor(
    po_group,
    levels = c("Liberal", "Moderate", "Conservative")
  ))

print(as_tibble(adoption_results), n = Inf, width = Inf)

# How much do users use each platform?

s1_use <- s1 %>%
  select(
    id, political_orientation, age, gender, ethnicity, ses, term,
    monthly_appusage_exclude, all_of(platforms)
  ) %>%
  pivot_longer(all_of(platforms), names_to = "platform", values_to = "use") %>%
  left_join(
    s1_adoption %>% select(id, platform, any_use),
    by = c("id", "platform")
  ) %>%
  mutate(po_group = po_group(political_orientation))

users_only_results <- bind_rows(
  s1_use %>%
    filter(any_use == 1, !is.na(use), !is.na(po_group)) %>%
    group_by(platform, po_group) %>%
    summarise(
      sample = "Study 1",
      n_users = n(),
      mean_use = mean(use),
      median_use = median(use),
      .groups = "drop"
    ),
  s2_long %>%
    filter(any_use == 1, !is.na(po_group)) %>%
    group_by(sample, platform, po_group) %>%
    summarise(
      n_users = n(),
      mean_use = weighted.mean(use, w3weight, na.rm = TRUE),
      median_use = median(use),
      .groups = "drop"
    )
) %>%
  mutate(across(c(mean_use, median_use), ~ round(.x, 2))) %>%
  arrange(platform, sample, factor(
    po_group,
    levels = c("Liberal", "Moderate", "Conservative")
  ))

print(as_tibble(users_only_results), n = Inf, width = Inf)
