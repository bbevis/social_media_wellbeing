
############ Load data

monthly_df = read.csv("monthly.csv")
weekly_df = read.csv("weekly.csv")
momentary_df = read.csv("momentary.csv")


##############################################
# Monthly level

# List of DVs and mediator exclusion vars
DVs <- c("swls_t2", "uls_t2", "awb_t2")
mediator_exclude_vars <- grep("_exclude$", names(monthly_df), value = TRUE)

# Create long-format summary table
exclusion_summary <- monthly_df %>%
  dplyr::select(id, all_of(DVs), all_of(mediator_exclude_vars)) %>%
  pivot_longer(cols = all_of(mediator_exclude_vars), names_to = "Mediator", values_to = "Excluded") %>%
  pivot_longer(cols = all_of(DVs), names_to = "DV", values_to = "DV_value") %>%
  # filter(!is.na(DV_value)) %>%  # Only consider valid DV observations
  group_by(DV, Mediator) %>%
  summarise(
    n_total = n(),
    n_excluded = sum(Excluded, na.rm = TRUE),
    percent_excluded = round(100 * n_excluded / n_total, 1),
    .groups = "drop"
  )

# write.csv(exclusion_summary, 'exclusion_summary_monthly.csv', row.names = FALSE)