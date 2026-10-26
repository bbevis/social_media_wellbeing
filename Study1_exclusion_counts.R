#############################################
# Study 1 exclusion counts
#############################################

appusage_weekly <- read.csv("appusage_weekly.csv")
weekly <- read.csv("weekly.csv")
monthly <- read.csv("monthly.csv")

n_inaccurate_participant_weeks <- sum(
  appusage_weekly$accuracy_exclude == 1,
  na.rm = TRUE
)

n_over_20_hour_participants <- length(unique(
  appusage_weekly$id[which(appusage_weekly$appusage_bounds_exclude == 1)]
))

n_low_ema_participant_weeks <- sum(
  weekly$ema_50_exclude == 1,
  na.rm = TRUE
)

n_monthly_participants_excluded <- length(unique(
  monthly$id[which(monthly$monthly_appusage_exclude == 1)]
))

print(n_inaccurate_participant_weeks)
print(n_over_20_hour_participants)
print(n_low_ema_participant_weeks)
print(n_monthly_participants_excluded)
