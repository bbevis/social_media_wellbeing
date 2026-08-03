#############################################
#
#       Study 1 standalone path a models
#
#############################################

library(dplyr)
library(tidyr)
library(lmerTest)
library(performance)

##############################################
# Parameters

path_a_iv <- "political_orientation"
path_a_controls <- c("gender", "ethnicity", "ses")
path_a_ci_level <- 0.95
path_a_save_file_name <- "study1_path_a_results.csv"

# These are the mediators reported across the Study 1 H2 figure and tables.
monthly_weekly_mediators <- c(
  "facebook",
  "twitter",
  "tiktok",
  "instagram",
  "youtube",
  "reddit",
  "snapchat",
  "information",
  "social",
  "productivity",
  "education",
  "entertainment",
  "creativity",
  "games",
  "shopping",
  "utilities",
  "other",
  "health",
  "total_platform",
  "total_activity"
)

momentary_mediators <- "sm_minutes"

##############################################
# Load data

monthly_path_a_data <- read.csv("monthly.csv")
weekly_path_a_data <- read.csv("weekly.csv")
momentary_path_a_data <- read.csv("momentary.csv")

##############################################

monthly_path_a_stats <- function(model, term) {
  model_summary <- summary(model)
  coefficient <- model_summary$coefficients[term, ]
  interval <- as.numeric(
    confint(model, parm = term, level = path_a_ci_level)
  )

  list(
    estimate = unname(coefficient[1]),
    se = unname(coefficient[2]),
    statistic = unname(coefficient[3]),
    p = unname(coefficient[4]),
    df = unname(model_summary$df[2]),
    r2 = unname(model_summary$r.squared),
    adjusted_r2 = unname(model_summary$adj.r.squared),
    r2_marginal = NA_real_,
    r2_conditional = NA_real_,
    ci_low = interval[1],
    ci_high = interval[2]
  )
}

mixed_path_a_stats <- function(model, term) {
  coefficient <- summary(model)$coefficients[term, ]
  term_df <- unname(coefficient[3])
  critical_t <- qt((1 + path_a_ci_level) / 2, df = term_df)
  model_r2 <- performance::r2(model)

  list(
    estimate = unname(coefficient[1]),
    se = unname(coefficient[2]),
    statistic = unname(coefficient[4]),
    p = unname(coefficient[5]),
    df = term_df,
    r2 = NA_real_,
    adjusted_r2 = NA_real_,
    r2_marginal = unname(model_r2$R2_marginal),
    r2_conditional = unname(model_r2$R2_conditional),
    ci_low = unname(coefficient[1] - critical_t * coefficient[2]),
    ci_high = unname(coefficient[1] + critical_t * coefficient[2])
  )
}

make_path_a_row <- function(level, mediator, model_type, model_data, stats) {
  data.frame(
    level = level,
    mediator = mediator,
    IV = path_a_iv,
    model_type = model_type,
    n = nrow(model_data),
    n_subjects = dplyr::n_distinct(model_data$id),
    estimate = stats$estimate,
    se = stats$se,
    t = stats$statistic,
    p = stats$p,
    df = stats$df,
    r2 = stats$r2,
    adjusted_r2 = stats$adjusted_r2,
    r2_marginal = stats$r2_marginal,
    r2_conditional = stats$r2_conditional,
    ci_low = stats$ci_low,
    ci_high = stats$ci_high
  )
}

run_monthly_path_a <- function(mediator) {
  exclusion_col <- paste0(mediator, "_exclude")
  required_vars <- c(
    "id",
    mediator,
    path_a_iv,
    "term",
    path_a_controls,
    exclusion_col
  )

  model_data <- monthly_path_a_data %>%
    dplyr::select(all_of(required_vars)) %>%
    filter(.data[[exclusion_col]] == 0) %>%
    tidyr::drop_na()

  if (nrow(model_data) < 50) return(NULL)

  model_data$mediator_z <- as.numeric(scale(model_data[[mediator]]))
  formula.M <- as.formula(
    paste(
      "mediator_z ~",
      path_a_iv,
      "+ term +",
      paste(path_a_controls, collapse = " + ")
    )
  )
  model.M <- lm(formula.M, data = model_data)

  make_path_a_row(
    level = "Monthly",
    mediator = mediator,
    model_type = "linear",
    model_data = model_data,
    stats = monthly_path_a_stats(model.M, path_a_iv)
  )
}

run_weekly_path_a <- function(mediator) {
  exclusion_col <- paste0(mediator, "_exclude")
  required_vars <- c(
    "id",
    mediator,
    path_a_iv,
    "term",
    "week",
    "n_appusage_surveys_completed",
    "post_election",
    path_a_controls,
    "ema_50_exclude",
    exclusion_col
  )

  model_data <- weekly_path_a_data %>%
    dplyr::select(all_of(required_vars)) %>%
    filter(
      ema_50_exclude == 0,
      .data[[exclusion_col]] == 0
    ) %>%
    tidyr::drop_na()

  if (nrow(model_data) < 50) return(NULL)

  model_data$level_id <- as.numeric(factor(model_data$id))
  model_data$mediator_z <- as.numeric(scale(model_data[[mediator]]))
  formula.M <- as.formula(
    paste(
      "mediator_z ~",
      path_a_iv,
      "+ term + week + n_appusage_surveys_completed + post_election +",
      paste(path_a_controls, collapse = " + "),
      "+ (1 | level_id)"
    )
  )
  model.M <- lmerTest::lmer(formula.M, data = model_data)

  make_path_a_row(
    level = "Weekly",
    mediator = mediator,
    model_type = "mixed",
    model_data = model_data,
    stats = mixed_path_a_stats(model.M, path_a_iv)
  )
}

run_momentary_path_a <- function(mediator) {
  required_vars <- c(
    "id",
    mediator,
    path_a_iv,
    "term",
    "week",
    "surveys_per_id",
    "post_election",
    path_a_controls
  )

  model_data <- momentary_path_a_data %>%
    dplyr::select(all_of(required_vars)) %>%
    tidyr::drop_na()

  if (nrow(model_data) < 50) return(NULL)

  model_data$level_id <- as.numeric(factor(model_data$id))
  model_data$mediator_z <- as.numeric(scale(model_data[[mediator]]))
  formula.M <- as.formula(
    paste(
      "mediator_z ~",
      path_a_iv,
      "+ term + week + surveys_per_id + post_election +",
      paste(path_a_controls, collapse = " + "),
      "+ (1 | level_id)"
    )
  )
  model.M <- lmerTest::lmer(formula.M, data = model_data)

  make_path_a_row(
    level = "Momentary",
    mediator = mediator,
    model_type = "mixed",
    model_data = model_data,
    stats = mixed_path_a_stats(model.M, path_a_iv)
  )
}

##############################################
# Run models

study1_path_a_results <- bind_rows(
  lapply(monthly_weekly_mediators, run_monthly_path_a),
  lapply(monthly_weekly_mediators, run_weekly_path_a),
  lapply(momentary_mediators, run_momentary_path_a)
)

write.csv(
  study1_path_a_results,
  path_a_save_file_name,
  row.names = FALSE
)

print(study1_path_a_results)
