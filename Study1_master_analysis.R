#############################################
#
#       Master analysis runner
#
#############################################

# Use this script to run batches of Monthly_analysis.R, Weekly_analysis.R,
# and Momentary_analysis.R
#
# The explicit run_analysis() calls below specify the three primary analyses
# for reproducing the submitted results. Edit their arguments to change the
# analysis level, IV, controls, or other settings.
#
# Important:
#   - EMA exclusion choices (e.g., 40% and 60%) only apply to weekly models.
#   - Individual analysis scripts can still be run manually on their own.
#   - Sourcing this file runs ALL analyses and writes their outputs.
#   - Beware of run time. Weekly and momentary bootstrap analyses can take hours or days.


#############################################
# EDIT THIS SECTION

run_requested_analyses <- function() {

  # PRIMARY ANALYSES REPORTED IN THE MANUSCRIPT

  # Monthly primary analysis: political orientation -> media use -> wellbeing.
  # Input: monthly.csv. Includes gender, ethnicity, SES, and semester controls;
  # baseline wellbeing is not included. Uses 1,000 bootstrap resamples and 95% CIs.
  # Output: monthly_results_withcontrols.csv.
  run_analysis(
    level = "monthly",
    analysis_iv = "political_orientation",
    include_controls = TRUE,
    include_Ypre = FALSE,
    control_vars = c("gender", "ethnicity", "ses"),
    n_sims = 1000,
    ci_level = 0.95,
    save_file_name = "monthly_results_withcontrols.csv"
  )

  # Weekly primary analysis: multilevel political orientation -> media use ->
  # wellbeing models. Input: weekly.csv; applies the primary 50% EMA threshold.
  # Requests gender, ethnicity, and SES controls, 1,000 bootstrap resamples,
  # and 95% CIs. The weekly script specifies the remaining model covariates.
  # Output: weekly_results_withcontrols.csv.
  run_analysis(
    level = "weekly",
    analysis_iv = "political_orientation",
    include_controls = TRUE,
    control_vars = c("gender", "ethnicity", "ses"),
    ema_exclude_col = "ema_50_exclude",
    n_sims = 1000,
    ci_level = 0.95,
    save_file_name = "weekly_results_withcontrols.csv"
  )

  # Momentary primary analysis: multilevel political orientation -> social media
  # use -> wellbeing models. Input: momentary.csv (named Momentary.csv locally).
  # Requests gender, ethnicity, and SES controls, 1,000 bootstrap resamples,
  # and 95% CIs. The momentary script specifies the remaining model covariates.
  # Output: momentary_results_withcontrols.csv.
  run_analysis(
    level = "momentary",
    analysis_iv = "political_orientation",
    include_controls = TRUE,
    control_vars = c("gender", "ethnicity", "ses"),
    n_sims = 1000,
    ci_level = 0.95,
    save_file_name = "momentary_results_withcontrols.csv"
  )

  #############################################
  # REPORTED SUPPLEMENTARY ANALYSES
  # These reported sensitivity analyses run after the primary analyses.

  # Tables S9 and S10: sensitivity to the weekly EMA completion threshold.
  # Uses weekly.csv, political orientation, and no demographic controls,
  # matching the result files read by the Table S9 and S10 scripts.
  # Other weekly model covariates remain specified in Weekly_analysis.R.
  # Outputs: weekly_results_nocontrols_40ema.csv and
  #          weekly_results_nocontrols_60ema.csv.
  run_analysis_batch(
    levels = "weekly",
    analysis_ivs = "political_orientation",
    include_controls = FALSE,
    ema_exclude_cols = c("ema_40_exclude", "ema_60_exclude")
  )

  # Table S11: sensitivity to coding explicit app non-use as zero rather than NA.
  # Selects weekly_nonuse_zero.csv for this run only.
  # Uses political orientation, demographic controls, and the 50% EMA threshold.
  # Output: weekly_results_withcontrols_nonuse_zero.csv.
  run_analysis(
    level = "weekly",
    weekly_input_file = "weekly_nonuse_zero.csv",
    analysis_iv = "political_orientation",
    include_controls = TRUE,
    ema_exclude_col = "ema_50_exclude",
    save_file_name = "weekly_results_withcontrols_nonuse_zero.csv"
  )

  #############################################
  # ADDITIONAL ANALYSES NOT REPORTED IN THE MANUSCRIPT

  # Alternative IVs: replace political orientation with authoritarianism,
  # party affiliation, or presidential support at each temporal level.
  # Uses the corresponding monthly.csv, weekly.csv, or momentary.csv input,
  # with demographic controls and the primary weekly EMA threshold.
  # Outputs for each level (monthly, weekly, momentary):
  #   <level>_results_withcontrols_authoritarianism.csv
  #   <level>_results_withcontrols_party_affiliation.csv
  #   <level>_results_withcontrols_presidential_support.csv
  # run_analysis_batch(
  #   levels = c("monthly", "weekly", "momentary"),
  #   analysis_ivs = c(
  #     "authoritarianism",
  #     "party_affiliation_t1",
  #     "presidential_support"
  #   ),
  #   include_controls = TRUE
  # )

  # Without demographic controls: compare with the primary adjusted analyses.
  # Uses political orientation and the same inputs/weekly threshold as above.
  # Semester and any other model-specific covariates are still retained;
  # include_controls = FALSE removes only gender, ethnicity, and SES.
  # Outputs: monthly_results_nocontrols.csv,
  #          weekly_results_nocontrols.csv,
  #          momentary_results_nocontrols.csv.
  # Compare these with the primary *_results_withcontrols.csv outputs.
  # run_analysis_batch(
  #   levels = c("monthly", "weekly", "momentary"),
  #   analysis_ivs = "political_orientation",
  #   include_controls = FALSE
  # )

}


#############################################

make_analysis_filename <- function(level,
                                   analysis_iv = "political_orientation",
                                   include_controls = FALSE,
                                   include_Ypre = FALSE,
                                   ema_exclude_col = "ema_50_exclude") {
  level <- match.arg(level, c("monthly", "weekly", "momentary"))
  control_tag <- if (include_controls) "withcontrols" else "nocontrols"
  iv_label <- switch(
    analysis_iv,
    political_orientation = "",
    party_affiliation_t1 = "party_affiliation",
    analysis_iv
  )
  iv_suffix <- if (iv_label == "") "" else paste0("_", iv_label)
  ema_suffix <- ""

  if (level == "weekly" && ema_exclude_col != "ema_50_exclude") {
    ema_suffix <- switch(
      ema_exclude_col,
      ema_40_exclude = "_40ema",
      ema_60_exclude = "_60ema",
      paste0("_", ema_exclude_col)
    )
  }

  paste0(level, "_results_", control_tag, ema_suffix, iv_suffix, ".csv")
}

run_analysis <- function(level,
                         analysis_iv = "political_orientation",
                         include_controls = FALSE,
                         include_Ypre = FALSE,
                         ema_exclude_col = "ema_50_exclude",
                         save_file_name = NULL,
                         n_sims = 1000,
                         ci_level = 0.95,
                         diagnostic_progress = FALSE,
                         control_vars = c("gender", "ethnicity", "ses"),
                         weekly_input_file = "weekly.csv") {
  level <- match.arg(level, c("monthly", "weekly", "momentary"))

  if (is.null(save_file_name)) {
    save_file_name <- make_analysis_filename(
      level = level,
      analysis_iv = analysis_iv,
      include_controls = include_controls,
      include_Ypre = include_Ypre,
      ema_exclude_col = ema_exclude_col
    )
  }

  script <- switch(
    level,
    monthly = "Monthly_analysis.R",
    weekly = "Weekly_analysis.R",
    momentary = "Momentary_analysis.R"
  )

  message("Running ", script, " -> ", save_file_name)

  prev_options <- options(
    coco.weekly_input_file = weekly_input_file,
    coco.analysis_iv = analysis_iv,
    coco.include_controls = include_controls,
    coco.include_Ypre = include_Ypre,
    coco.ema_exclude_col = ema_exclude_col,
    coco.save_file_name = save_file_name,
    coco.n_sims = n_sims,
    coco.ci_level = ci_level,
    coco.diagnostic_progress = diagnostic_progress,
    coco.control_vars = control_vars
  )
  on.exit(options(prev_options), add = TRUE)

  source(script)
  invisible(save_file_name)
}

run_analysis_batch <- function(levels,
                               analysis_ivs = "political_orientation",
                               include_controls = FALSE,
                               include_Ypre = FALSE,
                               ema_exclude_cols = "ema_50_exclude",
                               n_sims = 1000,
                               ci_level = 0.95,
                               diagnostic_progress = FALSE,
                               control_vars = c("gender", "ethnicity", "ses")) {
  runs <- expand.grid(
    level = levels,
    analysis_iv = analysis_ivs,
    include_controls = include_controls,
    include_Ypre = include_Ypre,
    ema_exclude_col = ema_exclude_cols,
    stringsAsFactors = FALSE
  )

  runs <- subset(runs, !(level != "monthly" & include_Ypre))
  runs <- subset(runs, !(level != "weekly" & ema_exclude_col != "ema_50_exclude"))

  output_files <- character(nrow(runs))
  for (i in seq_len(nrow(runs))) {
    output_files[i] <- run_analysis(
      level = runs$level[i],
      analysis_iv = runs$analysis_iv[i],
      include_controls = runs$include_controls[i],
      include_Ypre = runs$include_Ypre[i],
      ema_exclude_col = runs$ema_exclude_col[i],
      n_sims = n_sims,
      ci_level = ci_level,
      diagnostic_progress = diagnostic_progress,
      control_vars = control_vars
    )
  }

  invisible(output_files)
}


#############################################
# Execute selected run blocks

run_requested_analyses()
