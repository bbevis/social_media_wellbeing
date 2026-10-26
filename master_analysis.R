#############################################
#
#       Master analysis runner
#
#############################################

# Use this script to run batches of Monthly_analysis.R, Weekly_analysis.R,
# and Momentary_analysis.R without manually changing filenames in each script.
#
# Edit the run blocks immediately below. They use the same run_analysis_batch()
# The helper functions below just make those calls work and
# choose the correct output filenames.
#
# Important:
#   - EMA exclusion choices (e.g., 40% and 60%) only apply to weekly models.
#   - Individual analysis scripts can still be run manually on their own.


#############################################
# edit this section

run_requested_analyses <- function() {

  # Run main models only
  # run_analysis_batch(
  #   levels = c("momentary", "weekly", "monthly"),
  #   analysis_ivs = c(
  #     "political_orientation"
  #   ),
  #   include_controls = TRUE
  # )
  # 
  run_analysis(
    level = "weekly",
    analysis_iv = "political_orientation",
    include_controls = TRUE,
    save_file_name = "weekly_results_withcontrols_nonuse_zero.csv"
  )

  # Run alternative IVs models only
  # run_analysis_batch(
  #   levels = c("weekly", "monthly", "momentary"),
  #   analysis_ivs = c(
  #     "authoritarianism",
  #     "party_affiliation_t1",
  #     "presidential_support"
  #   ),
  #   include_controls = TRUE
  # )

  # Run alternative exclusion rules models only
  # run_analysis_batch(
  #   levels = c("weekly"),
  #   analysis_ivs = c(
  #     "political_orientation"
  #   ),
  #   include_controls = TRUE,
  #   ema_exclude_cols = c("ema_40_exclude", "ema_60_exclude")
  # )
}


#############################################
# Helper functions: normally do not edit

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
                         control_vars = c("gender", "ethnicity", "ses")) {
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


#############################################
# Extra examples

# Run one analysis:
# run_analysis(level = "weekly", analysis_iv = "political_orientation")

# Run weekly and monthly primary analyses, with and without demographic controls:
# run_analysis_batch(
#   levels = c("weekly", "monthly"),
#   analysis_ivs = "political_orientation",
#   include_controls = c(FALSE, TRUE)
# )

# Run all time levels for authoritarianism:
# run_analysis_batch(
#   levels = c("momentary", "weekly", "monthly"),
#   analysis_ivs = "authoritarianism",
#   include_controls = FALSE
# )

# Run weekly EMA-threshold robustness analyses:
# run_analysis_batch(
#   levels = "weekly",
#   analysis_ivs = "political_orientation",
#   include_controls = FALSE,
#   ema_exclude_cols = c("ema_40_exclude", "ema_50_exclude", "ema_60_exclude")
# )
