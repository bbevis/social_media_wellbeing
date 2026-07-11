# Social Media Well-Being Analysis

This repository contains the R code and output files for a research project on social media use, political orientation, and well-being.

If you want to replicate the project, this README is designed to help you:

- install the software you need
- understand the folder structure
- run the analysis in the right order
- know which files are inputs and which are outputs
- avoid the main issues that can break a rerun

The most useful file to look at first is `Content.R`. It shows the intended script order for the project and is the best high-level guide to how the repository is meant to be run.

## Before You Start

This is not an R package or a one-click pipeline. It is a collection of R scripts that are meant to be run from the project folder in a specific order.

You do not need to understand all of the code before running it, but you do need:

- `R`
- `RStudio`
- the raw data files in `Data/`

Recommended setup:

1. Install a recent version of R.
2. Install a recent version of RStudio.
3. Open `Analysis.Rproj` in RStudio.
4. Make sure your working directory is the repository root.
5. Open `Content.R` and use it as your guide to the analysis workflow.

## Package Requirements

Install the required R packages before running the scripts.

Core packages:

- `dplyr`
- `tidyr`
- `purrr`
- `readr`
- `stringr`
- `lubridate`
- `tibble`
- `ggplot2`

Modeling packages:

- `lme4`
- `lmerTest`
- `mediation`
- `mlma`
- `multilevelmediation`
- `boot`
- `psych`
- `pwr`
- `performance`
- `WebPower`

Parallel and utility packages:

- `future`
- `future.apply`
- `furrr`
- `progressr`
- `tictoc`

Plotting and output packages:

- `ggstance`
- `ggh4x`
- `ggrepel`
- `wesanderson`
- `gt`
- `webshot2`
- `chromote`
- `magick`

You can install them in R with:

```r
install.packages(c(
  "boot", "chromote", "dplyr", "furrr", "future", "future.apply",
  "ggplot2", "ggh4x", "ggrepel", "ggstance", "gt", "lme4", "lmerTest",
  "lubridate", "magick", "mediation", "mlma", "multilevelmediation",
  "performance", "progressr", "psych", "purrr", "pwr", "readr",
  "stringr", "tibble", "tictoc", "tidyr", "WebPower", "webshot2",
  "wesanderson"
))
```

## What This Project Contains

The repository mixes source scripts and generated output files in one folder.

Important folders and files:

- `Data/`
  Raw study data used by the scripts
- `Plots/`
  Exported figures and table images
- `Analysis.Rproj`
  RStudio project file
- `Content.R`
  The main guide to script order and the best place to start when learning how the project fits together
- `Monthly_build.R`, `Weekly_build.R`, `Momentary_build.R`
  Scripts that create analysis-ready datasets
- `Monthly_analysis.R`, `Weekly_analysis.R`, `Momentary_analysis.R`
  Scripts that run the main statistical models
- `master_analysis.R`
  Optional helper for running batches of analysis scripts with automatic output filenames
- `Demographics.R`, `Desc_stats*.R`
  Descriptive summary scripts
- `fig_*.R`
  Figure scripts
- `Table_S*.R`
  Supplementary table scripts

## Raw Input Data

The raw data are stored in `Data/`. The main inputs are:

- `coco_ut1_presurvey.csv`
- `coco_ut1_postsurvey.csv`
- `coco_ut1_ema_survey.csv`
- `coco_ut1_daily_survey.csv`
- `coco_ut1_appusage_survey.csv`
- `coco_ut1_appusage_survey_health.csv`
- `coco_ut2_presurvey.csv`
- `coco_ut2_postsurvey.csv`
- `coco_ut2_ema_survey.csv`
- `coco_ut2_daily_survey.csv`
- `coco_ut2_appusage_survey.csv`
- `coco_ut2_appusage_survey_health.csv`
- `demographics_fall_anonymized.csv`
- `demographics_spring_anonymized.csv`

There is also a codebook in `Data/38912-0001-Codebook-ICPSR.pdf`.

## Main Outputs

The most important generated datasets are:

- `monthly.csv`
- `weekly.csv`
- `momentary.csv`

The main model results are written to files such as:

- `monthly_results_*.csv`
- `weekly_results_*.csv`
- `momentary_results_*.csv`

Figures and exported tables are written to `Plots/`.

## How To Run The Project

If you are trying to replicate the analysis, start with `Content.R`.

`Content.R` is valuable because it shows the intended order of the major scripts. Even if you do not run `Content.R` directly, you should refer to it while running the project.

You do not need to run every script in the repository. In many cases, you only need:

- the main build scripts
- the main analysis scripts
- any descriptive, figure, or table scripts relevant to the output you want

Use the sequence in `Content.R` as the reference point for deciding what to run.

### Recommended approach

1. Open `Content.R`.
2. Identify which part of the project you want to reproduce.
3. Run only the scripts needed for that part, in the same general order shown in `Content.R`.

### Typical order

If you want the main analyses, use this order.

### Step 1: Build the datasets

Run these scripts first:

1. `Monthly_build.R`
2. `Weekly_build.R`
3. `Momentary_build.R`

These scripts read the raw files in `Data/` and create the main derived datasets:

- `monthly.csv`
- `weekly.csv`
- `momentary.csv`

They also create some helper files such as:

- `appusage_monthly.csv`
- `appusage_weekly.csv`
- `appusage_raw.csv`

### Step 2: Run the main analyses

After the build scripts finish, you can either run the analysis scripts manually:

1. `Monthly_analysis.R`
2. `Weekly_analysis.R`
3. `Momentary_analysis.R`

or use `master_analysis.R` to run batches. It chooses output filenames from the requested time level, IV, controls setting, and weekly EMA threshold. EMA thresholds apply only to weekly models; monthly models are run once per IV/control setting.

Important:

- `Momentary_analysis.R` is the slowest script and may run for hours.
- `Weekly_analysis.R` can also be slow because it runs many multilevel mediation models in parallel.
- The individual analysis scripts can still be run manually one at a time.
- `master_analysis.R` may contain an uncommented batch run; check it before sourcing.
- Some analysis scripts have multiple possible output file names, controlled by variables near the top of the script or by `master_analysis.R`.

### Step 3: Run descriptive summaries, figures, and tables if needed

If you want the descriptive outputs, main figures, or supplementary tables, run these after the analyses:

- `Demographics.R`
- `Desc_stats_iv.R`
- `Desc_stats_mediators.R`
- `Desc_stats_outcomes.R`
- `fig_*.R`
- `Table_S*.R`

These scripts generate summary CSVs and PNG exports in `Plots/`.

## Minimal Replication Path

If you only want the main analysis outputs, you do not need to run everything else in the repository.

Use this minimal order:

1. `Monthly_build.R`
2. `Weekly_build.R`
3. `Momentary_build.R`
4. `Monthly_analysis.R`, `Weekly_analysis.R`, and/or `Momentary_analysis.R`

For repeated model runs, use:

4. `master_analysis.R`

The current batch workflow supports running weekly and monthly models for all IVs with controls, while applying weekly EMA thresholds only to weekly models.

If you only care about one time scale, you can run just the matching pair:

- monthly: `Monthly_build.R` then `Monthly_analysis.R`
- weekly: `Weekly_build.R` then `Weekly_analysis.R`
- momentary: `Momentary_build.R` then `Momentary_analysis.R`

## What The Three Analysis Levels Mean

- `Monthly`
  Uses post-survey outcomes and app-use measures aggregated to the monthly level
- `Weekly`
  Uses weekly averages of EMA well-being and weekly app-use reports
- `Momentary`
  Uses individual EMA observations and past-hour social media use

You do not need to understand those distinctions in detail to run the code, but they explain why there are three separate build scripts and three separate analysis scripts.

## Existing Outputs

This repository already contains many generated CSV and PNG files.

Before rerunning anything, it is worth checking whether the output you need already exists. This can save a lot of time, especially for `Momentary_analysis.R`.

## Known Problems Before Rerunning

There are a few issues in the current repository that matter for replication.

- Some scripts use `data/...` while the folder in the repository is `Data/`.
- Some scripts save to `plots/...` while the folder in the repository is `Plots/`.
- `Momentary_build.R` creates a variable called `num_ema_completed`, but `Momentary_analysis.R` expects `surveys_per_id`.
- `Content.R` is helpful as a guide, but it is not a fully reliable one-click driver script.

These issues may not always fail on macOS because file paths can be case-insensitive, but they can fail on other systems or during a clean rerun.

## Suggested Way To Use `Content.R`

Treat `Content.R` as the roadmap for the project.

It is the quickest way to understand:

- which scripts are central
- the intended run order
- which scripts are optional downstream outputs

You can run scripts individually, but it is still advisable to keep `Content.R` open and use it as the reference for sequence and scope.

## Reproducibility Notes

This repository does not currently include:

- an `renv.lock` file
- a package management workflow
- a formal pipeline tool such as `targets`
- a clean separation between source files and generated outputs

That means replication is possible, but it is not yet fully automated.

## If You Are Sharing Or Cleaning Up This Project

The highest-value improvements would be:

1. standardize all path names to `Data/` and `Plots/`
2. fix the `num_ema_completed` versus `surveys_per_id` mismatch
3. add `renv` to lock package versions
4. separate generated outputs from source code
5. create a single reproducible driver script or pipeline
