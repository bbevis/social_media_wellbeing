#############################################
#
#       Run code in order of appearance
#       
#############################################

# Study 1
# Build datasets
# source("Monthly_build.R")
# source("Weekly_build.R")
# source("Momentary_build.R")

# Exploratory descriptive stats
source("Demographics.R")
source("Desc_stats_iv.R")
source("Desc_stats_mediators.R")
source("Desc_stats_outcomes.R")

# Mediation Analyses
# For batch runs with automatic output filenames, use master_analysis.R.
# The individual analysis scripts can still be run manually one at a time.
# source("master_analysis.R") ## WARNING. Batch runtime can last for hours. Check master_analysis.R before running.
source("Monthly_analysis.R")
source("Weekly_analysis.R")
source("Momentary_analysis.R") ## WARNING. RUNTIME LASTS FOR HOURS/DAYS!! ##
source("Study1_path_a_analysis.R") # Standalone H2 path a models

# Study 2
source("ANES_build.R")
source("ANES_monotonic_analysis.R") # Ordinal categorical mediation with survey weights, 1,000 bootstrap replicates and age moderation (36 calls)

# Plots
source("fig_PO_wellbeing_effects.R") # H1 path c
source("fig_PO_mediators.R") # H2 path a
source("fig_b_path_by_outcome_facets.R") # H3 path b
source("fig_ACME.R") # H4
source("fig_usage_effects.R") # H4
source("fig_ANES_social_media_distribution.R") # Frequency by age by platform
# source("fig_ANES_PO_wellbeing_effects.R") # Uses older numeric-analysis outputs; update before enabling
# source("fig_ANES_PO_mediators.R") # Uses older numeric-analysis outputs; update before enabling
# source("fig_ANES_b_path_by_outcome_facets.R") # Uses older numeric-analysis outputs; update before enabling
# source("fig_ANES.R") # Uses older numeric-analysis outputs; update before enabling
# source("fig_ANES_age_moderation.R") # Uses older numeric-analysis outputs; update before enabling

# Supplementary materials
source("Table_S2.R")
source("Table_S3.R")
source("Table_S4.R")
source("Table_S5.R")
source("Table_S6.R")
source("fig_b_path_by_outcome_facets.R")
source("Table_S7.R")
source("Table_S8.R")
# source("fig_ANES_b_path_by_outcome_facets.R") # Uses older numeric-analysis outputs; update before enabling
source("Table_S9.R")
source("Table_S10.R")
source("Table_S11.R")

# ANES data
source("Table_S12.R")
# source("fig_ANES_PO_wellbeing_effects.R") # Uses older numeric-analysis outputs; update before enabling
# source("fig_ANES_PO_mediators.R") # Uses older numeric-analysis outputs; update before enabling
source("Table_S13.R") # Age associations (ordinal logistic regression for social media use)
source("Table_S14.R") # Political orientation -> wellbeing
# source("fig_ANES_b_path_by_outcome_facets.R") # Uses older numeric-analysis outputs; update before enabling
source("Table_S15.R") # Political orientation -> social media use (ordinal logistic regression)
source("Table_S16.R") # Social media use -> wellbeing (frequency-category contrasts)
source("Table_S17.R") # ACME mediation results

