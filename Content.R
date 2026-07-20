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

# Study 2
source("ANES_build.R")
source("ANES_analysis.R") # All ANES mediation and age-moderation models; writes the unified results file

# Plots
source("fig_PO_wellbeing_effects.R") # H1 path c
source("fig_PO_mediators.R") # H2 path a
source("fig_b_path_by_outcome_facets.R") # H3 path b
source("fig_ACME.R") # H4
source("fig_usage_effects.R") # H4
source("fig_ANES_social_media_distribution.R") # Frequency by age by platform
source("fig_ANES.R") # Proportion mediated

# Supplementary materials
source("Table_S1.R")
source("Table_S2.R")
source("Table_S3.R")
source("Table_S4.R")
source("Table_S5.R")
source("Table_S6.R")
source("Table_S7.R")
source("Table_S8.R")
source("Table_S9.R")
source("Table_S10.R")
source("Table_S11.R")
source("Table_S12.R")
source("Table_S13.R")
source("Table_S14.R")
source("Table_S15.R")
source("Table_S16.R")
source("Table_S17.R")
source("Table_S18.R")

# ANES data
source("Table_S19.R")
source("Table_S20.R")
source("Table_S21.R")
source("Table_S22.R")
source("Table_S23.R")
source("Table_S24.R")
source("Table_S25.R")
source("Table_S26.R")
