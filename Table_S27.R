#############################################
#
#       Table S27
#       ANES age moderation: Path b
#
#############################################

source("fig_ANES_age_moderation.R")

table_s27 <- table_path_b

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s27,
  "Plots/tables_s27.png",
  vwidth = 2800
)
