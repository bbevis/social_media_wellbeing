#############################################
#
#       Table S26
#       ANES age moderation: Path a
#
#############################################

source("fig_ANES_age_moderation.R")

table_s26 <- table_path_a

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s26,
  "Plots/tables_s26.png",
  vwidth = 2800
)
