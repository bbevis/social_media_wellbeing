#############################################
#
#       Table S17
#       ANES age moderation: Path a
#
#############################################

source("fig_ANES_age_moderation.R")

table_s17 <- table_path_a

if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("The webshot2 package is required to save gt tables as PNG files.")
}

gtsave(
  table_s17,
  "Plots/tables_s17.png",
  vwidth = SUPPLEMENTARY_WIDE_TABLE_VWIDTH,
  vheight = 0
)
