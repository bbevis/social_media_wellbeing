#############################################
# Shared styling for supplementary tables

SUPPLEMENTARY_TABLE_VWIDTH <- 2000
SUPPLEMENTARY_WIDE_TABLE_VWIDTH <- 2400

apply_supplementary_table_style <- function(
    table,
    table_width = gt::pct(70)
) {
  table <- gt::tab_options(
    table,
    table.font.size = gt::px(23),
    table.font.names = "Times",
    heading.align = "left",
    data_row.padding = gt::px(4),
    row_group.padding = gt::px(4),
    column_labels.padding = gt::px(4),
    table.width = table_width,
    heading.border.bottom.style = "solid",
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    table.border.left.style = "none",
    table.border.right.style = "none",
    row_group.border.top.style = "none",
    row_group.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.hlines.color = "transparent",
    table_body.vlines.style = "none",
    column_labels.vlines.style = "none",
    stub.border.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "solid",
    row.striping.include_table_body = FALSE,
    row.striping.include_stub = FALSE
  )

  gt::tab_style(
    table,
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels(gt::everything())
  )
}
