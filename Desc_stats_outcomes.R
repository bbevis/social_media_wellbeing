
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(gt)
# install.packages("webshot2")
# install.packages("chromote")
library(webshot2)
library(chromote)


############ Load data

monthly_df = read.csv("monthly.csv")
momentary_df = read.csv("momentary.csv")

########### Define variables
monthly_outcomes   <- c("swls_t2","uls_t2","awb_t2")
momentary_outcomes <- c("awb_momentary","angry_rev","worried_rev","happy","sad_rev","stressed_rev","lonely_rev","accepted")


pretty_outcome <- function(x) {
  dplyr::recode(
    x,
    "awb_momentary" = "Affective Wellbeing",
    "awb_t2"        = "Affective Wellbeing",
    "swls_t2"       = "Life Satisfaction",
    "uls_t2"        = "Lonely (r)",
    "angry_rev"     = "Angry (r)",
    "worried_rev"   = "Worried (r)",
    "happy"         = "Happy",
    "sad_rev"       = "Sad (r)",
    "stressed_rev"  = "Stressed (r)",
    "lonely_rev"    = "Lonely (r)",
    "accepted"      = "Accepted"
  )
}


outcome_order <- c(
  "Affective Wellbeing",
  "Life Satisfaction",
  "Accepted",
  "Angry (r)",
  "Happy",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

make_outcome_desc <- function(df, level_label, vars, id_col = "id") {
  map_dfr(vars, function(v) {
    tmp <- dplyr::select(df, dplyr::any_of(c(id_col, v)))
    dplyr::summarise(
      tmp,
      Variable = v,
      `N participants` = if (id_col %in% names(tmp))
        dplyr::n_distinct(.data[[id_col]][!is.na(.data[[v]])]) else NA_integer_,
      `N observations` = sum(!is.na(.data[[v]])),
      Mean = mean(.data[[v]], na.rm = TRUE),
      SD   = sd(.data[[v]],   na.rm = TRUE),
      Min  = suppressWarnings(min(.data[[v]], na.rm = TRUE)),
      Max  = suppressWarnings(max(.data[[v]], na.rm = TRUE)),
      .groups = "drop"
    ) |>
      mutate(Level = level_label) |>
      relocate(Level, .before = 1)
  })
}

monthly_desc   <- make_outcome_desc(monthly_df,   "Monthly Level",   monthly_outcomes)
momentary_desc <- make_outcome_desc(momentary_df, "Momentary Level", momentary_outcomes)

outcomes_all <- bind_rows(momentary_desc, monthly_desc) %>%
  mutate(
    Level = factor(Level, levels = c("Momentary Level","Monthly Level")),
    Variable = pretty_outcome(Variable),
    Variable = factor(Variable, levels = outcome_order)
  ) %>%
  arrange(Level, Variable)

tbl_outcomes <- gt::gt(outcomes_all, groupname_col = "Level") %>%
  gt::cols_hide("Level") %>%
  gt::tab_style(gt::cell_text(weight = "bold"),
                locations = gt::cells_row_groups()) %>%
  gt::tab_style(gt::cell_text(size = gt::px(22), weight = "bold"),
                locations = gt::cells_title("title")) %>%
	  gt::cols_label(
	    Variable = "Variable",
	    `N participants` = gt::md("**N Participants**"),
	    `N observations` = gt::md("**N Observations**"),
	    Mean = gt::md("**Mean**"),
	    SD   = gt::md("**SD**"),
	    Min  = gt::md("**Min**"),
	    Max  = gt::md("**Max**")
	  ) %>%
	  gt::cols_width(
	    Variable ~ gt::px(330),
	    `N participants` ~ gt::px(150),
	    `N observations` ~ gt::px(150),
	    Mean ~ gt::px(100),
	    SD ~ gt::px(100),
	    Min ~ gt::px(100),
	    Max ~ gt::px(100)
	  ) %>%
	  gt::fmt_number(columns = tidyselect::all_of(c("Mean","SD","Min","Max")), decimals = 2) %>%
  gt::fmt_number(columns = tidyselect::all_of(c("N participants","N observations")),
                 decimals = 0, use_seps = TRUE) %>%
  
  #### ---- INDENT VARIABLES ----
  gt::tab_style(
    style = gt::cell_text(indent = gt::px(25)),
    locations = gt::cells_body(columns = "Variable")
  ) %>%
  
  #### ---- INCREASE BODY FONT SIZE ----
  gt::tab_style(
    style = gt::cell_text(size = gt::px(18)),
    locations = gt::cells_body()
  ) %>%
  
  #### ---- ALIGNMENT ----
  gt::cols_align(align = "center", columns = gt::everything()) %>%
    gt::cols_align(align = "left", columns = "Variable") %>%
    gt::tab_style(gt::cell_text(weight = "bold"),
                  locations = gt::cells_column_labels(gt::everything())) %>%
  
  #### ---- BORDER REMOVAL + KEEP ONLY TOP/BOTTOM + HEADER LINE ----
  gt::tab_options(
    table.font.names = "Helvetica, Arial, sans-serif",
    heading.align = "left",
    data_row.padding = gt::px(4),
    table.width = gt::pct(50),
    
    # Keep outer borders
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    
    # Remove everything else
    table.border.left.style   = "none",
    table.border.right.style  = "none",
    row_group.border.top.style = "none",
    row_group.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.hlines.color = "transparent",
    
    # Header bottom border only
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "solid"
  )



gtsave(tbl_outcomes, "Plots/tables_desc_outcomes.png", vwidth = 1600, vheight = 0)
