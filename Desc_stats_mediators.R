
library(tidyr)
library(dplyr)
library(ggplot2)
library(gt)
# install.packages("webshot2")
# install.packages("chromote")
library(webshot2)
library(chromote)
library(purrr)


############ Load data

appusage_monthly = read.csv("appusage_monthly.csv")
appusage_weekly = read.csv("appusage_weekly.csv")
weekly_df = read.csv("weekly.csv")
momentary_df = read.csv("momentary.csv")

########### Define variables

# BEFORE (raw totals)
monthly_mediators_before <- c(
  "facebook","twitter","tiktok","instagram","youtube","reddit","snapchat",
  "information","social","productivity","education","entertainment",
  "creativity","games","shopping","utilities","other",
  "total_platform_raw","total_activity_raw"
)
weekly_mediators_before  <- monthly_mediators_before

# AFTER (post-exclusion totals)
monthly_mediators_after <- c(
  "facebook","twitter","tiktok","instagram","youtube","reddit","snapchat",
  "information","social","productivity","education","entertainment",
  "creativity","games","shopping","utilities","other",
  "total_platform","total_activity"
)
weekly_mediators_after <- monthly_mediators_after

momentary_mediators <- c("sm_minutes")    # only minutes

# Pretty labels (map both raw and post-exclusion totals to the same label)
pretty_mediator <- function(x) {
  x <- dplyr::recode(
    x,
    "total_platform_raw" = "Social Media Apps (Included)",
    "total_activity_raw" = "Overall Screentime",
    "total_platform"     = "Social Media Apps (Included)",
    "total_activity"     = "Overall Screentime",
    "social"             = "Social Media",
    "sm_minutes"         = "Social Media Time"
  )
  tools::toTitleCase(gsub("_", " ", x))
}

# Compute descriptives for a given dataset & vars

make_desc_stats <- function(df,
                            level_label, # e.g., "Weekly", "Monthly"
                            vars,
                            id_col = "id", # participant ID column
                            apply_exclusions = FALSE, # whether to apply mediator-level exclusions
                            row_filter_col = NULL     # optional global exclusion column (e.g., ema_50_exclude)
) {
  
  # Global row filter (e.g., weekly ema_50_exclude)
  if (!is.null(row_filter_col) && row_filter_col %in% names(df)) {
    df <- dplyr::filter(df, .data[[row_filter_col]] == 0L)
  }
  
  map_dfr(vars, function(v) {
    exclusion_col <- paste0(v, "_exclude") # Construct name of possible mediator-specific exclusion column
    
    # Select relevant columns; include days_in_week if present (weekly only)
    cols_to_keep <- c(id_col, v, exclusion_col, "days_in_week")
    temp_df <- df %>% dplyr::select(any_of(cols_to_keep))
    
    # Monthly level exclusion
    # For mediators (like facebook, youtube, etc.), we remove any rows
    # where that specific variable was flagged for exclusion (e.g., usage <1
    # hr/day, >20 hr/day, or "not accurate").
    # Totals (e.g., total_platform, total_activity) are NOT filtered here,
    # because they already sum across mediators *after* exclusions are applied.
    if (apply_exclusions &&
        exclusion_col %in% names(temp_df) &&
        !(v %in% c("total_platform", "total_activity"))) {
      temp_df <- filter(temp_df, .data[[exclusion_col]] == 0L)
    }
    
    
    # Compute summary stats in the variable's native unit for each analysis level
    temp_df %>%
      dplyr::summarise(
        Variable = v,
        `N participants` = if (id_col %in% names(temp_df))
          n_distinct(.data[[id_col]][!is.na(.data[[v]])]) else NA_integer_,
        `N observations` = sum(!is.na(.data[[v]])),
        
        Mean = mean(.data[[v]], na.rm = TRUE),
        SD   = sd(.data[[v]],   na.rm = TRUE),
        Min  = suppressWarnings(min(.data[[v]], na.rm = TRUE)),
        Max  = suppressWarnings(max(.data[[v]], na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(Level = level_label) %>%
      relocate(Level, .before = 1)
  })
}


mediation_order <- c(
  "Social Media Time",
  "Overall Screentime",
  "App Categories",
  "Creativity", "Education", "Entertainment", "Games", "Information",
  "Other", "Productivity", "Shopping", "Social Media", "Utilities",
  "Individual Social Media Apps",
  "Facebook", "Instagram", "Reddit", "Snapchat", "Tiktok", "Twitter", "Youtube"
)

# Before exclusions
monthly_desc_before   <- make_desc_stats(appusage_monthly, "Monthly",
                                         monthly_mediators_before,
                                         id_col = "id", apply_exclusions = FALSE)

weekly_desc_before    <- make_desc_stats(appusage_weekly, "Weekly",
                                         weekly_mediators_before,
                                         id_col = "id", apply_exclusions = FALSE)

momentary_desc_before <- make_desc_stats(momentary_df, "Momentary",
                                         momentary_mediators,
                                         id_col = "id", apply_exclusions = FALSE)

# After exclusions
monthly_desc_after   <- make_desc_stats(appusage_monthly, "Monthly",
                                        monthly_mediators_after,
                                        id_col = "id", apply_exclusions = TRUE)

weekly_desc_after    <- make_desc_stats(weekly_df, "Weekly",
                                        weekly_mediators_after,
                                        id_col = "id", apply_exclusions = TRUE,
                                        row_filter_col = "ema_50_exclude")

momentary_desc_after <- make_desc_stats(momentary_df, "Momentary",
                                        momentary_mediators,
                                        id_col = "id", apply_exclusions = TRUE)



# BEFORE
before_all <- bind_rows(momentary_desc_before, weekly_desc_before, monthly_desc_before) %>%
  mutate(Level = factor(Level, levels = c("Momentary","Weekly","Monthly")),
         Variable = pretty_mediator(Variable),
         Variable = factor(Variable, levels = mediation_order)) %>%
  arrange(Level, Variable)

# AFTER
after_all <- bind_rows(momentary_desc_after, weekly_desc_after, monthly_desc_after) %>%
  mutate(Level = factor(Level, levels = c("Momentary","Weekly","Monthly")),
         Variable = pretty_mediator(Variable),
         Variable = factor(Variable, levels = mediation_order)) %>%
  arrange(Level, Variable)

# Final combined table (aligned by keys)
mediators_all <- bind_rows(
  before_all %>% mutate(Period = "Before exclusions"),
  after_all  %>% mutate(Period = "After exclusions")) %>%
  mutate(
    Level = case_when(
      Level == "Momentary" ~ "Momentary Level",
      Level %in% c("Weekly","Monthly") ~ paste0(Level, " Level"),
      TRUE ~ as.character(Level)
    )
  )
      

tbl_momentary <- mediators_all %>%
  filter(Period == "After exclusions", Level == "Momentary Level") %>%
  dplyr::select(-Period) %>%
  rename(
    'N Participants' = `N participants`,
    'N Observations' = `N observations`
  )

tbl_weekly <- mediators_all %>%
  filter(Period == "After exclusions", Level == "Weekly Level") %>%
  filter(Variable != "Social Media Apps (Included)") %>%
  dplyr::select(-Period) %>%
  rename(
    'N Participants' = `N participants`,
    'N Observations' = `N observations`
  ) %>%
  mutate(Variable = factor(Variable, levels = mediation_order)) %>%
  arrange(Variable)

tbl_weekly_categories <- tbl_weekly %>%
  filter(as.character(Variable) %in% c(
    "Overall Screentime", "Creativity", "Education", "Entertainment", "Games",
    "Information", "Other", "Productivity", "Shopping", "Social Media", "Utilities"
  ))

tbl_weekly_platforms <- tbl_weekly %>%
  filter(as.character(Variable) %in% c(
    "Facebook", "Instagram", "Reddit", "Snapchat", "Tiktok", "Twitter", "Youtube"
  ))

tbl3 <- bind_rows(
  tbl_momentary,
  tibble(
    Level = "Weekly Level",
    Variable = "App Categories",
    `N Participants` = NA_real_,
    `N Observations` = NA_real_,
    Mean = NA_real_,
    SD = NA_real_,
    Min = NA_real_,
    Max = NA_real_
  ),
  tbl_weekly_categories,
  tibble(
    Level = "Weekly Level",
    Variable = "Individual Social Media Apps",
    `N Participants` = NA_real_,
    `N Observations` = NA_real_,
    Mean = NA_real_,
    SD = NA_real_,
    Min = NA_real_,
    Max = NA_real_
  ),
  tbl_weekly_platforms
) %>%
  mutate(
    Variable_raw = as.character(Variable),
    Variable = Variable_raw
  )



tbl_final <- tbl3 %>%
  gt(groupname_col = "Level") %>%
  tab_header(title = NULL, subtitle = NULL) %>%
  row_group_order(c("Momentary Level", "Weekly Level")) %>%
  cols_hide("Level") %>%
  
  #### ----- STYLE GROUP LABELS -----
  tab_style(
    style = cell_text(size = px(25), weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  
  #### ----- INDENT VARIABLE COLUMN -----
  tab_style(
    style = cell_text(indent = px(25)),
    locations = cells_body(
      columns = "Variable",
      rows = !Variable_raw %in% c("App Categories", "Individual Social Media Apps")
    )
  ) %>%
  
  #### ----- INCREASE BODY FONT -----
  tab_style(
    style = cell_text(size = px(25)),
    locations = cells_body()
  ) %>%
    
  #### ----- LABELS -----
  cols_label(Variable = "Variable") %>%
  cols_hide("Variable_raw") %>%
  sub_missing(columns = c("N Participants", "N Observations", "Mean", "SD", "Min", "Max"), missing_text = "") %>%
  
  #### ----- BOLD COLUMN LABELS -----
  tab_style(
    style = cell_text(size = px(25), weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
    
  #### Bold selected Variable rows
  # tab_style(
  #   style = cell_text(size = px(25), weight = "bold"),
  #   locations = cells_body(
  #     rows = Variable_raw %in% c(
  #       "Overall Screentime"
  #     ),
  #     columns = "Variable"
  #   )
  # ) %>%
  
  #### Weekly section label rows
  tab_style(
    style = cell_text(size = px(25), style = "italic"),
    locations = cells_body(
      rows = Variable_raw %in% c("App Categories", "Individual Social Media Apps"),
      columns = "Variable"
    )
  ) %>%
  
  #### ----- NUMBER FORMATTING -----
  fmt_number(columns = c("Mean", "SD", "Min", "Max"), decimals = 2) %>%
    fmt_number(columns = c("N Participants", "N Observations"), decimals = 0, use_seps = TRUE) %>%
  
  #### ----- ALIGNMENT -----
  cols_align("center", columns = everything()) %>%
    cols_align("left", columns = "Variable") %>%
  
  
  #### ----- BORDERS: KEEP ONLY TOP, BOTTOM & HEADER LINE -----
  tab_options(
    table.font.names = "Helvetica, Arial, sans-serif",
    heading.align = "left",
    data_row.padding = px(4),
    table.width = pct(70),
    
    # keep outer borders
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    
    # remove all internal borders
    table.border.left.style   = "none",
    table.border.right.style  = "none",
    row_group.border.top.style = "none",
    row_group.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.hlines.color = "transparent",
    
    
    # header underline only
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "solid"
  )


# Save
gtsave(tbl_final, "Plots/tables_desc_mediators_combined.png", vwidth = 2000, vheight = 0)
