
library(dplyr)
library(gt)
# install.packages("webshot2")
# install.packages("chromote")
library(webshot2)
library(chromote)
library(purrr)


############ Load data

weekly_df = read.csv("weekly.csv")
momentary_df = read.csv("momentary.csv")
monthly_df = read.csv("monthly.csv")

########### Define variables

weekly_mediators <- c(
  "facebook","twitter","tiktok","instagram","youtube","reddit","snapchat",
  "information","social","productivity","education","entertainment",
  "creativity","games","shopping","utilities","other","health",
  "total_activity"
)

momentary_mediators <- c("sm_minutes")    # only minutes
app_category_header <- "Digital Media App Use"
social_media_header <- "Social Media App Use"

pretty_mediator <- function(x) {
  x <- dplyr::recode(
    x,
    "total_activity"     = "Overall Screentime",
    "total_platform"     = "Overall Social Media Use",
    "social"             = "Social Media",
    "sm_minutes"         = "Social Media Time"
  )
  x <- tools::toTitleCase(gsub("_", " ", x))
  dplyr::recode(
    x,
    "Tiktok"  = "TikTok",
    "Youtube" = "YouTube"
  )
}

# Compute descriptives for a given dataset & vars

make_desc_stats <- function(df,
                            level_label,
                            vars,
                            id_col = "id", # participant ID column
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
    
    # Mediator-specific exclusions are applied to individual apps/categories.
    # Totals are not filtered here because they already sum valid mediators.
    if (exclusion_col %in% names(temp_df) &&
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
  "Creativity", "Education", "Entertainment", "Games", "Health", "Information",
  "Other", "Productivity", "Shopping", "Social Media", "Utilities",
  "Individual Social Media Apps",
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)

weekly_desc <- make_desc_stats(weekly_df, "Weekly",
                               weekly_mediators,
                               id_col = "id",
                               row_filter_col = "ema_50_exclude")

momentary_desc <- make_desc_stats(momentary_df, "Momentary",
                                  momentary_mediators,
                                  id_col = "id")

mediators_all <- bind_rows(momentary_desc, weekly_desc) %>%
  mutate(
    Level = factor(Level, levels = c("Momentary", "Weekly")),
    Variable = pretty_mediator(Variable),
    Variable = factor(Variable, levels = mediation_order)
  ) %>%
  arrange(Level, Variable) %>%
  mutate(
    Level = case_when(
      Level == "Momentary" ~ "Momentary Level",
      Level == "Weekly" ~ "Weekly Level",
      TRUE ~ as.character(Level)
    )
  )
	  

tbl_momentary <- mediators_all %>%
  filter(Level == "Momentary Level") %>%
  rename(
    'N Participants' = `N participants`,
    'N Observations' = `N observations`
  )

tbl_weekly <- mediators_all %>%
  filter(Level == "Weekly Level") %>%
  rename(
    'N Participants' = `N participants`,
    'N Observations' = `N observations`
  ) %>%
  mutate(Variable = factor(Variable, levels = mediation_order)) %>%
  arrange(Variable)

tbl_weekly_categories <- tbl_weekly %>%
  filter(as.character(Variable) %in% c(
    "Overall Screentime", "Creativity", "Education", "Entertainment", "Games",
    "Health", "Information", "Other", "Productivity", "Shopping", "Social Media", "Utilities"
  ))

tbl_weekly_platforms <- tbl_weekly %>%
  filter(as.character(Variable) %in% c(
    "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
  ))

tbl3 <- bind_rows(
  tbl_momentary,
  tibble(
    Level = "Weekly Level",
    Variable = social_media_header,
    `N Participants` = NA_real_,
    `N Observations` = NA_real_,
    Mean = NA_real_,
    SD = NA_real_,
    Min = NA_real_,
    Max = NA_real_
  ),
  tbl_weekly_platforms,
  tibble(
    Level = "Weekly Level",
    Variable = app_category_header,
    `N Participants` = NA_real_,
    `N Observations` = NA_real_,
    Mean = NA_real_,
    SD = NA_real_,
    Min = NA_real_,
    Max = NA_real_
  ),
  tbl_weekly_categories
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
	      rows = !Variable_raw %in% c(app_category_header, social_media_header)
	    )
	  ) %>%
  
  #### ----- INCREASE BODY FONT -----
  tab_style(
    style = cell_text(size = px(25)),
    locations = cells_body()
  ) %>%
    
  #### ----- LABELS -----
  cols_label(Variable = "Variable") %>%
  cols_width(
    Variable ~ px(330),
    `N Participants` ~ px(150),
    `N Observations` ~ px(150),
    Mean ~ px(100),
    SD ~ px(100),
    Min ~ px(100),
    Max ~ px(100)
  ) %>%
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
	      rows = Variable_raw %in% c(app_category_header, social_media_header),
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


############ Descriptives by political orientation

social_media_vars <- c(
  "total_platform", "facebook", "instagram", "reddit",
  "snapchat", "tiktok", "twitter", "youtube"
)

make_grouped_desc <- function(df, level_label, vars, row_filter_col = NULL) {
  df <- df %>%
    mutate(
      Political_group = factor(
        case_when(
          political_orientation %in% 1:2 ~ "Liberal",
          political_orientation %in% 3:5 ~ "Moderate",
          political_orientation %in% 6:7 ~ "Conservative",
          TRUE ~ NA_character_
        ),
        levels = c("Liberal", "Moderate", "Conservative")
      )
    )

  if (!is.null(row_filter_col) && row_filter_col %in% names(df)) {
    df <- filter(df, .data[[row_filter_col]] == 0L)
  }

  map_dfr(vars, function(v) {
    exclusion_col <- paste0(v, "_exclude")
    temp_df <- df

    if (exclusion_col %in% names(temp_df)) {
      temp_df <- filter(temp_df, .data[[exclusion_col]] == 0L)
    }

    temp_df %>%
      filter(!is.na(Political_group), !is.na(.data[[v]])) %>%
      group_by(id, Political_group) %>%
      summarise(Value = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
      group_by(Political_group) %>%
      summarise(
        `N participants` = n(),
        Mean = mean(Value),
        SD = sd(Value),
        .groups = "drop"
      ) %>%
      mutate(Level = level_label, Variable = pretty_mediator(v), .before = 1)
  })
}

grouped_desc <- bind_rows(
  make_grouped_desc(momentary_df, "Momentary", "sm_minutes"),
  make_grouped_desc(
    weekly_df, "Weekly", social_media_vars,
    row_filter_col = "ema_50_exclude"
  ),
  make_grouped_desc(monthly_df, "Monthly", social_media_vars)
) %>%
  mutate(
    Level = factor(Level, levels = c("Momentary", "Weekly", "Monthly")),
    Variable = factor(
      Variable,
      levels = pretty_mediator(c("sm_minutes", social_media_vars))
    ),
    Mean = round(Mean, 2),
    SD = round(SD, 2)
  ) %>%
  arrange(Level, Variable, Political_group) %>%
  rename(`Political group` = Political_group)

write.csv(
  grouped_desc,
  "study1_social_media_by_political_group.csv",
  row.names = FALSE
)
