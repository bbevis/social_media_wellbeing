#############################################
#
# Path b - Mediators on Wellbeing (Combined)
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
library(ggh4x)
library(stringr)
library(wesanderson)

monthly_res <- read.csv("monthly_results_nocontrols.csv")
weekly_res <- read.csv("weekly_results_nocontrols.csv")
momentary_res <- read.csv("momentary_results_nocontrols.csv")

platforms <- c(
  "facebook", "twitter", "tiktok", "instagram",
  "youtube", "reddit", "snapchat", "total_platform"
)

activities <- c(
  "information", "social", "productivity", "education", "entertainment",
  "creativity", "games", "shopping", "utilities", "other", "total_activity"
)

minutes <- c("sm_minutes")

outcome_order <- c(
  "Overall Wellbeing",
  "Accepted",
  "Angry (r)",
  "Happy",
  "Life Satisfaction",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

platform_order <- c(
  "Facebook",
  "Instagram",
  "Reddit",
  "Snapchat",
  "Tiktok",
  "Twitter",
  "Youtube"
)

activity_order <- c(
  "Overall Screentime",
  "Creativity",
  "Education",
  "Entertainment",
  "Games",
  "Information",
  "Other",
  "Productivity",
  "Social Media"
)

platform_labels <- c(
  "Facebook",
  "Twitter",
  "Tiktok",
  "Instagram",
  "Youtube",
  "Reddit",
  "Snapchat"
)

activity_labels <- c(
  "Information",
  "Social",
  "Productivity",
  "Education",
  "Entertainment",
  "Creativity",
  "Games",
  "Other",
  "Overall Screentime"
)

build_base_df <- function() {
  bind_rows(
    monthly_res %>%
      dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
      mutate(level = "Monthly"),
    weekly_res %>%
      dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
      mutate(level = "Weekly"),
    momentary_res %>%
      dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
      mutate(level = "Momentary")
  ) %>%
    group_by(mediator, level, outcome) %>%
    summarise(
      m = mean(path_b),
      lwr = mean(path_b_CI_lower, na.rm = TRUE),
      upr = mean(path_b_CI_upper, na.rm = TRUE),
      p = mean(path_b_p),
      .groups = "drop"
    ) %>%
    mutate(
      level = factor(level, levels = c("Monthly", "Weekly", "Momentary")),
      sig = p <= 0.05,
      mediator = case_when(
        mediator == "total_platform" ~ "Social Media",
        mediator == "total_activity" ~ "Overall Screentime",
        mediator == "social" ~ "Social Media",
        mediator == "sm_minutes" ~ "Social Media",
        TRUE ~ mediator
      ),
      outcome = dplyr::recode(
        outcome,
        "awb_momentary" = "Overall Wellbeing",
        "awb_t2" = "Overall Wellbeing",
        "awb_week" = "Overall Wellbeing",
        "swls_t2" = "Life Satisfaction",
        "uls_t2" = "Lonely (r)",
        "angry_rev" = "Angry (r)",
        "worried_rev" = "Worried (r)",
        "happy" = "Happy",
        "sad_rev" = "Sad (r)",
        "stressed_rev" = "Stressed (r)",
        "lonely_rev" = "Lonely (r)",
        "accepted" = "Accepted"
      ),
      mediator = tools::toTitleCase(mediator)
    )
}

base_df <- build_base_df()

platform_df <- base_df %>%
  filter(mediator %in% platform_labels) %>%
  mutate(panel = "Platforms")

activity_df <- base_df %>%
  filter(mediator %in% c(activity_labels, "Social Media")) %>%
  mutate(
    panel = "Activities",
    mediator_row = mediator
  )

plot_df <- bind_rows(platform_df, activity_df) %>%
  mutate(
    panel = factor(panel, levels = c("Activities", "Platforms")),
    mediator_row = case_when(
      panel == "Platforms" ~ as.character(mediator),
      panel == "Activities" ~ mediator_row
    ),
    mediator = case_when(
      panel == "Platforms" ~ factor(mediator_row, levels = rev(platform_order)),
      panel == "Activities" ~ factor(mediator_row, levels = rev(activity_order))
    ),
    color_var = ifelse(sig, outcome, "n.s.")
  )

platform_x_limit <- plot_df %>%
  filter(panel == "Platforms") %>%
  summarise(limit = max(abs(c(lwr, upr)), na.rm = TRUE)) %>%
  pull(limit)
platform_x_limit <- max(0.2, ceiling(platform_x_limit * 10) / 10)

activity_x_limit <- plot_df %>%
  filter(panel == "Activities") %>%
  summarise(limit = max(abs(c(lwr, upr)), na.rm = TRUE)) %>%
  pull(limit)
activity_x_limit <- max(0.2, ceiling(activity_x_limit * 10) / 10)

axis_breaks <- function(limit) {
  pretty(c(-limit, limit), n = 5)
}

n_outcomes <- length(outcome_order)
wes_cols <- wesanderson::wes_palette("FantasticFox1", n_outcomes, type = "continuous")
color_palette <- c(
  "Overall Wellbeing" = wes_cols[5],
  "Accepted" = "#2E8B57",
  "Angry (r)" = "#D7301F",
  "Happy" = wes_cols[4],
  "Life Satisfaction" = "#66A61E",
  "Lonely (r)" = "#A50026",
  "Sad (r)" = "#E34A33",
  "Stressed (r)" = "#FDAE61",
  "Worried (r)" = "#F46D43",
  "n.s." = "grey85"
)

theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

level_shape_vals <- c(
  "Monthly" = 16,
  "Weekly" = 17,
  "Momentary" = 15
)

y_limits_platform <- seq(1.5, length(platform_order) - 0.5, by = 1)
y_limits_activity <- seq(1.5, length(activity_order) - 0.5, by = 1)

ggplot(
  plot_df,
  aes(
    x = m,
    y = mediator,
    color = color_var,
    shape = level,
    group = interaction(level, outcome, mediator, panel)
  )
) +
  geom_point(
    size = 2.8,
    position = ggstance::position_dodgev(height = 0.9)
  ) +
  ggstance::geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 0.9,
    position = ggstance::position_dodgev(height = 0.9)
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_hline(
    data = data.frame(
      panel = factor("Platforms", levels = c("Activities", "Platforms")),
      yint = y_limits_platform
    ),
    aes(yintercept = yint),
    inherit.aes = FALSE,
    linewidth = 0.3,
    colour = "grey75"
  ) +
  geom_hline(
    data = data.frame(
      panel = factor("Activities", levels = c("Activities", "Platforms")),
      yint = y_limits_activity
    ),
    aes(yintercept = yint),
    inherit.aes = FALSE,
    linewidth = 0.3,
    colour = "grey75"
  ) +
  facet_wrap(~panel, nrow = 1, scales = "free") +
  ggh4x::facetted_pos_scales(
    x = list(
      scale_x_continuous(
        limits = c(-activity_x_limit, activity_x_limit),
        breaks = axis_breaks(activity_x_limit)
      ),
      scale_x_continuous(
        limits = c(-platform_x_limit, platform_x_limit),
        breaks = axis_breaks(platform_x_limit)
      )
    )
  ) +
  scale_color_manual(
    values = color_palette,
    name = "Wellbeing",
    breaks = setdiff(names(color_palette), "n.s."),
    guide = guide_legend(order = 2)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_shape_manual(
    values = level_shape_vals,
    name = "Time level",
    breaks = c("Momentary", "Weekly", "Monthly"),
    guide = guide_legend(order = 1)
  ) +
  guides(
    shape = guide_legend(override.aes = list(size = 3), order = 1)
  ) +
  labs(
    x = "Associations Between Smartphone App/Social Media Use and Wellbeing (H3)",
    y = ""
  ) +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    axis.text.y = element_text(lineheight = 1),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

ggsave("plots/mediators_wellbeing_combined.png", width = 16, height = 10, dpi = 300)
