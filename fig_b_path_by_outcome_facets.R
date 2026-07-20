#############################################
#
# Path b - Mediators on Wellbeing by Outcome
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
library(stringr)

monthly_res <- read.csv("monthly_results_withcontrols.csv")
weekly_res <- read.csv("weekly_results_withcontrols.csv")
momentary_res <- read.csv("momentary_results_withcontrols.csv")

platforms <- c(
  "facebook", "twitter", "tiktok", "instagram",
  "youtube", "reddit", "snapchat"
)

activities <- c(
  "information", "social", "productivity", "education", "entertainment",
  "creativity", "games", "shopping", "utilities", "other", "health"
)

mediators_to_plot <- c(
  "total_platform", platforms,"sm_minutes"
) # "total_activity" and activities have been excluded

outcome_order <- c(
  "Affective Wellbeing",
  "Accepted",
  "Angry (r)",
  "Happy",
  "Life Satisfaction",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

mediator_order <- c(
  "Social Media",
  "Facebook",
  "Instagram",
  "Reddit",
  "Snapchat",
  "Tiktok",
  "Twitter",
  "Youtube"
  # " ",
  # "Overall Screentime",
  # "Creativity",
  # "Education",
  # "Entertainment",
  # "Games",
  # "Health",
  # "Information",
  # "Other",
  # "Productivity",
  # "Shopping",
  # "Social",
  # "Utilities"
)

level_shape_vals <- c(
  "Monthly" = 16,
  "Weekly" = 17,
  "Momentary" = 15
)

base_df <- bind_rows(
  monthly_res %>%
    select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
    mutate(level = "Monthly"),
  weekly_res %>%
    select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
    mutate(level = "Weekly"),
  momentary_res %>%
    select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
    mutate(level = "Momentary")
) %>%
  filter(mediator %in% mediators_to_plot) %>%
  mutate(
    mediator = recode(
      mediator,
      "total_platform" = "Social Media",
      "sm_minutes" = "Social Media",
      "total_activity" = "Overall Screentime",
      "social" = "Social"
    ),
    mediator = tools::toTitleCase(mediator),
    outcome = recode(
      outcome,
      "awb_momentary" = "Affective Wellbeing",
      "awb_t2" = "Affective Wellbeing",
      "awb_week" = "Affective Wellbeing",
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
    level = factor(level, levels = c("Monthly", "Weekly", "Momentary")),
    outcome = factor(outcome, levels = outcome_order),
    mediator = factor(mediator, levels = rev(mediator_order)),
    sig = path_b_p <= 0.05,
    sig_label = ifelse(sig, "p <= .05", "n.s.")
  ) %>%
  group_by(mediator, outcome, level) %>%
  summarise(
    m = mean(path_b, na.rm = TRUE),
    lwr = mean(path_b_CI_lower, na.rm = TRUE),
    upr = mean(path_b_CI_upper, na.rm = TRUE),
    sig = any(sig, na.rm = TRUE),
    sig_label = ifelse(sig, "p <= .05", "n.s."),
    .groups = "drop"
  )

x_limit <- base_df %>%
  summarise(limit = max(abs(c(lwr, upr)), na.rm = TRUE)) %>%
  pull(limit)
x_limit <- max(0.2, ceiling(x_limit * 10) / 10)

y_limits <- seq(1.5, length(mediator_order) - 0.5, by = 1)

y_axis_labels <- function(x) {
  labels <- ifelse(x %in% c("Social Media", "Overall Screentime"),
                   paste0("bold('", x, "')"),
                   paste0("'", str_wrap(x, width = 22), "'"))
  labels[x == " "] <- "''"
  parse(text = labels)
}

theme_clean <- theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(face = "bold", size = 20, colour = "black"),
    legend.position = "top",
    legend.box = "horizontal",
    legend.text = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 15, lineheight = 0.95, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.title.x = element_text(size = 24, colour = "black", margin = margin(t = 10))
  )

ggplot(
  base_df,
  aes(
    x = m,
    y = mediator,
    shape = level,
    colour = sig_label,
    group = interaction(level, mediator)
  )
) +
  geom_hline(
    yintercept = y_limits,
    linewidth = 0.3,
    colour = "grey75"
  ) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  ggstance::geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 0.25,
    position = ggstance::position_dodgev(height = 0.6),
    size = 0.45
  ) +
  geom_point(
    size = 2.5,
    position = ggstance::position_dodgev(height = 0.6)
  ) +
  facet_wrap(~outcome, ncol = 3) +
  coord_cartesian(xlim = c(-x_limit, x_limit)) +
  scale_y_discrete(labels = y_axis_labels, drop = FALSE) +
  scale_shape_manual(
    values = level_shape_vals,
    breaks = c("Momentary", "Weekly", "Monthly"),
    name = "Time level"
  ) +
  scale_colour_manual(
    values = c("p <= .05" = "#FF1493", "n.s." = "grey75"),
    breaks = c("p <= .05", "n.s."),
    name = NULL
  ) +
  guides(
    shape = guide_legend(title = NULL, override.aes = list(size = 3.8)),
    colour = "none"
  ) +
  labs(
    x = "Associations between Social and Digital Media Use and Wellbeing (H3)",
    y = ""
  ) +
  theme_clean

ggsave("plots/mediators_wellbeing_by_outcome_facets.png", width = 18, height = 16, dpi = 300)
