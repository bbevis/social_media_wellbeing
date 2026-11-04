#############################################
#
# Figure 5: Social media use and wellbeing (H3)
#
#############################################

# Load packages and analysis results

library(dplyr)
library(ggplot2)
library(ggstance)
library(stringr)

monthly_res <- read.csv("monthly_results_withcontrols.csv")
weekly_res <- read.csv("weekly_results_withcontrols.csv")
momentary_res <- read.csv("momentary_results_withcontrols.csv")

# Select social media measures
# Include overall social media use and seven platforms; exclude overall
# screentime and other app categories.

platforms <- c(
  "facebook", "twitter", "tiktok", "instagram",
  "youtube", "reddit", "snapchat"
)

mediators_to_plot <- c(
  "total_platform", platforms,"sm_minutes"
)

# Set outcome and measure order, and temporal-level shapes

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
  "TikTok",
  "Twitter",
  "YouTube"
)

level_shape_vals <- c(
  "Monthly" = 16,
  "Weekly" = 17,
  "Momentary" = 15
)

# Prepare path b estimates and confidence intervals
# Average estimates and confidence limits within each measure/outcome/level.
# A group is significant if any of its path b p-values is <= .05.

base_df <- bind_rows(
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
  filter(mediator %in% mediators_to_plot) %>%
  mutate(
    mediator = dplyr::recode(
      mediator,
      "total_platform" = "Social Media",
      "sm_minutes" = "Social Media",
      "total_activity" = "Overall Screentime",
      "social" = "Social"
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(mediator, "Tiktok" = "TikTok", "Youtube" = "YouTube"),
    outcome = dplyr::recode(
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
    outcome = factor(outcome, levels = rev(outcome_order)),
    mediator = factor(mediator, levels = mediator_order),
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

# Set axis range, labels and plot theme
# Adjust the horizontal range to include the confidence limits, with a
# minimum extent of 0.2 on either side of zero.

x_limit <- base_df %>%
  summarise(limit = max(abs(c(lwr, upr)), na.rm = TRUE)) %>%
  pull(limit)
x_limit <- max(0.2, ceiling(x_limit * 10) / 10)

y_limits <- seq(1.5, length(outcome_order) - 0.5, by = 1)

y_axis_labels <- function(x) {
  labels <- ifelse(x == "Affective Wellbeing",
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

# Create and save Figure 5
# Shapes distinguish temporal levels; pink highlights significant estimates
# and grey shows non-significant estimates.

ggplot(
  base_df,
  aes(
    x = m,
    y = outcome,
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
  facet_wrap(~mediator, ncol = 3) +
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
    x = "Associations between Social Media Use and Wellbeing (H3)",
    y = ""
  ) +
  theme_clean

ggsave("plots/Study1_Figure5_social_media_wellbeing.png", width = 18, height = 16, dpi = 300)
