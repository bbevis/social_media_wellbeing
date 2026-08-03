#############################################
#
#       ANES age-moderation simple slopes
#       Paths a and b
#
#############################################

library(dplyr)
library(ggplot2)

#############################################
# Load data

path_a_predictions <- read.csv(
  "anes_path_a_age_moderation_plot_results.csv"
)

path_b_predictions <- read.csv(
  "anes_age_moderation_plot_results.csv"
) %>%
  filter(
    path == "b",
    outcome %in% c("loneliness", "anhedonia", "depression")
  )

#############################################
# Labels and display settings

mediator_labels <- c(
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "twitter_frequency" = "Twitter",
  "youtube_frequency" = "YouTube"
)

mediator_order <- unname(mediator_labels)

outcome_labels <- c(
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)"
)

outcome_order <- unname(outcome_labels)
age_order <- c("-1 SD", "Mean age", "+1 SD")

age_palette <- c(
  "-1 SD" = "#276CF5",
  "Mean age" = "#555555",
  "+1 SD" = "#E26D2D"
)

theme_slopes <- theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#############################################
# Path a: political orientation -> social media use

path_a_plot_data <- path_a_predictions %>%
  mutate(
    mediator = dplyr::recode(mediator, !!!mediator_labels),
    mediator = factor(mediator, levels = mediator_order),
    age_level = factor(age_level, levels = age_order)
  )

stopifnot(
  nrow(path_a_plot_data) == 7 * 3 * 100,
  !anyDuplicated(
    path_a_plot_data[c("mediator", "age_level", "predictor_value")]
  )
)

path_a_plot <- ggplot(
  path_a_plot_data,
  aes(
    x = predictor_value,
    y = estimate,
    color = age_level,
    fill = age_level,
    group = age_level
  )
) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.08,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(vars(mediator), ncol = 4) +
  scale_x_continuous(breaks = 1:7) +
  scale_color_manual(values = age_palette, drop = FALSE) +
  scale_fill_manual(values = age_palette, guide = "none", drop = FALSE) +
  labs(
    x = "Political Orientation (Liberalism to Conservatism)",
    y = "Predicted Social Media Use (z)",
    title = "Age Moderation of Path a"
  ) +
  theme_slopes

ggsave(
  "Plots/ANES_age_moderation_path_a.png",
  plot = path_a_plot,
  width = 16,
  height = 10,
  dpi = 300
)

#############################################
# Path b: social media use -> wellbeing

path_b_plot_data <- path_b_predictions %>%
  mutate(
    mediator = dplyr::recode(mediator, !!!mediator_labels),
    mediator = factor(mediator, levels = mediator_order),
    outcome = dplyr::recode(outcome, !!!outcome_labels),
    outcome = factor(outcome, levels = outcome_order),
    age_level = factor(age_level, levels = age_order)
  )

stopifnot(
  nrow(path_b_plot_data) == 7 * 3 * 3 * 100,
  !anyDuplicated(
    path_b_plot_data[
      c("mediator", "outcome", "age_level", "predictor_value")
    ]
  )
)

path_b_plot <- ggplot(
  path_b_plot_data,
  aes(
    x = predictor_value,
    y = estimate,
    color = age_level,
    fill = age_level,
    group = age_level
  )
) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.06,
    color = NA
  ) +
  geom_line(linewidth = 0.65) +
  facet_grid(
    rows = vars(mediator),
    cols = vars(outcome),
    drop = FALSE
  ) +
  scale_x_continuous(breaks = -2:2) +
  scale_color_manual(values = age_palette, drop = FALSE) +
  scale_fill_manual(values = age_palette, guide = "none", drop = FALSE) +
  labs(
    x = "Social Media Use (z)",
    y = "Predicted Wellbeing (z)",
    title = "Age Moderation of Path b"
  ) +
  theme_slopes +
  theme(
    axis.text = element_text(size = 8),
    strip.text.x = element_text(size = 9),
    strip.text.y = element_text(size = 10),
    panel.spacing = unit(0.8, "lines")
  )

ggsave(
  "Plots/ANES_age_moderation_path_b.png",
  plot = path_b_plot,
  width = 15,
  height = 20,
  dpi = 300,
  limitsize = FALSE
)
