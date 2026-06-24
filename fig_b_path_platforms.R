
#############################################
#
# Path b - Mediators on Wellbeing
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
# install.packages("wesanderson")
library(wesanderson)
library(stringr)

monthly_res = read.csv('monthly_results_nocontrols.csv')
weekly_res = read.csv('weekly_results_nocontrols.csv')
momentary_res = read.csv('momentary_results_nocontrols.csv')

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other", "total_activity")
minutes <- c("sm_minutes")

plot_df <- monthly_res %>%
  dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
  mutate(level = 'Monthly') %>%
  bind_rows(
    weekly_res %>%
      dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
      mutate(level = 'Weekly')) %>%
  bind_rows(
    momentary_res %>%
      dplyr::select(mediator, outcome, path_b, path_b_CI_lower, path_b_CI_upper, path_b_p) %>%
      mutate(level = 'Momentary')) %>%
  filter(mediator %in% c(platforms, minutes)) %>%
  group_by(mediator, level, outcome) %>%
  summarise(
    m   = mean(path_b),
    lwr = mean(path_b_CI_lower, na.rm = TRUE),
    upr = mean(path_b_CI_upper, na.rm = TRUE),
    p   = mean(path_b_p),
    .groups = "drop"
  ) %>%
  mutate(
    level = factor(level, levels = c("Momentary","Weekly","Monthly")),
    sig   = p <= 0.05,
    type  = case_when(
      mediator %in% platforms  ~ "Platform",
      mediator %in% activities ~ "Activity",
      TRUE ~ "Rest"
    ),
    mediator = case_when(
      mediator == "total_platform"  ~ "Social Media Screen Time (Defined)",
      mediator == "total_activity"  ~ "Total Screen Time (All Types)",
      mediator == "sm_minutes"  ~ "Social Media Screen Time (Self-Assessed)",
      TRUE ~ mediator
    ),
    outcome = dplyr::recode(
      outcome,
      "awb_momentary" = "Overall Well-being",
      "awb_t2"        = "Overall Well-being",
      "awb_week"      = "Overall Well-being",
      "swls_t2"       = "Life Satisfaction",
      "uls_t2"        = "Loneliness (r)",
      "angry_rev"     = "Angry (r)",
      "worried_rev"   = "Worried (r)",
      "happy"         = "Happy",
      "sad_rev"       = "Sad (r)",
      "stressed_rev"  = "Stressed (r)",
      "lonely_rev"    = "Lonely (r)",
      "accepted"      = "Accepted"
    ),
    mediator = tools::toTitleCase(mediator),
    color_var = ifelse(sig, "sig", "n.s.")
  )


outcome_order <- c(
  "Overall Well-being",
  "Life Satisfaction",
  "Loneliness (r)",
  "Accepted",
  "Happy",
  "Angry (r)",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

mediator_order <- c(
  "Social Media Screen Time (Defined)",
  "Social Media Screen Time (Self-Assessed)",
  "Total Screen Time (All Types)",
  "Tiktok","Twitter", "Facebook","Reddit",
  "Snapchat", "Instagram", "Youtube",
  "Social", "Creativity", "Entertainment", "Education",
  "Games", "Information", "Other", "Productivity",
  "Shopping", "Utilities"
)

outcome_order = rev(outcome_order)

# assign platform colours (brand-like example, can edit to taste)
platform_colors <- c(
  "Facebook"      = "#1877F2",
  "Twitter"       = "#00B6F1",
  "Tiktok"        = "#010101",
  "Instagram"     = "#E1306C",
  "Youtube"       = "#FF0000",
  "Reddit"        = "#FF4500",
  "Snapchat"      = "#FFFC00",
  "All Platforms" = "#6A3D9A",
  "All Minutes"   = "#33A02C"
)

# Add grey for NS to palette
color_palette <- c(platform_colors, "n.s." = "grey85")

# Use an ordered factor for y
plot_df <- plot_df %>%
  mutate(outcome = factor(outcome, levels = outcome_order),
         color_var = ifelse(sig, mediator, "n.s."))

# Positions for separators between rows
sep_y <- seq(1.5, length(mediator_order) - 0.5, by = 1)

# clean theme
theme_clean <- theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

level_shape_vals <- c(
  "Monthly"   = 16,  # filled circle
  "Weekly"    = 17,  # filled triangle
  "Momentary" = 15   # filled square
)


ggplot(
  plot_df,
  aes(x = m, y = outcome, color = color_var, shape = level,
      group = interaction(level, mediator))) +
  geom_point(
    size = 2.2,
    position = position_dodgev(height = 1)
  ) +
  geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 1,
    position = ggstance::position_dodgev(height = 1)
  ) +
  geom_vline(xintercept = 0, color = "grey75", linetype = "dashed") +
  # thin lines between every row
  geom_hline(yintercept = sep_y, linewidth = 0.3, colour = "grey75") +
  scale_y_discrete(limits = rev(mediator_order), drop = FALSE,
                   expand = expansion(mult = c(0.05, 0.05))) +   # keep the blank spacer row visible
  scale_color_manual(values = color_palette, name = "Outcomes",
                     breaks = setdiff(names(color_palette), "n.s.")) + # remove "n.s." from legend
  scale_x_continuous(limits = c(-.2, .2)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_shape_manual(values = level_shape_vals, name = "Time level") +
  labs(x = "Liberalism                                 Conservatism\n \nEffects of Political Orientation on Screen Time (H2)",
       y = "") +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    axis.text.y = element_text(lineheight = 1)
  )


ggsave("plots/mediators_platforms_wellbeing.png", width = 10, height = 8, dpi = 300)
# ggsave("plots/mediators_activities_wellbeing.png", width = 10, height = 8, dpi = 300)
