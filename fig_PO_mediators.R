#############################################
#
# Figure 2 - Political orientation on social media app usage
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
library(stringr)

monthly_res = read.csv('monthly_results_withcontrols.csv')
weekly_res = read.csv('weekly_results_withcontrols.csv')
momentary_res = read.csv('momentary_results_withcontrols.csv')

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other","health", "total_activity")
minutes <- c("sm_minutes")



level_shape_vals <- c(
  "Monthly"   = 16,  # filled circle
  "Weekly"    = 17,  # filled triangle
  "Momentary" = 15   # filled square
)


plot_df <- monthly_res %>%
  filter(outcome == 'awb_t2') %>%
  dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
  mutate(level = 'Monthly') %>%
  bind_rows(
    weekly_res %>%
      filter(outcome == 'awb_week') %>%
      dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
      mutate(level = 'Weekly')) %>%
    bind_rows(
      momentary_res %>%
      filter(outcome == 'awb_momentary') %>%
      dplyr::select(mediator, path_a, path_a_CI_lower, path_a_CI_upper, path_a_p) %>%
      mutate(level = 'Momentary')) %>%
  filter(mediator %in% c(platforms, minutes, "social")) %>%
  group_by(mediator, level) %>%
  summarise(
    m   = mean(path_a),
    lwr = mean(path_a_CI_lower, na.rm = TRUE),
    upr = mean(path_a_CI_upper, na.rm = TRUE),
    p   = mean(path_a_p),
    .groups = "drop"
  ) %>%
  mutate(
    level = factor(level, levels = c("Monthly","Weekly","Momentary")),
    sig   = p <= 0.05,
    type  = case_when(
      mediator %in% platforms  ~ "Platform",
      mediator %in% activities ~ "Activity",
      TRUE ~ "Rest"
    ),
    mediator = case_when(
      mediator == "total_platform"  ~ NA_character_,
      mediator == "total_activity"  ~ "All Apps",
      mediator == "sm_minutes"  ~ "Social Media",
      mediator == "social" ~ "Social Media",
      TRUE ~ mediator
    ),
    mediator = tools::toTitleCase(mediator),
    color_var = ifelse(sig, "sig", "n.s.")
  )

mediator_order <- c(
  "Social Media",
  "Facebook", "Instagram", "Reddit", "Snapchat", "Tiktok", "Twitter", "Youtube"
)

mediator_order = rev(mediator_order)


plot_df <- plot_df %>%
  filter(!is.na(mediator)) %>%
  mutate(mediator = factor(mediator, levels = mediator_order))

# Exclude shopping and utilities due to small size
plot_df <- plot_df %>%
  filter(!mediator%in%c("All Apps",
                        "Creativity", "Education", "Entertainment", "Games", "Health", "Information",
                        "Other", "Productivity", "Shopping", "Utilities"))

y_limits <- seq(1.5, length(unique(plot_df$mediator)) - 0.5, by = 1)


# clean theme
theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggplot(
  plot_df,
  aes(x = m, y = mediator,
      shape = level,
      color = color_var,
      group = interaction(level, mediator))
) +
  geom_point(
    position = position_dodge(width = .8),
    size = 3
  ) +
  geom_errorbar(
    aes(xmin = lwr, xmax = upr),
    width = 0.6,
    position = position_dodge(width = .8)
  ) +
  # thin lines between every row
  geom_hline(
    yintercept = seq(0.5, length(y_limits) + 0.5, by = 1),
    color = "grey75", linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  scale_x_continuous(limits = c(-.5, .5),
                     breaks = seq(-.5, .5, by = .2)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  # scale_y_discrete(limits = y_limits, drop = FALSE) +  # keep the blank rows
  scale_color_manual(
    values = c("sig" = "#276CF5", "n.s." = "grey85"),
    name   = "Significance",
    labels = c("sig" = "p ≤ .05", "n.s." = "n.s."),
    guide  = guide_legend(order = 2)
  ) +
  scale_shape_manual(
    values = level_shape_vals,
    name   = "Time Level",
    breaks = c("Momentary", "Weekly", "Monthly"),
    guide  = guide_legend(order = 1)    # Time Level ABOVE
  ) +
  guides(
    color = "none",
    shape = guide_legend(
      override.aes = list(size = 3),
      order = 1                          
    )) +
  labs(x = "Liberalism                                            Conservatism\n \nAssociations Between Political Orientation and Social Media Use (H2)",
       y = "") +
  theme_clean +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("plots/PO_mediators_platforms.png", width = 12, height = 8, dpi = 300)
# ggsave("plots/PO_mediators_activities.png", width = 12, height = 8, dpi = 300)
