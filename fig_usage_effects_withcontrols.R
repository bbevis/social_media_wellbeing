library(dplyr)
library(ggplot2)
library(ggstance)
# install.packages("ggh4x")
library(ggh4x)
library(ggrepel)
library(stringr)

monthly_res = read.csv('monthly_results_withcontrols.csv')
weekly_res = read.csv('weekly_results_withcontrols.csv')
momentary_res = read.csv('momentary_results_withcontrols.csv')

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other","health", "total_activity")
minutes <- c("sm_minutes")

wb_outcome <- c(Monthly = "awb_t2", Weekly = "awb_week", Momentary = "awb_momentary")


bubble_df <- bind_rows(
  monthly_res %>%
    filter(outcome == wb_outcome[["Monthly"]]) %>%
    dplyr::select(mediator, path_a, path_b, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Monthly"),
  weekly_res %>%
    filter(outcome == wb_outcome[["Weekly"]]) %>%
    dplyr::select(mediator, path_a, path_b, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Weekly"),
  momentary_res %>%
    filter(outcome == wb_outcome[["Momentary"]]) %>%
    dplyr::select(mediator, path_a, path_b, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Momentary")
) %>%
  # keep mediators you care about (platforms + activities; include totals if you wish)
  filter(mediator %in% c(platforms, minutes)) %>%
  group_by(mediator, level) %>%
  summarise(
    a    = mean(path_a,         na.rm = TRUE),  # Path A (x)
    b    = mean(path_b,         na.rm = TRUE),  # Path B to Overall WB (y)
    prop_med = mean(prop_mediated,           na.rm = TRUE),  # bubble size
    lwr  = mean(ACME_CI_lower,  na.rm = TRUE),
    upr  = mean(ACME_CI_upper,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sig_prop_med = !(lwr < 0 & upr > 0) & is.finite(lwr) & is.finite(upr),
    type = dplyr::case_when(
      mediator %in% platforms  ~ "Platform",
      mediator %in% activities ~ "Activity",
      TRUE ~ "Aggregate"
    ),
    mediator = case_when(
      mediator == "total_platform"  ~ "Social Media Apps (Included)",
      mediator == "total_activity"  ~ "All Apps",
      mediator == "sm_minutes"  ~ "Social Media Apps (Total)",
      TRUE ~ mediator
    ),
    level = factor(level, levels = c("Momentary","Weekly","Monthly")),
    mediator = tools::toTitleCase(mediator)
  ) %>%
  filter(is.finite(a), is.finite(b), is.finite(prop_med))

topN <- 17  # change this number to show more or fewer
top_labels <- bubble_df %>%
  dplyr::arrange(desc(abs(prop_med))) %>%
  dplyr::slice(1:topN) %>%
  mutate(mediator = str_wrap(mediator, width = 20))

level_shape_vals <- c(
  "Monthly"   = 16,  # filled circle
  "Weekly"    = 17,  # filled triangle
  "Momentary" = 15   # filled square
)

# Exclude shopping and utilities due to small size
bubble_df <- bubble_df %>%
  filter(!mediator%in%c('Shopping', 'Utilities', 'Reddit',
                        "Social Media Apps (Included)"))

# clean theme
theme_clean <- theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

sig_colors <- c(`TRUE` = "#2a9d8f", `FALSE` = "grey85")

ggplot(bubble_df, aes(x = a, y = b)) +
  geom_hline(yintercept = 0, colour = "grey75", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "grey75", linetype = "dashed") +
  geom_point(aes(size = abs(prop_med), color = sig_prop_med, shape = level)) +
  ggrepel::geom_text_repel(
    data = top_labels,
    aes(label = mediator),
    size = 5,
    box.padding = 1,
    point.padding = 1.8,
    force         = 2,
    segment.color = "grey75",
    max.overlaps = Inf
  ) +
  scale_size_area(
    max_size = 16,
    name = "Proportion mediated",
    breaks = c(0.025, 0.05, 0.10),   # pick meaningful breakpoints for your data
    labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = sig_colors, guide = "none") +
  scale_shape_manual(values = level_shape_vals, name = "Time level") +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  # scale_x_continuous(limits = c(-1.5, 1.5),
  #                    breaks = seq(-1.5, 1.5, by = 0.5)) +
  # guides(
  #   color = "none",
  #   shape = guide_legend(override.aes = list(size = 3))
  # ) +
  labs(
    x = "Liberalism                                       Conservatism\n \nRelationship Between Political Orientation and App Use (H2)",
    y = "Relationship Between App Use and Wellbeing (H3)\n \nLower wellbeing         Higher wellbeing "
  ) +
  theme_clean +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "right")


ggsave("plots/mediation_bubble_map_withcontrols.png", width = 20, height = 8, dpi = 300)
