library(dplyr)
library(ggplot2)
library(ggstance)
# install.packages("ggh4x")
library(ggh4x)
library(ggrepel)
library(stringr)

monthly_res = read.csv('monthly_results_nocontrols.csv')
weekly_res = read.csv('weekly_results_nocontrols.csv')
momentary_res = read.csv('momentary_results_nocontrols.csv')

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other", "total_activity")
minutes <- c("sm_minutes")

mediator_order <- c(
  "Facebook",
  "Twitter",
  "TikTok",
  "Instagram",
  "YouTube",
  "Reddit",
  "Snapchat",
  "Information",
  "Social Media",
  "Productivity",
  "Education",
  "Entertainment",
  "Creativity",
  "Games",
  "Shopping",
  "Utilities",
  "Other",
  "Overall Screentime"
)

outcome_labels <- c(
  "awb_momentary" = "Affective Wellbeing",
  "awb_week" = "Affective Wellbeing",
  "awb_t2" = "Affective Wellbeing",
  "swls_t2" = "Life Satisfaction",
  "uls_t2" = "Loneliness",
  "accepted" = "Accepted",
  "angry_rev" = "Anger",
  "happy" = "Happiness",
  "lonely_rev" = "Loneliness",
  "sad_rev" = "Sadness",
  "stressed_rev" = "Stress",
  "worried_rev" = "Worry"
)

outcome_order <- c(
  "Affective Wellbeing",
  "Life Satisfaction",
  "Loneliness",
  "Accepted",
  "Anger",
  "Happiness",
  "Sadness",
  "Stress",
  "Worry"
)

outcome_shape_vals <- c(
  "Affective Wellbeing" = 16,
  "Life Satisfaction" = 17,
  "Loneliness" = 15,
  "Accepted" = 5,
  "Anger" = 3,
  "Happiness" = 4,
  "Sadness" = 8,
  "Stress" = 7,
  "Worry" = 9
)

color_palette <- c(
  "Facebook" = "#0072B2",
  "Twitter" = "#56B4E9",
  "TikTok" = "#010101",
  "Instagram" = "#CC79A7",
  "YouTube" = "#D55E00",
  "Reddit" = "#E41A1C",
  "Snapchat" = "#F0E442",
  "Social Media" = "#7B3294",
  "Information" = "#332288",
  "Productivity" = "#88CCEE",
  "Education" = "#1B9E77",
  "Entertainment" = "#AA4499",
  "Creativity" = "#117733",
  "Games" = "#999933",
  "Shopping" = "#DDCC77",
  "Utilities" = "#E69F00",
  "Other" = "#882255",
  "Overall Screentime" = "#661100",
  "n.s." = "grey85"
)


bubble_df <- bind_rows(
  monthly_res %>%
    dplyr::select(mediator, outcome, path_a, path_b, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Monthly"),
  weekly_res %>%
    dplyr::select(mediator, outcome,path_a, path_b, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Weekly"),
  momentary_res %>%
    dplyr::select(mediator, outcome,path_a, path_b, prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Momentary")
) %>%
  # keep mediators you care about across platforms, activities, and total social media time
  filter(mediator %in% c(platforms, activities, minutes)) %>%
  mutate(
    mediator = case_when(
      mediator %in% c("total_platform", "social", "sm_minutes") ~ "Social Media",
      mediator == "total_activity" ~ "Overall Screentime",
      TRUE ~ mediator
    ),
    level = factor(level, levels = c("Momentary", "Weekly", "Monthly")),
    outcome = dplyr::recode(outcome, !!!outcome_labels),
    outcome = factor(outcome, levels = outcome_order),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(
      mediator,
      "Tiktok" = "TikTok",
      "Youtube" = "YouTube"
    )
  ) %>%
  group_by(outcome, mediator, level) %>%
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
    color_var = if_else(sig_prop_med, as.character(mediator), "n.s.")
  ) %>%
  filter(is.finite(a), is.finite(b), is.finite(prop_med))

bubble_df <- bubble_df %>%
  mutate(
    color_var = factor(color_var, levels = c(mediator_order, "n.s."))
  )

x_limit <- max(abs(bubble_df$a), na.rm = TRUE) * 1.12
y_limit <- max(abs(bubble_df$b), na.rm = TRUE) * 1.12
x_limit <- max(0.2, ceiling(x_limit * 10) / 10)
y_limit <- max(0.2, ceiling(y_limit * 10) / 10)

axis_labels <- function(x) sprintf("%.1f", x)

# clean theme
theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

p <- ggplot(bubble_df, aes(x = a, y = b)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  geom_point(
    aes(size = abs(prop_med), color = color_var, shape = outcome),
    alpha = 0.65
  ) +
  scale_size_area(
    max_size = 17,
    name = "Proportion mediated",
    breaks = c(0.025, 0.05, 0.10),
    labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = color_palette,
    name = NULL,
    breaks = mediator_order,
    guide  = guide_legend(override.aes = list(size = 5), order = 2)
  ) +
  scale_shape_manual(values = outcome_shape_vals,
                     name = "Wellbeing outcome",
                     guide  = guide_legend(override.aes = list(size = 5, color = "black"), order = 1)) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-y_limit, y_limit, by = 0.1),
    labels = axis_labels
  ) +
  scale_x_continuous(limits = c(-x_limit, x_limit),
                     breaks = seq(-x_limit, x_limit, by = 0.1),
                     labels = axis_labels) +
  facet_wrap(~ level, ncol = 1) +
  labs(
    x = "Liberalism                                       Conservatism\n\nAssociations between Political Orientation and Social/Digital Media Use",
    y = "Associations between Social/Digital Media Use and Wellbeing"
  ) +
  theme_clean +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(2.2, "lines"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 23, lineheight = 1.05, margin = margin(t = 14)),
        axis.title.y = element_text(size = 23, lineheight = 1.05, margin = margin(r = 14)),
        strip.text = element_text(size = 22, face = "bold", hjust = 0.5),
        strip.background = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 18),
        legend.key.height = unit(0.9, "lines"),
        plot.margin = margin(t = 18, r = 18, b = 18, l = 18))


ggsave("plots/mediation_bubble_map.png", plot = p, width = 15, height = 18, dpi = 300)
