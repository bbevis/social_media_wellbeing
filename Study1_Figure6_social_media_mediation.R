# Figure 6: Social media mediation (H4)

# Load packages and analysis results

library(dplyr)
library(ggplot2)

monthly_res = read.csv('monthly_results_withcontrols.csv')
weekly_res = read.csv('weekly_results_withcontrols.csv')
momentary_res = read.csv('momentary_results_withcontrols.csv')

# Select social media measures
# Include overall social media use and seven platforms; exclude app categories
# and overall screentime.

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
minutes <- c("sm_minutes")

# Set measure and outcome labels, order, shapes and colours

mediator_order <- c(
  "Social Media",
  "Facebook",
  "Instagram",
  "Reddit",
  "Snapchat",
  "TikTok",
  "Twitter",
  "YouTube",
  "Information",
  "Productivity",
  "Education",
  "Entertainment",
  "Creativity",
  "Games",
  "Health",
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
  "uls_t2" = "Lonely (r)",
  "accepted" = "Accepted",
  "angry_rev" = "Angry (r)",
  "happy" = "Happy",
  "lonely_rev" = "Lonely (r)",
  "sad_rev" = "Sad (r)",
  "stressed_rev" = "Stressed (r)",
  "worried_rev" = "Worried (r)"
)

outcome_order <- c(
  "Affective Wellbeing",
  "Life Satisfaction",
  "Accepted",
  "Angry (r)",
  "Happy",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

outcome_shape_vals <- c(
  "Affective Wellbeing" = 16,
  "Life Satisfaction" = 17,
  "Accepted" = 5,
  "Angry (r)" = 3,
  "Happy" = 4,
  "Lonely (r)" = 6,
  "Sad (r)" = 8,
  "Stressed (r)" = 7,
  "Worried (r)" = 9
)

color_palette <- c(
  "Facebook" = "#1877F2",
  "Twitter" = "#F28E2B",
  "TikTok" = "#8A3FFC",
  "Instagram" = "#C13584",
  "YouTube" = "#FF0000",
  "Reddit" = "#E41A1C",
  "Snapchat" = "#F0C808",
  "Social Media" = "#009E9A",
  "Information" = "#332288",
  "Productivity" = "#88CCEE",
  "Education" = "#F0E442",
  "Entertainment" = "#AA4499",
  "Creativity" = "#117733",
  "Games" = "#999933",
  "Health" = "#CC6677",
  "Shopping" = "#DDCC77",
  "Utilities" = "#E69F00",
  "Other" = "#882255",
  "Overall Screentime" = "#661100",
  "n.s." = "grey85"
)

# Prepare mediation estimates
# Average estimates within each outcome/measure/level. A group is significant
# only when all its row-level ACME significance flags are TRUE, using the
# existing all(sig_row, na.rm = TRUE) rule. Row-level significance requires
# finite ACME confidence limits that exclude zero.

bubble_df <- bind_rows(
  monthly_res %>%
    dplyr::select(mediator, outcome, path_a, path_b, path_a_p, path_b_p,
                  prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Monthly"),
  weekly_res %>%
    dplyr::select(mediator, outcome, path_a, path_b, path_a_p, path_b_p,
                  prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Weekly"),
  momentary_res %>%
    dplyr::select(mediator, outcome, path_a, path_b, path_a_p, path_b_p,
                  prop_mediated, ACME_CI_lower, ACME_CI_upper) %>%
    mutate(level = "Momentary")
) %>%
  # Keep the selected platforms and overall social media measures
  filter(mediator %in% c(platforms, minutes)) %>%
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
    ),
    sig_row = (ACME_CI_lower > 0 | ACME_CI_upper < 0) &
      is.finite(ACME_CI_lower) &
      is.finite(ACME_CI_upper)
  ) %>%
  group_by(outcome, mediator, level) %>%
  summarise(
    a    = mean(path_a,         na.rm = TRUE),  # Political orientation to social media use (horizontal axis)
    b    = mean(path_b,         na.rm = TRUE),  # Social media use to the specified wellbeing outcome (vertical axis)
    a_p  = mean(path_a_p,       na.rm = TRUE),
    b_p  = mean(path_b_p,       na.rm = TRUE),
    prop_med = mean(prop_mediated,           na.rm = TRUE),  # Absolute value determines bubble area
    lwr  = mean(ACME_CI_lower,  na.rm = TRUE),
    upr  = mean(ACME_CI_upper,  na.rm = TRUE),
    sig  = all(sig_row, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    color_var = if_else(sig, as.character(mediator), "n.s.")
  ) %>%
  filter(is.finite(a), is.finite(b), is.finite(prop_med))

bubble_df <- bubble_df %>%
  mutate(
    color_var = factor(color_var, levels = c(mediator_order, "n.s."))
  )

# Set axis ranges, labels and plot theme

x_limit <- max(abs(bubble_df$a), na.rm = TRUE) * 1.12
y_limit <- max(abs(bubble_df$b), na.rm = TRUE) * 1.12
x_limit <- max(0.2, ceiling(x_limit * 10) / 10)
y_limit <- max(0.2, ceiling(y_limit * 10) / 10)

axis_labels <- function(x) sprintf("%.1f", x)

# Use a white background and remove unnecessary grid lines
theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

# Create and save Figure 6
# Shapes identify wellbeing outcomes. Bubble area represents the absolute
# proportion mediated. Significant groups use measure-specific colours;
# non-significant groups are grey.

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
                     name = NULL,
                     guide  = guide_legend(override.aes = list(size = 5, color = "black"), order = 1)) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-y_limit, y_limit, by = 0.1),
    labels = axis_labels
  ) +
  scale_x_continuous(limits = c(-x_limit, x_limit),
                     breaks = seq(-x_limit, x_limit, by = 0.1),
                     labels = axis_labels) +
  labs(
    x = "Liberalism                                 Conservatism\n\nAssociations between Political Orientation and Social Media Use",
    y = "Associations between Social Media Use and Wellbeing\n\nLower wellbeing                              Higher wellbeing"
      ) +
  theme_clean +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 23, lineheight = 1.05, margin = margin(t = 14)),
        axis.title.y = element_text(size = 23, lineheight = 1.05, margin = margin(r = 14)),
        legend.position = "right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 18),
        legend.key.height = unit(0.9, "lines"),
        plot.margin = margin(t = 18, r = 18, b = 18, l = 18))

ggsave("plots/Study1_Figure6_social_media_mediation.png", plot = p, width = 15, height = 10, dpi = 300)
