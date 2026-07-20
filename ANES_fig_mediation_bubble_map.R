#############################################
#
#       ANES mediation bubble map
#
#############################################

library(dplyr)
library(ggplot2)

anes_res <- read.csv("anes_w3_mediation_results.csv")

platforms <- c(
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency",
  "total_platform_frequency"
)

mediator_order <- c(
  "Social Media",
  "Facebook",
  "Instagram",
  "Reddit",
  "TikTok",
  "Twitter",
  "YouTube"
)

outcome_labels <- c(
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)",
  "affective_wellbeing" = "Affective Wellbeing",
  "happy" = "Happy",
  "hopeful" = "Hopeful",
  "afraid" = "Afraid (r)",
  "angry" = "Angry (r)",
  "outraged" = "Outraged (r)",
  "worried" = "Worried (r)"
)

outcome_order <- unname(outcome_labels)

outcome_shape_vals <- c(
  "Affective Wellbeing" = 16,
  "Happy" = 4,
  "Angry (r)" = 3,
  "Worried (r)" = 9,
  "Loneliness (r)" = 6,
  "Anhedonia (r)" = 17,
  "Depression (r)" = 15,
  "Hopeful" = 5,
  "Afraid (r)" = 8,
  "Outraged (r)" = 7
)

#############################################
# Prepare data

bubble_df <- anes_res %>%
  filter(
    mediator %in% platforms,
    model_type == "mediation",
    sample == "all"
  ) %>%
  dplyr::select(
    mediator,
    outcome,
    a_path_est,
    b_path_est,
    a_path_p,
    b_path_p,
    prop_mediated_est,
    acme_ci_low,
    acme_ci_high
  ) %>%
  transmute(
    mediator,
    outcome,
    a = a_path_est,
    b = b_path_est,
    a_p = a_path_p,
    b_p = b_path_p,
    prop_med = prop_mediated_est,
    lwr = acme_ci_low,
    upr = acme_ci_high
  ) %>%
  mutate(
    sig_prop_med = a_p <= 0.05 &
      b_p <= 0.05 &
      !(lwr < 0 & upr > 0) &
      is.finite(lwr) &
      is.finite(upr),
    outcome = dplyr::recode(outcome, !!!outcome_labels),
    outcome = factor(outcome, levels = outcome_order),
    mediator = dplyr::recode(
      mediator,
      "facebook_frequency" = "Facebook",
      "instagram_frequency" = "Instagram",
      "reddit_frequency" = "Reddit",
      "tiktok_frequency" = "TikTok",
      "youtube_frequency" = "YouTube",
      "twitter_frequency" = "Twitter",
      "total_platform_frequency" = "Social Media"
    ),
    color_var = if_else(sig_prop_med, mediator, "n.s.")
  ) %>%
  filter(is.finite(a), is.finite(b), is.finite(prop_med))

color_palette <- c(
  "Facebook" = "#1877F2",
  "Twitter" = "#F28E2B",
  "TikTok" = "#8A3FFC",
  "Instagram" = "#C13584",
  "YouTube" = "#D55E00",
  "Reddit" = "#E41A1C",
  "Social Media" = "#009E9A",
  "n.s." = "grey85"
)

bubble_df <- bubble_df %>%
  mutate(
    color_var = factor(
      color_var,
      levels = c(mediator_order, "n.s.")
    )
  )

#############################################
# Plot

theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

x_limit <- max(abs(bubble_df$a), na.rm = TRUE) * 1.12
y_limit <- max(abs(bubble_df$b), na.rm = TRUE) * 1.12
x_limit <- max(0.2, ceiling(x_limit * 10) / 10)
y_limit <- max(0.2, ceiling(y_limit * 10) / 10)

axis_labels <- function(x) sprintf("%.1f", x)

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
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = color_palette,
    name = NULL,
    breaks = mediator_order,
    guide = guide_legend(override.aes = list(size = 5), order = 2)
  ) +
  scale_shape_manual(
    values = outcome_shape_vals,
    name = "Wellbeing outcome",
    guide = guide_legend(override.aes = list(size = 5), order = 1)
  ) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-y_limit, y_limit, by = 0.1),
    labels = axis_labels
  ) +
  scale_x_continuous(
    limits = c(-x_limit, x_limit),
    breaks = seq(-x_limit, x_limit, by = 0.1),
    labels = axis_labels
  ) +
  labs(
    x = "Liberalism                                 Conservatism\n\nAssociations between Political Orientation and Social Media Use",
    y = "Associations between Social Media Use and Wellbeing\n\nLower wellbeing                              Higher wellbeing"
  ) +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 23, lineheight = 1.05, margin = margin(t = 14)),
    axis.title.y = element_text(size = 23, lineheight = 1.05, margin = margin(r = 14)),
    legend.position = "right",
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 18),
    legend.key.height = unit(0.9, "lines"),
    plot.margin = margin(t = 18, r = 18, b = 18, l = 18)
  )

p

ggsave(
  "Plots/ANES_mediation_bubble_map.png",
  plot = p,
  width = 15,
  height = 10,
  dpi = 300
)
