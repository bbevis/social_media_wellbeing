library(dplyr)
library(ggplot2)

#############################################
# Study 1 mediation bubble map: app categories

monthly_res <- read.csv("monthly_results_withcontrols.csv")
weekly_res <- read.csv("weekly_results_withcontrols.csv")
path_a_res <- read.csv("study1_path_a_results.csv")

app_categories <- c(
  "information", "productivity", "education", "entertainment",
  "creativity", "games", "shopping", "utilities", "other", "health",
  "total_activity"
)

mediator_order <- c(
  "Overall Screentime", "Creativity", "Education", "Entertainment", "Games",
  "Health", "Information", "Other", "Productivity", "Shopping", "Utilities"
)

outcome_labels <- c(
  "awb_week" = "Affective Wellbeing",
  "awb_t2" = "Affective Wellbeing",
  "swls_t2" = "Life Satisfaction",
  "accepted" = "Accepted",
  "angry_rev" = "Angry (r)",
  "happy" = "Happy",
  "lonely_rev" = "Lonely (r)",
  "uls_t2" = "Lonely (r)",
  "sad_rev" = "Sad (r)",
  "stressed_rev" = "Stressed (r)",
  "worried_rev" = "Worried (r)"
)

outcome_order <- c(
  "Affective Wellbeing", "Accepted", "Angry (r)", "Happy",
  "Life Satisfaction", "Lonely (r)", "Sad (r)", "Stressed (r)",
  "Worried (r)"
)

outcome_shape_vals <- c(
  "Affective Wellbeing" = 16,
  "Accepted" = 5,
  "Angry (r)" = 3,
  "Happy" = 4,
  "Life Satisfaction" = 17,
  "Lonely (r)" = 6,
  "Sad (r)" = 8,
  "Stressed (r)" = 7,
  "Worried (r)" = 9
)

color_palette <- c(
  "Overall Screentime" = "#661100",
  "Creativity" = "#117733",
  "Education" = "#F0E442",
  "Entertainment" = "#AA4499",
  "Games" = "#999933",
  "Health" = "#CC6677",
  "Information" = "#332288",
  "Other" = "#882255",
  "Productivity" = "#88CCEE",
  "Shopping" = "#DDCC77",
  "Utilities" = "#E69F00",
  "n.s." = "grey85"
)

#############################################

path_a_df <- path_a_res %>%
  filter(
    mediator %in% app_categories,
    IV == "political_orientation",
    level %in% c("Weekly", "Monthly")
  ) %>%
  transmute(
    mediator,
    level,
    a = estimate,
    a_p = p
  )

bubble_df <- bind_rows(
  weekly_res %>%
    transmute(
      mediator, outcome,
      b = path_b,
      b_p = path_b_p,
      prop_med = prop_mediated,
      acme_lwr = ACME_CI_lower,
      acme_upr = ACME_CI_upper,
      level = "Weekly"
    ),
  monthly_res %>%
    transmute(
      mediator, outcome,
      b = path_b,
      b_p = path_b_p,
      prop_med = prop_mediated,
      acme_lwr = ACME_CI_lower,
      acme_upr = ACME_CI_upper,
      level = "Monthly"
    )
) %>%
  filter(mediator %in% app_categories) %>%
  left_join(path_a_df, by = c("mediator", "level")) %>%
  mutate(
    sig = (acme_lwr > 0 | acme_upr < 0) &
      is.finite(acme_lwr) &
      is.finite(acme_upr),
    mediator = if_else(
      mediator == "total_activity",
      "Overall Screentime",
      mediator
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = factor(mediator, levels = mediator_order),
    outcome = recode(outcome, !!!outcome_labels),
    outcome = factor(outcome, levels = outcome_order),
    level = factor(level, levels = c("Weekly", "Monthly")),
    color_var = if_else(sig, as.character(mediator), "n.s."),
    color_var = factor(color_var, levels = c(mediator_order, "n.s."))
  ) %>%
  filter(
    !is.na(mediator), !is.na(outcome),
    is.finite(a), is.finite(b), is.finite(prop_med)
  ) %>%
  arrange(sig)

stopifnot(
  nrow(bubble_df) > 0,
  !anyNA(bubble_df$a_p),
  !anyNA(bubble_df$b_p)
)

x_limit <- max(abs(bubble_df$a), na.rm = TRUE) * 1.12
y_limit <- max(abs(bubble_df$b), na.rm = TRUE) * 1.12
x_limit <- max(0.2, ceiling(x_limit * 10) / 10)
y_limit <- max(0.2, ceiling(y_limit * 10) / 10)

axis_labels <- function(x) sprintf("%.1f", x)

theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

app_category_mediation_bubble_map <- ggplot(bubble_df, aes(x = a, y = b)) +
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
    guide = guide_legend(
      override.aes = list(size = 5, color = "black"),
      order = 1
    )
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
    x = paste0(
      "Liberalism                                 Conservatism\n\n",
      "Associations between Political Orientation and Digital Media Use"
    ),
    y = paste0(
      "Associations between Digital Media Use and Wellbeing\n\n",
      "Lower wellbeing                              Higher wellbeing"
    )
  ) +
  theme_clean +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(
      size = 23,
      lineheight = 1.05,
      margin = margin(t = 14)
    ),
    axis.title.y = element_text(
      size = 23,
      lineheight = 1.05,
      margin = margin(r = 14)
    ),
    legend.position = "right",
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 18),
    legend.key.height = grid::unit(0.9, "lines"),
    plot.margin = margin(t = 18, r = 18, b = 18, l = 18)
  )

ggsave(
  "Plots/app_category_mediation_bubble_map.png",
  plot = app_category_mediation_bubble_map,
  width = 15,
  height = 10,
  dpi = 300
)

print(app_category_mediation_bubble_map)
