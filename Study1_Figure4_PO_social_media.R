#############################################
#
# Figure 4: Political orientation and social media use (H2)
#
#############################################

# Load packages and path a results
# study1_path_a_results.csv is produced by Study1_path_a_analysis.R.

library(dplyr)
library(ggplot2)
library(stringr)

path_a_res <- read.csv("study1_path_a_results.csv")

# Prepare social media estimates and confidence intervals

reported_mediators <- c(
  "facebook", "twitter", "tiktok", "instagram", "youtube", "reddit",
  "snapchat", "social", "sm_minutes"
)

# Shapes distinguish the three temporal levels.
level_shape_vals <- c(
  "Monthly"   = 16,  # filled circle
  "Weekly"    = 17,  # filled triangle
  "Momentary" = 15   # filled square
)

plot_df <- path_a_res %>%
  filter(mediator %in% reported_mediators) %>%
  transmute(
    level,
    mediator,
    m = estimate,
    lwr = ci_low,
    upr = ci_high,
    p
  ) %>%
  mutate(
    level = factor(level, levels = c("Monthly","Weekly","Momentary")),
    # Significant estimates are blue; other estimates are grey.
    sig   = p <= 0.05,
    # Give momentary social media use and the social app category
    # the same label in the plot.
    mediator = case_when(
      mediator %in% c("sm_minutes", "social") ~ "Social Media",
      TRUE ~ mediator
    ),
    mediator = tools::toTitleCase(mediator),
    mediator = dplyr::recode(mediator, "Tiktok" = "TikTok", "Youtube" = "YouTube"),
    color_var = ifelse(sig, "sig", "n.s.")
  )

# Set the order of social media measures

mediator_order <- c(
  "Social Media",
  "Facebook", "Instagram", "Reddit", "Snapchat", "TikTok", "Twitter", "YouTube"
)

mediator_order = rev(mediator_order)

# Check the expected estimates
# Expect eight monthly, eight weekly and one momentary estimate, with
# no duplicate level/measure combinations or non-finite estimates or limits.

stopifnot(
  nrow(plot_df) == 17,
  !anyDuplicated(plot_df[c("level", "mediator")]),
  all(is.finite(plot_df$m)),
  all(is.finite(plot_df$lwr)),
  all(is.finite(plot_df$upr))
)

# Set axis labels and plot theme

plot_df <- plot_df %>%
  mutate(mediator = factor(mediator, levels = mediator_order))

y_limits <- seq(1.5, length(unique(plot_df$mediator)) - 0.5, by = 1)

y_axis_labels <- function(x) {
  labels <- ifelse(
    x == "Social Media",
    "bold('Social Media')",
    paste0("'", str_wrap(x, width = 20), "'")
  )
  parse(text = labels)
}

# Use a white background and remove unnecessary grid lines
theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

# Create and save Figure 4

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
  # Draw thin horizontal lines between social media measures
  geom_hline(
    yintercept = seq(0.5, length(y_limits) + 0.5, by = 1),
    color = "grey75", linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  scale_x_continuous(limits = c(-.5, .5),
                     breaks = seq(-.5, .5, by = .2)) +
  scale_y_discrete(labels = y_axis_labels) +
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
    guide  = guide_legend(order = 1)    # Place the temporal-level legend first
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

ggsave("plots/Study1_Figure4_PO_social_media.png", width = 12, height = 8, dpi = 300)
