#############################################
#
# Figure S1: Political orientation and digital media use
#
#############################################

# Load packages and path a results
# study1_path_a_results.csv is produced by Study1_path_a_analysis.R.

library(dplyr)
library(ggplot2)
library(stringr)

path_a_res <- read.csv("study1_path_a_results.csv")

# Prepare digital media estimates and confidence intervals
# Include overall screentime and ten app categories at monthly and weekly levels.

reported_mediators <- c(
  "total_activity", "creativity", "education", "entertainment", "games",
  "health", "information", "other", "productivity", "shopping", "utilities"
)

# Shapes distinguish the two temporal levels.
level_shape_vals <- c(
  "Monthly"   = 16,  # filled circle
  "Weekly"    = 17   # filled triangle
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
    level = factor(level, levels = c("Monthly","Weekly")),
    # Significant estimates are blue; other estimates are grey.
    sig   = p <= 0.05,
    mediator = case_when(
      mediator == "total_activity" ~ "Overall Screentime",
      TRUE ~ mediator
    ),
    mediator = tools::toTitleCase(mediator),
    color_var = ifelse(sig, "sig", "n.s.")
  )

# Set the order of digital media measures

mediator_order <- c(
  "Overall Screentime", "Creativity", "Education", "Entertainment", "Games",
  "Health", "Information", "Other", "Productivity", "Shopping", "Utilities"
)

mediator_order = rev(mediator_order)

# Check the expected estimates
# Expect 22 estimates: eleven monthly and eleven weekly, with no duplicate
# level/measure combinations or non-finite estimates or confidence limits.

stopifnot(
  nrow(plot_df) == 22,
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
    x == "Overall Screentime",
    "bold('Overall Screentime')",
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

# Create and save Figure S1

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
  # Draw thin horizontal lines between digital media measures
  geom_hline(
    yintercept = seq(0.5, length(y_limits) + 0.5, by = 1),
    color = "grey75", linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  scale_x_continuous(limits = c(-.2, .2),
                     breaks = seq(-.2, .2, by = .1)) +
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
    breaks = c("Weekly", "Monthly"),
    guide  = guide_legend(order = 1)    # Place the temporal-level legend first
  ) +
  guides(
    color = "none",
    shape = guide_legend(
      override.aes = list(size = 3),
      order = 1                          
    )) +
  labs(x = "Liberalism                                            Conservatism\n \nAssociations Between Political Orientation and Digital Media Use (H2)",
       y = "") +
  theme_clean +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("plots/Study1_FigureS1_PO_digital_media.png", width = 12, height = 8, dpi = 300)
