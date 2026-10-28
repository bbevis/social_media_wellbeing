#############################################
#
# Political orientation on digital media use (app categories)
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
library(stringr)

path_a_res <- read.csv("study1_path_a_results.csv")

reported_mediators <- c(
  "total_activity", "creativity", "education", "entertainment", "games",
  "health", "information", "other", "productivity", "shopping", "utilities"
)

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
    sig   = p <= 0.05,
    mediator = case_when(
      mediator == "total_activity" ~ "Overall Screentime",
      TRUE ~ mediator
    ),
    mediator = tools::toTitleCase(mediator),
    color_var = ifelse(sig, "sig", "n.s.")
  )

mediator_order <- c(
  "Overall Screentime", "Creativity", "Education", "Entertainment", "Games",
  "Health", "Information", "Other", "Productivity", "Shopping", "Utilities"
)

mediator_order = rev(mediator_order)

stopifnot(
  nrow(plot_df) == 22,
  !anyDuplicated(plot_df[c("level", "mediator")]),
  all(is.finite(plot_df$m)),
  all(is.finite(plot_df$lwr)),
  all(is.finite(plot_df$upr))
)

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
  scale_x_continuous(limits = c(-.2, .2),
                     breaks = seq(-.2, .2, by = .1)) +
  scale_y_discrete(labels = y_axis_labels) +
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
    breaks = c("Weekly", "Monthly"),
    guide  = guide_legend(order = 1)    # Time Level ABOVE
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

ggsave("plots/PO_mediators_activities.png", width = 12, height = 8, dpi = 300)
