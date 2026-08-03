#############################################
#
# Path A - Political orientation on social media use
# Study 2 (ANES Wave 3)
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)

path_a_res <- read.csv("anes_w3_path_a_results.csv")

mediator_labels <- c(
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "twitter_frequency" = "Twitter",
  "youtube_frequency" = "YouTube"
)

# Overall social media use is shown first, followed by platforms alphabetically.
mediator_order <- unname(mediator_labels)

plot_df <- path_a_res %>%
  filter(mediator %in% names(mediator_labels)) %>%
  transmute(
    mediator = dplyr::recode(mediator, !!!mediator_labels),
    m = estimate,
    lwr = ci_low,
    upr = ci_high,
    p = p,
    sig = p <= 0.05,
    color_var = if_else(sig, "sig", "n.s.")
  )

stopifnot(
  nrow(plot_df) == length(mediator_order),
  !anyDuplicated(plot_df$mediator),
  all(is.finite(plot_df$m)),
  all(is.finite(plot_df$lwr)),
  all(is.finite(plot_df$upr))
)

plot_df <- plot_df %>%
  mutate(mediator = factor(mediator, levels = mediator_order))

color_palette <- c("sig" = "#276CF5", "n.s." = "grey85")

x_limit <- max(abs(c(plot_df$lwr, plot_df$upr)), na.rm = TRUE)
x_limit <- max(0.15, ceiling(x_limit / 0.05) * 0.05)

y_axis_labels <- function(x) {
  labels <- ifelse(
    x == "Social Media",
    "bold('Social Media')",
    paste0("'", x, "'")
  )
  parse(text = labels)
}

theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

p <- ggplot(
  plot_df,
  aes(x = m, y = mediator, color = color_var)
) +
  geom_hline(
    yintercept = seq(0.5, length(mediator_order) + 0.5, by = 1),
    color = "grey75",
    linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  ggstance::geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 0.6
  ) +
  geom_point(size = 3) +
  scale_x_continuous(
    limits = c(-x_limit, x_limit),
    breaks = seq(-x_limit, x_limit, by = 0.05)
  ) +
  scale_y_discrete(
    limits = rev(mediator_order),
    labels = y_axis_labels,
    drop = FALSE
  ) +
  scale_color_manual(values = color_palette, guide = "none") +
  labs(
    x = paste0(
      "Liberalism                                            Conservatism\n \n",
      "Associations Between Political Orientation and Social Media Use (H2)"
    ),
    y = ""
  ) +
  theme_clean +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "Plots/ANES_PO_mediators_platforms.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)
