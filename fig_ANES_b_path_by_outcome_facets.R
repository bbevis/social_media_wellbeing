#############################################
#
# Path B - Social media use on wellbeing by outcome
# Study 2 (ANES Wave 3)
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)

anes_res <- read.csv("anes_w3_mediation_results.csv")

mediator_labels <- c(
  "total_platform_frequency" = "Social Media",
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "twitter_frequency" = "Twitter",
  "youtube_frequency" = "YouTube"
)

outcome_labels <- c(
  "loneliness" = "Loneliness (r)",
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)"
)

# Overall social media use is shown first; outcomes are alphabetical.
mediator_order <- unname(mediator_labels)
outcome_order <- unname(outcome_labels)

plot_df <- anes_res %>%
  filter(
    sample == "all",
    model_type == "mediation",
    mediator %in% names(mediator_labels),
    outcome %in% names(outcome_labels)
  ) %>%
  transmute(
    mediator = dplyr::recode(mediator, !!!mediator_labels),
    outcome = dplyr::recode(outcome, !!!outcome_labels),
    m = b_path_est,
    lwr = b_path_ci_low,
    upr = b_path_ci_high,
    p = b_path_p,
    sig = p <= 0.05,
    color_var = if_else(sig, "sig", "n.s.")
  )

stopifnot(
  nrow(plot_df) == length(mediator_order) * length(outcome_order),
  !anyDuplicated(plot_df[c("mediator", "outcome")]),
  all(is.finite(plot_df$m)),
  all(is.finite(plot_df$lwr)),
  all(is.finite(plot_df$upr))
)

plot_df <- plot_df %>%
  mutate(
    mediator = factor(mediator, levels = rev(mediator_order)),
    outcome = factor(outcome, levels = outcome_order)
  )

x_limit <- max(abs(c(plot_df$lwr, plot_df$upr)), na.rm = TRUE)
x_limit <- max(0.2, ceiling(x_limit * 10) / 10)

y_axis_labels <- function(x) {
  labels <- ifelse(
    x == "Social Media",
    "bold('Social Media')",
    paste0("'", x, "'")
  )
  parse(text = labels)
}

theme_clean <- theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(face = "bold", size = 20, colour = "black"),
    axis.text.y = element_text(size = 15, lineheight = 0.95, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.title.x = element_text(
      size = 24,
      colour = "black",
      margin = margin(t = 10)
    )
  )

p <- ggplot(
  plot_df,
  aes(x = m, y = mediator, color = color_var)
) +
  geom_hline(
    yintercept = seq(1.5, length(mediator_order) - 0.5, by = 1),
    linewidth = 0.3,
    colour = "grey75"
  ) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  ggstance::geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 0.25,
    size = 0.45
  ) +
  geom_point(size = 2.5) +
  facet_wrap(~outcome, ncol = 3) +
  coord_cartesian(xlim = c(-x_limit, x_limit)) +
  scale_y_discrete(labels = y_axis_labels, drop = FALSE) +
  scale_color_manual(
    values = c("sig" = "#FF1493", "n.s." = "grey75"),
    guide = "none"
  ) +
  labs(
    x = "Associations Between Social Media Use and Wellbeing (H3)",
    y = ""
  ) +
  theme_clean

ggsave(
  "Plots/ANES_mediators_wellbeing_by_outcome_facets.png",
  plot = p,
  width = 18,
  height = 6,
  dpi = 300
)
