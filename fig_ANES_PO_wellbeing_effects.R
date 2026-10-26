#############################################
#
# Path C - Political orientation on wellbeing
# Study 2 (ANES Wave 3)
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)

anes_res <- read.csv("anes_w3_mediation_results.csv")

outcome_labels <- c(
  "anhedonia" = "Anhedonia (r)",
  "depression" = "Depression (r)",
  "loneliness" = "Loneliness (r)"
)

# Outcomes are shown alphabetically.
outcome_order <- unname(outcome_labels)

plot_df <- anes_res %>%
  filter(
    sample == "all",
    model_type == "mediation",
    mediator == "total_platform_frequency",
    outcome %in% names(outcome_labels)
  ) %>%
  transmute(
    outcome = dplyr::recode(outcome, !!!outcome_labels),
    m = c_path_est,
    lwr = c_path_ci_low,
    upr = c_path_ci_high,
    p = c_path_p,
    sig = p <= 0.05,
    color_var = if_else(sig, "sig", "n.s.")
  )

stopifnot(
  nrow(plot_df) == length(outcome_order),
  !anyDuplicated(plot_df$outcome),
  all(is.finite(plot_df$m)),
  all(is.finite(plot_df$lwr)),
  all(is.finite(plot_df$upr))
)

plot_df <- plot_df %>%
  mutate(outcome = factor(outcome, levels = outcome_order))

color_palette <- c("sig" = "#008000", "n.s." = "grey85")

theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

p <- ggplot(
  plot_df,
  aes(x = m, y = outcome, color = color_var)
) +
  geom_hline(
    yintercept = seq(0.5, length(outcome_order) + 0.5, by = 1),
    color = "grey90",
    linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  ggstance::geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 0.5
  ) +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(-0.25, 0.25)) +
  scale_y_discrete(
    limits = rev(outcome_order),
    drop = FALSE,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_color_manual(values = color_palette, guide = "none") +
  labs(
    x = paste0(
      "Liberalism                                                                    ",
      "Conservatism\n \n",
      "Association Between Political Orientation and Wellbeing (H1)"
    ),
    y = ""
  ) +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 22, lineheight = 1)
  )

ggsave(
  "Plots/ANES_PO_wellbeing.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)
