#############################################
#
#       ANES main mediation figure
#
#############################################

library(dplyr)
library(ggplot2)

anes_res <- read.csv("anes_monotonic_mediation_results.csv")

platforms <- c(
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency"
)

mediator_order <- c(
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
  "depression" = "Depression (r)"
)

outcome_order <- unname(outcome_labels)

outcome_shape_vals <- c(
  "Loneliness (r)" = 16,
  "Anhedonia (r)" = 17,
  "Depression (r)" = 15
)

#############################################
# Prepare data

# Refit only the 18 weighted outcome regressions to extract their six b contrasts.
# These match Table S17; no mediation bootstrap is rerun.
d <- read.csv("anes_w3.csv")
main_res <- anes_res %>% filter(
  mediator %in% platforms, model_type == "mediation", sample == "all",
  outcome %in% names(outcome_labels)
)
contrast_labels <- c(
  "Less than once a month vs never",
  "Once/twice a month vs never",
  "About once a week vs never",
  "A few times/week vs never",
  "Once a day vs never",
  "More than once a day vs never"
)
b_rows <- lapply(seq_len(nrow(main_res)), function(i) {
  r <- main_res[i, ]
  vars <- c(r$mediator, r$outcome, "political_orientation", "age", "gender",
            "race_ethnicity", "income", "education", "w3weight")
  md <- d[complete.cases(d[, vars]) & is.finite(d$w3weight) & d$w3weight > 0, ]
  md$mediator_value <- factor(md[[r$mediator]], levels = 1:7)
  md$outcome_z <- as.numeric(scale(md[[r$outcome]]))
  fit <- lm(outcome_z ~ political_orientation + mediator_value + age + gender +
              race_ethnicity + income + education, data = md, weights = w3weight)
  stopifnot(nrow(md) == r$n,
            abs(coef(fit)["political_orientation"] - r$c_prime_est) < 1e-8)
  data.frame(mediator = r$mediator, outcome = r$outcome,
             contrast = factor(contrast_labels, levels = contrast_labels),
             y_est = unname(coef(fit)[paste0("mediator_value", 2:7)]))
})
fig_df <- main_res %>%
  inner_join(bind_rows(b_rows), by = c("mediator", "outcome")) %>%
  mutate(
    sig_mediation = (acme_ci_low > 0 | acme_ci_high < 0) &
      is.finite(acme_ci_low) &
      is.finite(acme_ci_high),
    outcome = dplyr::recode(outcome, !!!outcome_labels),
    mediator = dplyr::recode(
      mediator,
      "facebook_frequency" = "Facebook",
      "instagram_frequency" = "Instagram",
      "reddit_frequency" = "Reddit",
      "tiktok_frequency" = "TikTok",
      "youtube_frequency" = "YouTube",
      "twitter_frequency" = "Twitter"
    ),
    color_var = if_else(sig_mediation, mediator, "n.s.")
  ) %>%
  filter(is.finite(a_path_est), is.finite(y_est), is.finite(prop_mediated_est)) %>%
  mutate(
    outcome = factor(outcome, levels = outcome_order),
    color_var = factor(
      color_var,
      levels = c(mediator_order, "n.s.")
    )
  )

#############################################
# Plot settings

color_palette <- c(
  "Facebook" = "#1877F2",
  "Twitter" = "#F28E2B",
  "TikTok" = "#8A3FFC",
  "Instagram" = "#C13584",
  "YouTube" = "#2E7D32",
  "Reddit" = "#E41A1C",
  "n.s." = "grey85"
)

theme_clean <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

x_limit <- max(abs(fig_df$a_path_est), na.rm = TRUE) * 1.12
y_limit <- max(abs(fig_df$y_est), na.rm = TRUE) * 1.12

#############################################
# Create figure

p <- ggplot(fig_df, aes(x = a_path_est, y = y_est)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  geom_point(
    aes(
      size = abs(prop_mediated_est),
      color = color_var,
      shape = outcome
    ),
    alpha = 0.65
  ) +
  scale_size_area(
    max_size = 14,
    name = "Proportion mediated",
    breaks = c(0.05, 0.10, 0.20),
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
  facet_wrap(~contrast, ncol = 3) +
  scale_x_continuous(limits = c(-x_limit, x_limit), breaks = c(-0.2, 0, 0.2)) +
  scale_y_continuous(limits = c(-y_limit, y_limit)) +
  labs(
    x = "Liberalism                                       Conservatism\n\nAssociations Between\nPolitical Orientation and Social Media Use",
    y = "Associations Between\nSocial Media Use and Wellbeing\n\nLower wellbeing                              Higher wellbeing"
  ) +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 14),
    panel.spacing = unit(1.2, "lines"),
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
  "Plots/fig_ANES.png",
  plot = p,
  width = 20,
  height = 13,
  dpi = 300
)
