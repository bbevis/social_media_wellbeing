#############################################
#
# Path C - Political orientation on Wellbeing
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)

monthly_res = read.csv('monthly_results_nocontrols.csv')
weekly_res = read.csv('weekly_results_nocontrols.csv')
momentary_res = read.csv('momentary_results_nocontrols.csv')


vars = c("awb_week", "awb_momentary", "angry_rev", "worried_rev", "happy", "sad_rev", "stressed_rev", "lonely_rev", "accepted", "swls_t2", "uls_t2", "awb_t2")



level_shape_vals <- c(
  "Monthly"   = 16,  # filled circle
  "Weekly"    = 17,  # filled triangle
  "Momentary" = 15   # filled square
)


plot_df <- monthly_res %>%
  filter(mediator%in%c('total_activity')) %>%
  dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
  mutate(level = 'Monthly') %>%
  bind_rows(
    weekly_res %>%
      filter(mediator%in%c('total_activity')) %>%
      dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
      mutate(level = 'Weekly')) %>%
  bind_rows(
    momentary_res %>%
      dplyr::select(outcome, path_c, path_c_CI_lower, path_c_CI_upper, path_c_p) %>%
      mutate(level = 'Momentary')) %>%
  group_by(outcome,level) %>%
  summarise(
    m   = mean(path_c),
    lwr = mean(path_c_CI_lower, na.rm = TRUE),
    upr = mean(path_c_CI_upper, na.rm = TRUE),
    p   = mean(path_c_p),
    .groups = "drop"
  ) %>%
  mutate(
    level = factor(level, levels = c("Monthly","Weekly","Momentary")),
    sig   = p <= 0.05,
    color_var = ifelse(sig, "sig", "n.s."),
    outcome = dplyr::recode(outcome,
                      "awb_momentary" = "Affective Wellbeing",
                      "awb_t2" = "Affective Wellbeing",
                      "awb_week" = "Affective Wellbeing",
                      "swls_t2" = "Life Satisfaction",
                      "uls_t2" = "Lonely (r)",
                      "angry_rev" = "Angry (r)",
                      "worried_rev" = "Worried (r)",
                      "happy" = "Happy",
                      "sad_rev" = "Sad (r)",
                      "stressed_rev" = "Stressed (r)",
                      "lonely_rev" = "Lonely (r)",
                      "accepted" = "Accepted")
    
  )



outcome_order <- c(
  "Affective Wellbeing",
  "Accepted",
  "Angry (r)",
  "Happy",
  "Life Satisfaction",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

color_palette <- c("sig" = "#008000", "n.s." = "grey85")


# Use an ordered factor for y
plot_df <- plot_df %>%
  mutate(
    outcome_f = factor(outcome, levels = outcome_order),
    color_var = ifelse(sig, "sig", "n.s.")
  )

y_limits <- seq(1.5, length(outcome_order) - 0.5, by = 1)

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
  aes(x = m, y = outcome_f, color = color_var, shape = level)) +
  geom_point(
    size = 3,
    position = position_dodgev(height = .5)) +
  # thin lines between every row
  geom_hline(
    yintercept = seq(0.5, length(y_limits) + 0.5, by = 1),
    color = "grey90", linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = .5,
    position = position_dodgev(height = .5)) +
  scale_x_continuous(limits = c(-.25, .25)) +
  scale_y_discrete(limits = rev(outcome_order), drop = FALSE,
                   expand = expansion(mult = c(0.05, 0.05))) +   # keep the blank spacer row visible
  scale_shape_manual(
    values = level_shape_vals,
    breaks = c("Momentary", "Weekly", "Monthly"),
    name = "Time level"
  ) +
  scale_color_manual(values = color_palette, guide = "none") +
  labs(x = "Liberalism                                                                            Conservatism\n \nAssociation Between Political Orientation and Wellbeing (H1)",
       y = "") +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    axis.text.y = element_text(lineheight = 1)
  )
ggsave("plots/PO_wellbeing.png", width = 12, height = 8, dpi = 300)
