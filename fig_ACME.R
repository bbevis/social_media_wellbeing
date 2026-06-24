
#############################################
#
# ACME - Mediators on Wellbeing
#
#############################################

library(dplyr)
library(ggplot2)
library(ggstance)
# install.packages("wesanderson")
library(wesanderson)

monthly_res = read.csv('monthly_results_nocontrols.csv')
weekly_res = read.csv('weekly_results_nocontrols.csv')
momentary_res = read.csv('momentary_results_nocontrols.csv')

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform")
activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other", "total_activity")
minutes <- c("sm_minutes")

plot_df <- monthly_res %>%
      dplyr::select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Monthly") %>%
    bind_rows(
      weekly_res %>%
      dplyr::select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Weekly")) %>%
  bind_rows(
    momentary_res %>%
      dplyr::select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Momentary")) %>%
  filter(mediator %in% c(platforms, minutes)) %>%
  group_by(mediator, level, outcome) %>%
  summarise(
    m   = mean(ACME, na.rm = TRUE),
    lwr = mean(ACME_CI_lower, na.rm = TRUE),
    upr = mean(ACME_CI_upper, na.rm = TRUE),
    .groups = "drop" 
  ) %>%
    mutate(
        level = factor(level, levels = c("Momentary","Weekly","Monthly")),
        type  = case_when(
          mediator %in% platforms  ~ "Platform",
          mediator %in% activities ~ "Activity",
          TRUE ~ "Rest"),
      outcome = dplyr::recode(
        outcome,
        "awb_momentary" = "Overall Well-being",
        "awb_t2"        = "Overall Well-being",
        "awb_week"      = "Overall Well-being",
        "swls_t2"       = "Life Satisfaction",
        "uls_t2"        = "Loneliness (r)",
        "angry_rev"     = "Angry (r)",
        "worried_rev"   = "Worried (r)",
        "happy"         = "Happy",
        "sad_rev"       = "Sad (r)",
        "stressed_rev"  = "Stressed (r)",
        "lonely_rev"    = "Lonely (r)",
        "accepted"      = "Accepted"
      ),
      mediator = case_when(
        mediator == "total_platform"  ~ "Social Media Apps (Included)",
        mediator == "total_activity"  ~ "All Apps",
        mediator == "sm_minutes"  ~ "Social Media Apps (Total)",
        TRUE ~ mediator
      ),
    # ensure lwr <= upr, then derive significance from CI excluding 0
      lwr = pmin(lwr, upr),
      upr = pmax(lwr, upr),
      level = factor(level, levels = c("Momentary","Weekly","Monthly")),
      sig = !(lwr < 0 & upr > 0) & is.finite(lwr) & is.finite(upr),
      mediator = tools::toTitleCase(mediator),
    color_var = ifelse(sig, "sig", "n.s.")
    )



# Axis orders
outcome_order <- c(
  "Overall Well-being",
  "Life Satisfaction",
  "Loneliness (r)",
  " ",
  "Accepted",
  "Happy",
  "Angry (r)",
  "Lonely (r)",
  "Sad (r)",
  "Stressed (r)",
  "Worried (r)"
)

mediator_order <- c(
  "Social Media Apps (Included)",
  "Social Media Apps (Total)",
  "All Apps",
  "Tiktok","Twitter", "Facebook", "Instagram", "Youtube",
  "Reddit", "Snapchat",
  "Social", "Creativity", "Entertainment", "Education",
  "Games", "Information", "Other", "Productivity",
  "Shopping", "Utilities"
)

mediator_order = rev(mediator_order)

# Wes Anderson palette for platforms (+ a grey for n.s.)
n_outcomes <- length(outcome_order)
wes_cols <- wesanderson::wes_palette("FantasticFox1", n_outcomes, type = "continuous")
names(wes_cols) <- outcome_order
color_palette <- c(wes_cols, "n.s." = "grey85")

# Use an ordered factor for y
plot_df <- plot_df %>%
  mutate(mediator = factor(mediator, levels = mediator_order),
         color_var = ifelse(sig, outcome, "n.s."))

# Positions for separators between rows
y_limits <- seq(1.5, length(outcome_order) - 0.5, by = 1)

# Exclude shopping and utilities due to small size
plot_df <- plot_df %>%
  filter(!mediator%in%c('Shopping', 'Utilities',
                        "Social Media Apps (Included)"))

# clean theme
theme_clean <- theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

level_shape_vals <- c(
  "Monthly"   = 16,  # filled circle
  "Weekly"    = 17,  # filled triangle
  "Momentary" = 15   # filled square
)

ggplot(plot_df, aes(x = m,
                   y = mediator,
                   color = color_var,
                   shape = level,
                   group = interaction(level, outcome))) + # row separators (between categories)
  geom_point(size = 2.2, position = position_dodgev(height = 1)) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr),
                           height = 1,
                           position = ggstance::position_dodgev(height = 1)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  # thin lines between every row
  geom_hline(yintercept = y_limits, linewidth = 0.3, colour = "grey75") +
  scale_y_discrete(limits = rev(outcome_order), drop = FALSE,
                   expand = expansion(mult = c(0.05, 0.05))) +   # keep the blank spacer row visible
  scale_color_manual(values = color_palette, name = "Outcomes",
                     breaks = setdiff(names(color_palette), "n.s.")) + # remove "n.s." from legend
  scale_x_continuous(limits = c(-.1, .1),
                     breaks = seq(-.1, .1, by = .05)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_shape_manual(values = level_shape_vals, name = "Time level") +
  labs(x = "Average Causal Mediated Effect (ACME)", y = "") +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    axis.text.y = element_text(lineheight = 1)
  )

ggsave("plots/ACME_platforms.png", width = 12, height = 8, dpi = 300)
# ggsave("plots/ACME_activities.png", width = 12, height = 8, dpi = 300)
