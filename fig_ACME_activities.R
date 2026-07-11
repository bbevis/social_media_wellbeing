

library(dplyr)
library(ggplot2)
library(ggstance)
# install.packages("ggh4x")
library(ggh4x)

monthly_res = read.csv('monthly_results_withcontrols.csv')
weekly_res = read.csv('weekly_results_withcontrols.csv')
momentary_res = read.csv('momentary_results_withcontrols.csv')

activities <- c("information","social","productivity","education","entertainment",
                "creativity","games","shopping","utilities","other","health", "total_activity", "sm_minutes")

# bind monthly/weekly/momentary, recode labels, summarize,
# add significance + level×sign color factor, title-case mediator
make_summary_df <- function(monthly_res, weekly_res, momentary_res, activities) {
  bind_rows(
    monthly_res %>%
      filter(mediator %in% activities) %>%
      select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Monthly"),
    weekly_res %>%
      filter(mediator %in% activities) %>%
      select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Weekly"),
    momentary_res %>%
      filter(mediator %in% activities) %>%
      select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Momentary")
  ) %>%
    mutate(
      outcome = dplyr::recode(
        outcome,
        "awb_momentary" = "Overall Well-being",
        "awb_t2"        = "Overall Well-being",
        "awb_week"      = "Overall Well-being",
        "swls_t2"       = "Life Satisfaction",
        "uls_t2"        = "Loneliness (R)",
        "angry_rev"     = "Angry (R)",
        "worried_rev"   = "Worried (R)",
        "happy"         = "Happy",
        "sad_rev"       = "Sad (R)",
        "stressed_rev"  = "Stressed (R)",
        "lonely_rev"    = "Lonely (R)",
        "accepted"      = "Accepted"
      ),
      mediator = dplyr::recode(
        mediator,
        "total_activity" = "Activities (Total)",
        "sm_minutes"     = "App Use Duration"
      )
    ) %>%
    dplyr::group_by(mediator, level, outcome) %>%
    dplyr::summarise(
      m   = mean(ACME, na.rm = TRUE),
      lwr = mean(ACME_CI_lower, na.rm = TRUE),
      upr = mean(ACME_CI_upper, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # ensure lwr <= upr, then derive significance from CI excluding 0
    dplyr::mutate(
      lwr = pmin(lwr, upr),
      upr = pmax(lwr, upr),
      level = factor(level, levels = c("Momentary","Weekly","Monthly")),
      sig   = !(lwr <= 0 & upr >= 0) & is.finite(lwr) & is.finite(upr),
      sig_color = factor(
        ifelse(sig, paste0(level, "_sig"), paste0(level, "_ns")),
        levels = c("Momentary_sig","Weekly_sig","Monthly_sig",
                   "Momentary_ns","Weekly_ns","Monthly_ns")
      ),
      mediator = tools::toTitleCase(mediator)
    )
}

df_sum <- make_summary_df(monthly_res, weekly_res, momentary_res, activities)

outcome_order <- c(
  "Overall Well-being",
  "Life Satisfaction",
  "Loneliness (R)",
  " ",
  "Accepted",
  "Happy",
  "Angry (R)",
  "Lonely (R)",
  "Sad (R)",
  "Stressed (R)",
  "Worried (R)"
)

# Use an ordered factor for y
df_sum <- df_sum %>%
  mutate(outcome_f = factor(outcome, levels = outcome_order))

# Positions for separators between rows
sep_y <- seq(1.5, length(outcome_order) - 0.5, by = 1)

#############################################
#   - Colours = Platform (mediator)
#   - Shapes  = Time level (Monthly/Weekly/Momentary)
#   - Alpha   = Significance (NS lighter)
#############################################

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

# assign activity colours
activity_colors <- c(
  "Information"    = "#1f78b4",  # strong blue
  "Social"         = "#e31a1c",  # deep red
  "Productivity"   = "#33a02c",  # medium green
  "Education"      = "#6a3d9a",  # purple
  "Entertainment"  = "#ff7f00",  # orange
  "Creativity"     = "#b15928",  # brown
  "Games"          = "#a6cee3",  # light blue
  "Health"         = "#cc6677",  # rose
  "Shopping"       = "#fb9a99",  # light red/pink
  "Utilities"      = "#b2df8a",  # light green
  "Other"          = "#cab2d6",  # light lavender
  "Activities (Total)" = "#fdbf6f",  # goldenrod
  "App Use Duration"  = "#010101"   #black (neutral, for overall usage)
)

# Add a color variable that greys out non-sig
df_sum <- df_sum %>%
  mutate(color_var = ifelse(sig, mediator, "n.s."))

# Add grey for NS to palette
color_palette <- c(activity_colors, "n.s." = "grey85")

ggplot(
  df_sum,
  aes(
    x = m,
    y = outcome_f,
    color = color_var,   # mediator if sig, else "NS"
    shape = level,
    group = interaction(level, mediator)
  )
) +
  geom_hline(yintercept = sep_y, linewidth = 0.3, colour = "grey85") +
  geom_vline(xintercept = 0, linewidth = 0.4, alpha = 0.5) +
  ggstance::geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 1,
    position = ggstance::position_dodgev(height = 1)
  ) +
  geom_point(
    size = 2.2,
    position = ggstance::position_dodgev(height = 1)
  ) +
  scale_y_discrete(limits = rev(outcome_order), drop = FALSE,
                   expand = expansion(mult = c(0.05, 0.05))) +   # keep the blank spacer row visible
  scale_shape_manual(values = level_shape_vals, name = "Time level") +
  scale_color_manual(values = color_palette, name = "Platform") +
  labs(x = "Average Causal Mediated Effects of Activities", y = "") +
  theme_clean +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    axis.text.y = element_text(lineheight = 1.5)
  ) + guides(
    shape = guide_legend(order = 1),  # Time level on top
    color = guide_legend(order = 2),  # Platform/Significance below
    fill = "none",
    linetype = "none",
    size = "none",
    alpha = "none"
  )
ggsave("plots/Activities_ACME_singlecol.png", width = 12, height = 14, dpi = 300)
