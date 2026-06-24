

library(dplyr)
library(ggplot2)
library(ggstance)
# install.packages("ggh4x")
library(ggh4x)

monthly_res = read.csv('monthly_results_nocontrols.csv')
weekly_res = read.csv('weekly_results_nocontrols.csv')
momentary_res = read.csv('momentary_results_nocontrols.csv')

platforms  <- c("facebook","twitter","tiktok","instagram","youtube","reddit","snapchat", "total_platform", "sm_minutes")

# bind monthly/weekly/momentary, recode labels, summarize,
# add significance + level×sign color factor, title-case mediator
make_summary_df <- function(monthly_res, weekly_res, momentary_res, platforms) {
  bind_rows(
    monthly_res %>%
      filter(mediator %in% platforms) %>%
      dplyr::select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Monthly"),
    weekly_res %>%
      filter(mediator %in% platforms) %>%
      dplyr::select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
      mutate(level = "Weekly"),
    momentary_res %>%
      filter(mediator %in% platforms) %>%
      dplyr::select(mediator, outcome, ACME, ACME_CI_lower, ACME_CI_upper) %>%
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
        "total_platform" = "Social Media (Total)",
        "sm_minutes"     = "Social Media Use Duration"
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
        levels = c("Monthly_sig","Weekly_sig","Momentary_sig",
                   "Monthly_ns","Weekly_ns","Momentary_ns")
      ),
      mediator = tools::toTitleCase(mediator)
    )
}

df_sum <- make_summary_df(monthly_res, weekly_res, momentary_res, platforms)


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

# assign platform colours (brand-like example, can edit to taste)
platform_colors <- c(
  "Facebook"      = "#1877F2",
  "Twitter"       = "#00B6F1",
  "Tiktok"        = "#010101",
  "Instagram"     = "#E1306C",
  "Youtube"       = "#FF0000",
  "Reddit"        = "#FF4500",
  "Snapchat"      = "#FFFC00",
  "All Platforms" = "#6A3D9A",
  "All Minutes"   = "#33A02C"
)


# Add a color variable that greys out non-sig
df_sum <- df_sum %>%
  mutate(color_var = ifelse(sig, mediator, "n.s."))

# Add grey for NS to palette
color_palette <- c(platform_colors, "n.s." = "grey85")

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
  # row separators (between categories)
  geom_hline(yintercept = sep_y, linewidth = 0.3, colour = "grey85") +
  # zero line
  geom_vline(xintercept = 0, linewidth = 0.4, alpha = 0.5) +
  ggstance::geom_errorbarh(
    aes(xmin = lwr, xmax = upr),
    height = 1,
    position = ggstance::position_dodgev(height = 0.6)
  ) +
  geom_point(
    size = 2.2,
    position = ggstance::position_dodgev(height = 0.6)
  ) +
  scale_shape_manual(values = level_shape_vals, name = "Time level") +
  scale_color_manual(values = color_palette, name = "Platform") +
  labs(x = "Average Causal Mediated Effects of Platforms", y = "") +
  scale_y_discrete(limits = rev(outcome_order), drop = FALSE,
                   expand = expansion(mult = c(0.05, 0.05))) +   # keep the blank spacer row visible
  theme_clean +
  theme(
    # 4) turn off y-grid so only separators show
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # keep/adjust x-grid if you like
    # panel.grid.major.x = element_line(colour = "grey90"),
    legend.position = "right",
    legend.box = "vertical",
    axis.text.y = element_text(lineheight = 1.5)
  )
ggsave("plots/platforms_ACME.png", width = 12, height = 14, dpi = 300)
