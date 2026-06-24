#############################################
#
#       ANES social media use by age group
#
#############################################

library(dplyr)
library(tidyr)
library(ggplot2)

anes_w3 <- read.csv("anes_w3.csv")

platform_vars <- c(
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency"
)

frequency_labels <- c(
  "1" = "Never",
  "2" = "Less than once a month",
  "3" = "Once or twice a month",
  "4" = "About once a week",
  "5" = "A few times per week",
  "6" = "Once a day",
  "7" = "More than once a day"
)

platform_labels <- c(
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "youtube_frequency" = "YouTube",
  "twitter_frequency" = "Twitter"
)

age_labels <- c(
  "18-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80+"
)

#############################################
# Prepare data

plot_df <- anes_w3 %>%
  filter(!is.na(age)) %>%
  mutate(
    age_bin = cut(
      age,
      breaks = c(18, 30, 40, 50, 60, 70, 80, Inf),
      labels = age_labels,
      right = FALSE
    )
  ) %>%
  pivot_longer(
    cols = all_of(platform_vars),
    names_to = "platform",
    values_to = "frequency"
  ) %>%
  filter(!is.na(age_bin), !is.na(frequency)) %>%
  mutate(
    platform = dplyr::recode(platform, !!!platform_labels),
    platform = factor(
      platform,
      levels = c("Facebook", "Instagram", "Reddit", "TikTok", "YouTube", "Twitter")
    ),
    age_bin = factor(age_bin, levels = age_labels),
    frequency = factor(
      as.character(frequency),
      levels = as.character(1:7),
      labels = frequency_labels
    )
  ) %>%
  count(platform, age_bin, frequency, name = "n") %>%
  group_by(platform, age_bin) %>%
  mutate(
    total_n = sum(n),
    prop = n / total_n
  ) %>%
  ungroup()

#############################################
# Plot

frequency_palette <- c(
  "Never" = "#F2F2F2",
  "Less than once a month" = "#D9EAF7",
  "Once or twice a month" = "#B9D7EA",
  "About once a week" = "#8CBED6",
  "A few times per week" = "#5FA6C6",
  "Once a day" = "#2F7FAF",
  "More than once a day" = "#08519C"
)

theme_clean <- theme_minimal(base_size = 22) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

p <- ggplot(plot_df, aes(x = age_bin, y = prop, fill = frequency)) +
  geom_col(width = 0.78, colour = "white", linewidth = 0.25) +
  facet_wrap(~ platform, ncol = 3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = frequency_palette,
    name = "Use frequency",
    breaks = rev(names(frequency_palette))
  ) +
  labs(
    x = "Age group",
    y = "Percentage of respondents"
  ) +
  theme_clean +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.x = element_text(size = 24, margin = margin(t = 12)),
    axis.title.y = element_text(size = 24, margin = margin(r = 12)),
    strip.text = element_text(size = 24, face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    plot.margin = margin(t = 18, r = 18, b = 18, l = 18)
  )

p

ggsave(
  "Plots/fig_ANES_social_media_distribution.png",
  plot = p,
  width = 17,
  height = 11,
  dpi = 300
)
