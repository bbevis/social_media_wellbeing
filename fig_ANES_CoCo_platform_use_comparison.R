#############################################
#
#       ANES and CoCo platform use comparison
#       Common social media platforms only
#
#############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

#############################################
# Load data

anes_w3 <- read.csv("anes_w3.csv")
weekly <- read.csv("weekly.csv")

#############################################
# Define variables

platform_lookup <- c(
  "facebook" = "Facebook",
  "instagram" = "Instagram",
  "reddit" = "Reddit",
  "tiktok" = "TikTok",
  "youtube" = "YouTube",
  "twitter" = "Twitter"
)

platform_order <- c(
  "Facebook",
  "Instagram",
  "Reddit",
  "TikTok",
  "Twitter",
  "YouTube"
)

anes_platform_vars <- c(
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency"
)

anes_labels <- c(
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "youtube_frequency" = "YouTube",
  "twitter_frequency" = "Twitter"
)

coco_platform_vars <- names(platform_lookup)

#############################################
# Prepare participant-level platform use

anes_long_all <- anes_w3 %>%
  select(caseid, young_adult, all_of(anes_platform_vars)) %>%
  pivot_longer(
    cols = all_of(anes_platform_vars),
    names_to = "platform",
    values_to = "raw_use"
  ) %>%
  mutate(
    dataset = "ANES",
    sample_group = "All adults",
    id = as.character(caseid),
    platform = dplyr::recode(platform, !!!anes_labels)
  ) %>%
  select(dataset, sample_group, id, platform, raw_use, young_adult)

anes_long_young <- anes_long_all %>%
  filter(young_adult == 1L) %>%
  mutate(sample_group = "Young adults")

anes_long <- bind_rows(anes_long_all, anes_long_young) %>%
  select(dataset, sample_group, id, platform, raw_use)

coco_long <- weekly %>%
  filter(ema_50_exclude == 0L) %>%
  select(
    id,
    days_in_week,
    all_of(coco_platform_vars),
    all_of(paste0(coco_platform_vars, "_exclude"))
  ) %>%
  pivot_longer(
    cols = all_of(coco_platform_vars),
    names_to = "platform",
    values_to = "raw_use"
  ) %>%
  mutate(
    platform_exclude = case_when(
      platform == "facebook" ~ facebook_exclude,
      platform == "instagram" ~ instagram_exclude,
      platform == "reddit" ~ reddit_exclude,
      platform == "tiktok" ~ tiktok_exclude,
      platform == "youtube" ~ youtube_exclude,
      platform == "twitter" ~ twitter_exclude,
      TRUE ~ NA_integer_
    ),
    raw_use = ifelse(platform_exclude == 0L, raw_use * (7 / days_in_week), NA_real_)
  ) %>%
  select(id, platform, raw_use) %>%
  group_by(id, platform) %>%
  summarise(
    raw_use = mean(raw_use, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    raw_use = ifelse(is.nan(raw_use), NA_real_, raw_use)
  ) %>%
  mutate(
    dataset = "CoCo",
    sample_group = "CoCo",
    id = as.character(id),
    platform = dplyr::recode(platform, !!!platform_lookup)
  ) %>%
  select(dataset, sample_group, id, platform, raw_use)

platform_use <- bind_rows(anes_long, coco_long) %>%
  filter(!is.na(raw_use)) %>%
  mutate(
    platform = factor(platform, levels = platform_order),
    dataset = factor(dataset, levels = c("CoCo", "ANES")),
    sample_group = factor(sample_group, levels = c("CoCo", "All adults", "Young adults"))
  )

#############################################
# Summaries

platform_summary <- platform_use %>%
  group_by(dataset, sample_group, platform) %>%
  summarise(
    n = n(),
    mean_raw = mean(raw_use, na.rm = TRUE),
    sd_raw = sd(raw_use, na.rm = TRUE),
    se_raw = sd_raw / sqrt(n),
    ci_raw = qt(0.975, df = n - 1) * se_raw,
    ci_low_raw = mean_raw - ci_raw,
    ci_high_raw = mean_raw + ci_raw,
    .groups = "drop"
  )

plot_df <- platform_summary %>%
  mutate(
    platform = factor(platform, levels = rev(platform_order)),
    group_offset = case_when(
      sample_group == "All adults" ~ -0.13,
      sample_group == "Young adults" ~ 0.13,
      TRUE ~ 0
    ),
    platform_y = as.numeric(platform) + group_offset
  )

write.csv(
  platform_summary,
  "platform_use_dataset_summary.csv",
  row.names = FALSE
)

write.csv(
  platform_summary,
  "platform_use_dataset_comparison.csv",
  row.names = FALSE
)

#############################################
# Plot

guide_color <- "grey85"
coco_young_adult_color <- "#276CF5"

anes_group_colors <- c(
  "All adults" = "#D55E00",
  "Young adults" = coco_young_adult_color
)

make_platform_plot <- function(dataset_name, x_title, show_y_axis = TRUE) {
  axis_text_y <- if (show_y_axis) {
    element_text(size = 18, color = "black")
  } else {
    element_blank()
  }

  x_limits <- if (dataset_name == "CoCo") c(0, 10) else c(1, 7)
  x_breaks <- if (dataset_name == "CoCo") waiver() else 1:7
  x_expand <- if (dataset_name == "CoCo") expansion(mult = c(0.05, 0.12)) else expansion(mult = c(0, 0))

  dataset_df <- filter(plot_df, dataset == dataset_name)
  row_boundaries <- seq(0.5, length(levels(plot_df$platform)) + 0.5, by = 1)

  p <- ggplot(dataset_df, aes(x = mean_raw, y = platform_y)) +
    geom_hline(
      yintercept = row_boundaries,
      color = guide_color,
      linewidth = 0.3
    )

  if (dataset_name == "ANES") {
    p <- p +
      geom_errorbar(
        aes(xmin = ci_low_raw, xmax = ci_high_raw, color = sample_group),
        linewidth = 0.6,
        width = 0.16
      ) +
      geom_point(
        aes(color = sample_group),
        size = 3.2
      ) +
      scale_color_manual(values = anes_group_colors, name = NULL)
  } else {
    p <- p +
      geom_errorbar(
        aes(xmin = ci_low_raw, xmax = ci_high_raw),
        color = coco_young_adult_color,
        linewidth = 0.6,
        width = 0.16
      ) +
      geom_point(color = coco_young_adult_color, size = 3.2)
  }

  p +
    scale_y_continuous(
      limits = c(0.5, length(levels(plot_df$platform)) + 0.5),
      breaks = seq_along(levels(plot_df$platform)),
      labels = levels(plot_df$platform),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      expand = x_expand
    ) +
    labs(
      title = dataset_name,
      x = x_title,
      y = NULL
    ) +
    theme_minimal(base_family = "Helvetica", base_size = 18) +
    theme(
      legend.position = if (dataset_name == "ANES") "top" else "none",
      legend.text = element_text(size = 14, color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text.x = element_text(size = 16, color = "black"),
      axis.text.y = axis_text_y,
      axis.ticks.y = if (show_y_axis) element_line(color = "black", linewidth = 0.5) else element_blank(),
      axis.title.x = element_text(size = 15, margin = margin(t = 10)),
      plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5),
      plot.margin = margin(10, 15, 10, 10)
    )
}

p_coco <- make_platform_plot(
  "CoCo",
  "Mean weekly app use (hours)"
)

p_anes <- make_platform_plot(
  "ANES",
  "Mean use frequency",
  show_y_axis = FALSE
)

p <- p_coco + p_anes +
  plot_layout(widths = c(1, 1), guides = "collect") &
  theme(
    legend.position = "top",
    legend.justification = "center"
  )

ggsave(
  "Plots/fig_ANES_CoCo_platform_use_comparison.png",
  p,
  width = 12,
  height = 6,
  dpi = 300
)

print(platform_summary)
