#############################################
#
#       ANES social media use by age
#       Spearman correlations
#
#############################################

library(dplyr)
library(tidyr)

#############################################
# Load data

anes_w3 <- read.csv("anes_w3.csv")

platform_vars <- c(
  "facebook_frequency",
  "instagram_frequency",
  "reddit_frequency",
  "tiktok_frequency",
  "youtube_frequency",
  "twitter_frequency"
)

platform_labels <- c(
  "facebook_frequency" = "Facebook",
  "instagram_frequency" = "Instagram",
  "reddit_frequency" = "Reddit",
  "tiktok_frequency" = "TikTok",
  "youtube_frequency" = "YouTube",
  "twitter_frequency" = "Twitter"
)

#############################################
# Spearman correlations

format_p <- function(p) {
  case_when(
    is.na(p) ~ NA_character_,
    p < .001 ~ "<.001",
    TRUE ~ sub("^0\\.", ".", sprintf("%.3f", p))
  )
}

spearman_by_platform <- anes_w3 %>%
  dplyr::select(age, all_of(platform_vars)) %>%
  pivot_longer(
    cols = all_of(platform_vars),
    names_to = "platform",
    values_to = "frequency"
  ) %>%
  filter(!is.na(age), !is.na(frequency)) %>%
  mutate(
    platform = dplyr::recode(platform, !!!platform_labels)
  ) %>%
  group_by(platform) %>%
  summarise(
    n = n(),
    spearman_rho = suppressWarnings(
      cor(age, frequency, method = "spearman", use = "complete.obs")
    ),
    p = suppressWarnings(
      cor.test(age, frequency, method = "spearman", exact = FALSE)$p.value
    ),
    .groups = "drop"
  ) %>%
  mutate(
    rho_squared = spearman_rho^2,
    p_fmt = format_p(p),
    platform = factor(
      platform,
      levels = c("Facebook", "Instagram", "Reddit", "TikTok", "YouTube", "Twitter")
    )
  ) %>%
  arrange(desc(abs(spearman_rho))) %>%
  mutate(
    spearman_rho = round(spearman_rho, 3),
    rho_squared = round(rho_squared, 3)
  )

#############################################
# Print and save

print(spearman_by_platform)

write.csv(
  spearman_by_platform,
  "anes_social_media_age_spearman_correlations.csv",
  row.names = FALSE
)
