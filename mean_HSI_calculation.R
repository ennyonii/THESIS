############INFLUENCE OF RAINFALL & NICHE WIDTH, ON REPRODUCTIVE INVESTMENT (HSI)#############
############ENIOLA OLU-AYORINDE###################
############2026-10-06##############################
# All HSI related calculations

library(readr)
library(dplyr)
library(tidyverse)

# ── Load data ──────────────────────────────────────────────────────────────────
Fish_info <- read_csv("data_paper_csvs/H.marginatus_food_items - fish info (3).csv")
View(Fish_info)

# ── Calculate HSI ──────────────────────────────────────────────────────────────
# HSI = (liver weight / total body weight) * 100
Fish_info <- Fish_info %>%
  mutate(HSI = (liver_weight / total_weight) * 100)
View(Fish_info)

# ── Check HSI column ───────────────────────────────────────────────────────────
sum(is.na(Fish_info$HSI))
sum(Fish_info$total_weight == 0, na.rm = TRUE)
summary(Fish_info$HSI)

# ── Calculate mean HSI per event and sex ──────────────────────────────────────
mean_HSI_by_event <- Fish_info %>%
  group_by(event_id, sex) %>%
  summarise(
    n        = n(),
    mean_HSI = mean(HSI, na.rm = TRUE),
    sd_HSI   = sd(HSI,   na.rm = TRUE)
  ) %>%
  arrange(as.numeric(gsub("E", "", event_id)))

View(mean_HSI_by_event)
write.csv(mean_HSI_by_event, "mean_HSI_by_event.csv")

# ── Separate male from female ──────────────────────────────────────────────────
mean_HSI_females <- mean_HSI_by_event %>%
  filter(sex == "F")

mean_HSI_males <- mean_HSI_by_event %>%
  filter(sex == "M")

View(mean_HSI_females)
View(mean_HSI_males)

# ── Combine into one wide dataset ─────────────────────────────────────────────
mean_HSI_combined <- merge(mean_HSI_females, mean_HSI_males,
                           by       = "event_id",
                           suffixes = c("_female", "_male")) %>%
  arrange(as.numeric(gsub("E", "", event_id)))

View(mean_HSI_combined)
write.csv(mean_HSI_combined, "mean_HSI_combined.csv")

# ── Boxplot of HSI by sex ──────────────────────────────────────────────────────
library(ggplot2)

ggplot(Fish_info, aes(x = sex, y = HSI, fill = sex)) +
  geom_boxplot() +
  labs(title = "Hepatosomatic Index by Sex",
       x     = "Sex",
       y     = "Hepatosomatic Index (HSI)") +
  theme_classic()