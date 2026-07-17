############INFLUENCE OF RAINFALL & NICHE WIDTH, ON REPRODUCTIVE INVESTMENT (REPRODUCTIVE STATUS)#############
############ENIOLA OLU-AYORINDE###################
############2026-10-06##############################
# All Reproductive Status related calculations

library(readr)
library(dplyr)
library(tidyverse)

# ── Load data ──────────────────────────────────────────────────────────────────
Fish_info <- read_csv("data_paper_csvs/H.marginatus_food_items - fish info (3).csv")
View(Fish_info)

# ── Check reproductive status column ──────────────────────────────────────────
sum(is.na(Fish_info$reproductive_status))
table(Fish_info$reproductive_status)
table(Fish_info$sex, Fish_info$reproductive_status)

# ── Calculate mean reproductive status per event and sex ──────────────────────
mean_RS_by_event <- Fish_info %>%
  group_by(event_id, sex) %>%
  summarise(
    n      = n(),
    mean_RS = mean(reproductive_status, na.rm = TRUE),
    sd_RS   = sd(reproductive_status,   na.rm = TRUE)
  ) %>%
  arrange(as.numeric(gsub("E", "", event_id)))

View(mean_RS_by_event)
write.csv(mean_RS_by_event, "mean_RS_by_event.csv")

# ── Separate male from female ──────────────────────────────────────────────────
mean_RS_females <- mean_RS_by_event %>%
  filter(sex == "F")

mean_RS_males <- mean_RS_by_event %>%
  filter(sex == "M")

View(mean_RS_females)
View(mean_RS_males)

# ── Combine into one wide dataset ─────────────────────────────────────────────
mean_RS_combined <- merge(mean_RS_females, mean_RS_males,
                          by     = "event_id",
                          suffixes = c("_female", "_male")) %>%
  arrange(as.numeric(gsub("E", "", event_id)))

View(mean_RS_combined)
write.csv(mean_RS_combined, "mean_RS_combined.csv")

# ── Boxplot of reproductive status by sex ─────────────────────────────────────
library(ggplot2)

ggplot(Fish_info, aes(x = sex, y = reproductive_status, fill = sex)) +
  geom_boxplot() +
  labs(title = "Reproductive Status by Sex",
       x     = "Sex",
       y     = "Reproductive Status") +
  theme_classic()