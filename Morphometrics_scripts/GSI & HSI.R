############INFLUENCE OF RAINFALL & NICHE WIDTH, ON REPRODUCTIVE INVESTMENT#############
############ENIOLA OLU-AYORINDE###################
############2026-10-06##############################
# Population-level GSI and HSI analysis

library(readr)
library(dplyr)
library(tidyverse)
library(readxl)

# ── Load data ──────────────────────────────────────────────────────────────────
Fish_info <- read_csv("data_paper_csvs/H.marginatus_food_items - fish info (3).csv")

# ── Load environmental data ────────────────────────────────────────────────────
env <- read_excel("raw_data/Variables_for_chapter_4.xlsx") %>%
  rename(
    Li = Levins_index,
    p  = Precip_p1_MERGE,
    p3 = Precip_p15_MERGE
  )

# ── Calculate GSI and HSI ──────────────────────────────────────────────────────
Fish_info <- Fish_info %>%
  mutate(
    GSI = (gonad_weight / total_weight) * 100,
    HSI = (liver_weight / total_weight) * 100
  )

# ── Join environmental variables onto individual fish data ─────────────────────
fish <- Fish_info %>%
  left_join(env %>% select(event_id, Month, Location, Biome, Li, p, p3),
            by = "event_id")

# ── Quick checks ───────────────────────────────────────────────────────────────
sum(is.na(fish$GSI))
sum(is.na(fish$HSI))
summary(fish$GSI)
summary(fish$HSI)

# ── Population-level means per event ──────────────────────────────────────────
pop_repro <- fish %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(event_id, sex, Month, Location, Biome, p3) %>%
  summarise(
    mean_GSI = mean(GSI, na.rm = TRUE),
    sd_GSI   = sd(GSI,   na.rm = TRUE),
    mean_HSI = mean(HSI, na.rm = TRUE),
    sd_HSI   = sd(HSI,   na.rm = TRUE),
    n        = n(),
    .groups  = "drop"
  )

View(pop_repro)
write.csv(pop_repro, "pop_repro_summary.csv")

# ── Order months correctly for plotting ───────────────────────────────────────
pop_repro <- pop_repro %>%
  mutate(Month = factor(Month, levels = c("January", "April", "July", "October")))

# ── Plot 1: Population mean GSI by month and location ─────────────────────────
GSI_by_location <- ggplot(pop_repro, 
                          aes(x = Month, y = mean_GSI,
                              group = sex, colour = sex)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_GSI - sd_GSI,
                    ymax = mean_GSI + sd_GSI), width = 0.2) +
  facet_wrap(~Location) +
  theme_bw() +
  labs(title = "Population mean GSI by month and location",
       y     = "Mean GSI (%)",
       x     = "Month")

GSI_by_location
ggsave("GSI_by_location.png", plot = GSI_by_location,
       width = 12, height = 8, dpi = 300)

# ── Plot 2: GSI and HSI together to show energetic trade-off ──────────────────
GSI_HSI_tradeoff <- pop_repro %>%
  pivot_longer(cols      = c(mean_GSI, mean_HSI),
               names_to  = "index",
               values_to = "value") %>%
  mutate(index = case_when(
    index == "mean_GSI" ~ "GSI",
    index == "mean_HSI" ~ "HSI",
    TRUE ~ index
  )) %>%
  ggplot(aes(x = Month, y = value, colour = index, group = index)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(sex ~ Location) +
  theme_bw() +
  labs(title  = "GSI and HSI across sampling months by sex and location",
       y      = "Index value (%)",
       x      = "Month",
       colour = "Index")

GSI_HSI_tradeoff
ggsave("GSI_HSI_tradeoff.png", plot = GSI_HSI_tradeoff,
       width = 14, height = 8, dpi = 300)