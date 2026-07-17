library(tidyverse)
library(readxl)
library(lme4)

# ── Load individual data ───────────────────────────────────────────────────────
fish <- read_csv("data_paper_csvs/H.marginatus_food_items - fish info (3).csv")
View(fish)
# ── Calculate GSI ──────────────────────────────────────────────────────────────
# GSI = (gonad weight / total body weight) * 100
fish <- fish %>%
  mutate(GSI = (gonad_weight / total_weight) * 100)

# ── Separate by sex ────────────────────────────────────────────────────────────
females <- fish %>% filter(sex == "F")
males   <- fish %>% filter(sex == "M")

# ── Quick check ────────────────────────────────────────────────────────────────
cat("Females:", nrow(females), "\n")
cat("Males:  ", nrow(males), "\n")

summary(females$GSI)
summary(males$GSI)

# ── Join with your environmental variables ─────────────────────────────────────
env <- read_excel("raw_data/Variables_for_chapter_4.xlsx") %>%
  rename(
    Li = Levins_index,
    p  = Precip_p1_MERGE,
    p3 = Precip_p15_MERGE
  )

# Join on event_id so each fish gets its rainfall + diet values
females_full <- females %>% left_join(env, by = "event_id")
males_full   <- males   %>% left_join(env, by = "event_id")

# ── Check join worked ──────────────────────────────────────────────────────────
glimpse(females_full)
View(females_full)

# ── Models using individual-level GSI ─────────────────────────────────────────
# Individual nested within sampling event, within location
model_f1 <- lmer(GSI ~ p3 + (1|Location/event_id), data = females_full)
model_m1 <- lmer(GSI ~ p3 + (1|Location/event_id), data = males_full)

model_f2 <- lmer(GSI ~ Li + (1|Location/event_id), data = females_full)
model_m2 <- lmer(GSI ~ Li + (1|Location/event_id), data = males_full)

model_f3 <- lmer(GSI ~ Li + p3 + (1|Location/event_id), data = females_full)
model_m3 <- lmer(GSI ~ Li + p3 + (1|Location/event_id), data = males_full)

summary(model_f1)
summary(model_m1)
summary(model_f2)
summary(model_m2)
summary(model_f3)
summary(model_m3)

# ── Re-run power analysis with new n ──────────────────────────────────────────
library(pwr)
pwr.f2.test(u = 3,
            v = nrow(females_full) - 3 - 1,
            f2 = 0.15,
            sig.level = 0.05)


# If GSI is very low variance, nothing will predict it
hist(females$GSI)
hist(males$GSI)

var(females$GSI, na.rm = TRUE)
var(males$GSI, na.rm = TRUE)

# Check what is actually driving female variance
# Is it Location? Season? 
install.packages("performance")
library(performance)

model_f1 <- lmer(GSI ~ p3 + (1|Location/event_id), data = females_full)
icc(model_f1)  # how much variance is explained by Location vs individual?

# Is most of the female variance between locations or within?
females_full %>%
  group_by(Location) %>%
  summarise(mean = mean(GSI, na.rm = TRUE),
            var  = var(GSI, na.rm = TRUE))


ggplot(females_full, aes(x = Month, y = GSI, group = Location, colour = Location)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  labs(title = "Female GSI across seasons")


table(females$reproductive_status)
table(males$reproductive_status)

# Does stage distribution differ by month?
table(females_full$Month, females_full$reproductive_status)
table(males_full$Month,   males_full$reproductive_status)




females_full$reproductive_status <- factor(females_full$reproductive_status, 
                                           ordered = TRUE)
males_full$reproductive_status   <- factor(males_full$reproductive_status, 
                                           ordered = TRUE)

# Verify it worked
class(females_full$reproductive_status)
levels(females_full$reproductive_status)
install.packages("ordinal")
library(ordinal)
# This is likely your strongest and most honest model
model_f_final <- clmm(reproductive_status ~ Month + (1|Location),
                      data = females_full)

model_m_final <- clmm(reproductive_status ~ Month + (1|Location),
                      data = males_full)

summary(model_f_final)
summary(model_m_final)

# Then test whether diet adds anything beyond season
model_f_Li_final <- clmm(reproductive_status ~ Month + Li + (1|Location),
                         data = females_full)

AIC(model_f_final, model_f_Li_final)


library(dplyr)
library(tidyverse)
# Recode month into explicit rainfall season
females_full <- females_full %>%
  mutate(season = ifelse(Month %in% c("April", "July"), "Wet", "Dry"))

males_full <- males_full %>%
  mutate(season = ifelse(Month %in% c("April", "July"), "Wet", "Dry"))

model_f_season <- clmm(reproductive_status ~ season + (1|Location),
                       data = females_full)
model_m_season <- clmm(reproductive_status ~ season + (1|Location),
                       data = males_full)

summary(model_f_season)
summary(model_m_season)




# Run the same model on individual-level data
model_f_month_ind <- lmer(GSI ~ Month + (1|Location/event_id), 
                          data = females_full)
model_m_month_ind <- lmer(GSI ~ Month + (1|Location/event_id), 
                          data = males_full)

summary(model_f_month_ind)
summary(model_m_month_ind)

# And diet
model_f_Li_month_ind <- lmer(GSI ~ Li + Month + (1|Location/event_id), 
                             data = females_full)
summary(model_f_Li_month_ind)

# AIC comparison
AIC(model_f_month_ind, model_f_Li_month_ind)