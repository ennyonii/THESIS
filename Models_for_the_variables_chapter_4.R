library(readxl)
library(lme4)
library(car)
library(lmerTest)
library(dplyr)
library(tidyverse)
library(patchwork)

# ── Load data ──────────────────────────────────────────────────────────────────
Chapter_4 <- read_excel("raw_data/Variables_for_chapter_4.xlsx")

p33 <- Chapter_4$precip_p3_MERGE
p7  <- Chapter_4$precip_p7_MERGE
Li  <- Chapter_4$Levins_index
Gf  <- Chapter_4$mean_GSI_female
Gm  <- Chapter_4$mean_GSI_male

# ── COMBINED MODELS (all variables together) ───────────────────────────────────
# Female: p33 + p7 + Li
model_f_combined <- lmer(Gf ~ p33 + p7 + Li + (1|Location),
                         data = Chapter_4)
summary(model_f_combined)

# Male: p33 + p7 + Li
model_m_combined <- lmer(Gm ~ p33 + p7 + Li + (1|Location),
                         data = Chapter_4)
summary(model_m_combined)

# ── VIF check ─────────────────────────────────────────────────────────────────
vif(model_f_combined)
vif(model_m_combined)

# ── Correlation between rainfall variables ─────────────────────────────────────
cor(Chapter_4$precip_p3_MERGE, Chapter_4$precip_p7_MERGE,
    use = "complete.obs")

# ── FIGURE — 2 rows, 3 columns ────────────────────────────────────────────────
plot_data <- Chapter_4 %>%
  select(Location, Month, mean_GSI_female, mean_GSI_male,
         precip_p3_MERGE, precip_p7_MERGE, Levins_index) %>%
  rename(Gf  = mean_GSI_female,
         Gm  = mean_GSI_male,
         p33 = precip_p3_MERGE,
         p7  = precip_p7_MERGE,
         Li  = Levins_index)

# ── Base theme ─────────────────────────────────────────────────────────────────
pt <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11),
        axis.title = element_blank(),
        axis.text = element_text(size = 10))

# ── ROW 1: Female GSI ─────────────────────────────────────────────────────────
pA <- ggplot(plot_data, aes(x = p33, y = Gf)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "A - p = 0.020") +
  pt +
  theme(axis.title.y = element_text(size = 13)) +
  ylab("Mean female GSI (%)")

pB <- ggplot(plot_data, aes(x = p7, y = Gf)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "B - p = 0.074") + pt

pC <- ggplot(plot_data, aes(x = Li, y = Gf)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "C") + pt

# ── ROW 2: Male GSI ───────────────────────────────────────────────────────────
pD <- ggplot(plot_data, aes(x = p33, y = Gm)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "D") +
  pt +
  theme(axis.title.y = element_text(size = 13)) +
  ylab("Mean male GSI (%)")

pE <- ggplot(plot_data, aes(x = p7, y = Gm)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "E") + pt

pF <- ggplot(plot_data, aes(x = Li, y = Gm)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "F - p = 0.052") +
  pt +
  theme(axis.title.x = element_text(size = 13)) +
  xlab("Levins index")

# ── Bottom row x axis labels for A and B ──────────────────────────────────────
pD <- pD +
  theme(axis.title.x = element_text(size = 13)) +
  xlab("3-day precipitation (mm)")

pE <- pE +
  theme(axis.title.x = element_text(size = 13)) +
  xlab("7-day precipitation (mm)")

# ── Combine — 2 rows, 3 columns ───────────────────────────────────────────────
combined <- wrap_plots(pA, pB, pC,
                       pD, pE, pF,
                       nrow = 2)

combined
ggsave("GSI_rainfall_diet_models.png", plot = combined,
       width = 12, height = 8, dpi = 300)
# MODEL SECTION 2 ---------------------------------------------------------


# Add season column to Chapter_4
Chapter_4 <- Chapter_4 %>%
  mutate(season = ifelse(Month %in% c("April", "July"), "Wet", "Dry"))

#use season for model

model46 <- lmer(Gf ~ season + (1|Location), 
                data = Chapter_4)
summary(model46)

model47 <- lmer(Gm ~ season + (1|Location), 
                data = Chapter_4)
summary(model47)


model50 <- lmer(Gf ~ Li+ season + (1|Location), 
                data = Chapter_4)
summary(model50)

model51 <- lmer(Gm ~ Li+ season + (1|Location), 
                data = Chapter_4)
summary(model51)


# MODEL SECTION 3 ---------------------------------------------------------



#use month for model

model52 <- lmer(Gf ~ Month + (1|Location), 
                data = Chapter_4)
summary(model52)

model53 <- lmer(Gm ~ Month + (1|Location), 
                data = Chapter_4)
summary(model53)

model56 <- lmer(Gf ~ Li+ Month + (1|Location), 
                data = Chapter_4)
summary(model56)

model57 <- lmer(Gm ~ Li+ Month + (1|Location), 
                data = Chapter_4)
summary(model57)

model70 <- lmer(Gf ~ Li + Month + p33 + (1|Location), data = Chapter_4)
summary(model70) 
model71 <- lmer(Gm ~ Li + Month + p33 + (1|Location), data = Chapter_4)
summary(model71)  

# MODEL SECTION 4 ---------------------------------------------------------


#use reproductive status for response variable (using mean reproductive status for Lmer)
model58 <- lmer(Rf ~ Month + (1|Location), 
                data = Chapter_4)
summary(model58)

model59 <- lmer(Rm ~ Month + (1|Location), 
                data = Chapter_4)
summary(model59)

model60 <- lmer(Rf ~ Li + (1|Location), 
                data = Chapter_4)
summary(model60)

model61 <- lmer(Rm ~ Li + (1|Location), 
                data = Chapter_4)
summary(model61)

model62 <- lmer(Rf ~ Li+ Month + (1|Location), 
                data = Chapter_4)
summary(model62)

model63 <- lmer(Rm ~ Li+ Month + (1|Location), 
                data = Chapter_4)
summary(model63)

model75 <- lmer(Rf ~ Li + Month + p3 + (1|Location), 
                data = Chapter_4)
summary(model75) 


model74 <- lmer(Rm ~ Li + Month + p3 + (1|Location), 
                data = Chapter_4)
summary(model74) 

# MODEL SECTION 5 ---------------------------------------------------------

#using reproduction status as response variable, individual level (not population) and ordinal mixed model
install.packages("ordinal")
library(ordinal)
library(tidyverse)
library(readxl)
library(lme4)

# ── Load data ──────────────────────────────────────────────────────────────────
fish <- read_csv("data_paper_csvs/H.marginatus_food_items - fish info (3).csv")

fish <- fish %>%
  mutate(GSI = (gonad_weight / total_weight) * 100)

females <- fish %>% filter(sex == "F")
males   <- fish %>% filter(sex == "M")

summary(females$GSI)
summary(males$GSI)

# ── Load environmental data ────────────────────────────────────────────────────
env <- read_excel("raw_data/Variables_for_chapter_4.xlsx") %>%
  rename(Li = Levins_index)

# ── Join environmental data ────────────────────────────────────────────────────
females_full <- females %>% left_join(env, by = "event_id")
males_full   <- males   %>% left_join(env, by = "event_id")

# ── Create rainfall variables for both sexes ───────────────────────────────────
females_full$p3  <- females_full$Precip_p15_MERGE
females_full$p33 <- females_full$precip_p3_MERGE
females_full$p7  <- females_full$precip_p7_MERGE
females_full$p   <- females_full$Precip_p1_MERGE

males_full$p3    <- males_full$Precip_p15_MERGE
males_full$p33   <- males_full$precip_p3_MERGE
males_full$p7    <- males_full$precip_p7_MERGE
males_full$p     <- males_full$Precip_p1_MERGE

# ── Convert reproductive status to ordered factor ──────────────────────────────
females_full$reproductive_status <- factor(females_full$reproductive_status,
                                           levels = 1:5,
                                           ordered = TRUE)
males_full$reproductive_status   <- factor(males_full$reproductive_status,
                                           levels = 1:5,
                                           ordered = TRUE)

# ── Order months ───────────────────────────────────────────────────────────────
month_order <- c("January", "April", "July", "October")
females_full <- females_full %>%
  mutate(Month = factor(Month, levels = month_order))
males_full <- males_full %>%
  mutate(Month = factor(Month, levels = month_order))

# ── Scale predictors ───────────────────────────────────────────────────────────
females_full$Li_z  <- as.numeric(scale(females_full$Li))
females_full$p_z   <- as.numeric(scale(females_full$p))
females_full$p3_z  <- as.numeric(scale(females_full$p3))
females_full$p33_z <- as.numeric(scale(females_full$p33))
females_full$p7_z  <- as.numeric(scale(females_full$p7))

males_full$Li_z    <- as.numeric(scale(males_full$Li))
males_full$p_z     <- as.numeric(scale(males_full$p))
males_full$p3_z    <- as.numeric(scale(males_full$p3))
males_full$p33_z   <- as.numeric(scale(males_full$p33))
males_full$p7_z    <- as.numeric(scale(males_full$p7))

# ── Models ─────────────────────────────────────────────────────────────────────

# Month models
model64 <- clmm(reproductive_status ~ Month + (1|Location),
                data = females_full)
model65 <- clmm(reproductive_status ~ Month + (1|Location),
                data = males_full)

# Levins index only
model66 <- clmm(reproductive_status ~ Li_z + (1|Location),
                data = females_full)
model67 <- clmm(reproductive_status ~ Li_z + (1|Location),
                data = males_full)

# Levins index + Month
model68 <- clmm(reproductive_status ~ Li_z + Month + (1|Location),
                data = females_full)
model69 <- clmm(reproductive_status ~ Li_z + Month + (1|Location),
                data = males_full)

# 15-day and 1-day rainfall
model79 <- clmm(reproductive_status ~ p3_z + p_z + (1|Location),
                data = females_full)
model80 <- clmm(reproductive_status ~ p3_z + p_z + (1|Location),
                data = males_full)

# Levins + Month + 1-day rainfall
model77 <- clmm(reproductive_status ~ Li_z + Month + p_z + (1|Location),
                data = females_full)
model78 <- clmm(reproductive_status ~ Li_z + Month + p_z + (1|Location),
                data = males_full)

# 3-day and 7-day rainfall
model120 <- clmm(reproductive_status ~ p33_z + p7_z + (1|Location),
                 data = females_full)
model121 <- clmm(reproductive_status ~ p33_z + p7_z + (1|Location),
                 data = males_full)

# ── Summaries ──────────────────────────────────────────────────────────────────
summary(model64)
summary(model65)
summary(model66)
summary(model67)
summary(model68)
summary(model69)
summary(model79)
summary(model80)
summary(model77)
summary(model78)
summary(model120)
summary(model121)

# ── AIC comparisons ────────────────────────────────────────────────────────────
AIC(model64, model68)
AIC(model65, model69)
AIC(model120, model64)
AIC(model121, model65)




# Check p33 is in fish
cat("p33 in fish:", "p33" %in% names(fish), "\n")
cat("p7 in fish:", "p7" %in% names(fish), "\n")

# Recreate females_full and males_full from the current fish object
females_full <- fish %>%
  filter(sex == "F") %>%
  mutate(reproductive_status = factor(reproductive_status,
                                      levels = 1:5,
                                      ordered = TRUE))

males_full <- fish %>%
  filter(sex == "M") %>%
  mutate(reproductive_status = factor(reproductive_status,
                                      levels = 1:5,
                                      ordered = TRUE))

# Confirm p33 is now there
cat("p33 in females_full:", "p33" %in% names(females_full), "\n")
cat("p33 in males_full:", "p33" %in% names(males_full), "\n")

# Now scale
females_full$p33_z <- as.numeric(scale(females_full$p33))
females_full$p7_z  <- as.numeric(scale(females_full$p7))
males_full$p33_z   <- as.numeric(scale(males_full$p33))
males_full$p7_z    <- as.numeric(scale(males_full$p7))

# Run models
model120 <- clmm(reproductive_status ~ p33_z + p7_z + (1|Location),
                 data = females_full)
model121 <- clmm(reproductive_status ~ p33_z + p7_z + (1|Location),
                 data = males_full)

summary(model120)
summary(model121)

