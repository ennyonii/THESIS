library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(car)
library(patchwork)

# ── Load data ──────────────────────────────────────────────────────────────────
fish <- read_csv("data_paper_csvs/H.marginatus_food_items - fish info (3).csv")

env <- read_excel("raw_data/Variables_for_chapter_4.xlsx") %>%
  rename(
    Li  = Levins_index,
    p   = Precip_p1_MERGE,
    p3  = Precip_p15_MERGE,
    p33 = precip_p3_MERGE,
    p7  = precip_p7_MERGE
  )

Chapter_4 <- read_excel("raw_data/Variables_for_chapter_4.xlsx") %>%
  rename(
    Li  = Levins_index,
    p   = Precip_p1_MERGE,
    p3  = Precip_p15_MERGE,
    p33 = precip_p3_MERGE,
    p7  = precip_p7_MERGE
  )

# ── Join and calculate indices ─────────────────────────────────────────────────
fish <- fish %>%
  left_join(env %>% select(event_id, Month, Location, Biome, Li, p, p3, p33, p7),
            by = "event_id") %>%
  mutate(
    SL_mm = standard_length_cm,
    GSI   = (gonad_weight / total_weight) * 100,
    HSI   = (liver_weight / total_weight) * 100,
    K     = (total_weight / SL_mm^3) * 100000
  )

# ── Order months ───────────────────────────────────────────────────────────────
month_order <- c("January", "April", "July", "October")
fish <- fish %>%
  mutate(Month = factor(Month, levels = month_order))

females <- fish %>% filter(sex == "F")
males   <- fish %>% filter(sex == "M")

# ══════════════════════════════════════════════════════════════════════════════
# 1. SEX RATIO
# ══════════════════════════════════════════════════════════════════════════════
cat("=== SEX RATIO ===\n")
fish %>% filter(sex %in% c("F","M")) %>% count(sex)

fish %>%
  filter(sex %in% c("F","M")) %>%
  count(Month, sex) %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  mutate(ratio_F_M = round(F/M, 2))

sex_counts <- table(fish$sex[fish$sex %in% c("F","M")])
chisq.test(sex_counts)

# ══════════════════════════════════════════════════════════════════════════════
# 2. LENGTH-WEIGHT RELATIONSHIP — WITH OUTLIER REMOVAL
# ══════════════════════════════════════════════════════════════════════════════

# Step 1: Fit initial models to identify outliers
females_clean <- females %>% filter(!is.na(total_weight), !is.na(SL_mm))
males_clean   <- males   %>% filter(!is.na(total_weight), !is.na(SL_mm))

lm_f_initial <- lm(log(total_weight) ~ log(SL_mm), data = females_clean)
lm_m_initial <- lm(log(total_weight) ~ log(SL_mm), data = males_clean)

females_clean <- females_clean %>% mutate(resid = residuals(lm_f_initial))
males_clean   <- males_clean   %>% mutate(resid = residuals(lm_m_initial))

# Step 2: Identify and report outliers
outliers <- bind_rows(
  females_clean %>% filter(abs(resid) > 0.5),
  males_clean   %>% filter(abs(resid) > 0.5)
) %>%
  select(sample_id, sex, SL_mm, total_weight, Month, Location, resid) %>%
  arrange(desc(abs(resid)))

cat("\n=== OUTLIERS REMOVED ===\n")
print(outliers)
cat("Total outliers removed:", nrow(outliers), "\n")

# Step 3: Remove outliers
females_no_outliers <- females_clean %>% filter(abs(resid) <= 0.5)
males_no_outliers   <- males_clean   %>% filter(abs(resid) <= 0.5)

cat("Females after outlier removal:", nrow(females_no_outliers), "\n")
cat("Males after outlier removal:",   nrow(males_no_outliers), "\n")

# Step 4: Refit models without outliers
lm_f <- lm(log(total_weight) ~ log(SL_mm), data = females_no_outliers)
lm_m <- lm(log(total_weight) ~ log(SL_mm), data = males_no_outliers)

summary(lm_f)
summary(lm_m)

cat("\nFemale: a =", round(exp(coef(lm_f)[1]), 6),
    "b =", round(coef(lm_f)[2], 3),
    "R² =", round(summary(lm_f)$r.squared, 3), "\n")
cat("Male:   a =", round(exp(coef(lm_m)[1]), 6),
    "b =", round(coef(lm_m)[2], 3),
    "R² =", round(summary(lm_m)$r.squared, 3), "\n")

# Step 5: Isometry test
t_f <- (coef(lm_f)[2] - 3) / summary(lm_f)$coefficients[2, 2]
p_f <- 2 * pt(abs(t_f), df = lm_f$df.residual, lower.tail = FALSE)
t_m <- (coef(lm_m)[2] - 3) / summary(lm_m)$coefficients[2, 2]
p_m <- 2 * pt(abs(t_m), df = lm_m$df.residual, lower.tail = FALSE)

cat("\nFemale isometry test: t =", round(t_f, 3), "p =", round(p_f, 4), "\n")
cat("Male isometry test:   t =", round(t_m, 3), "p =", round(p_m, 4), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. SIZE AT FIRST MATURITY
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== SIZE AT FIRST MATURITY ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  mutate(mature = ifelse(reproductive_status >= 3, "mature", "immature")) %>%
  group_by(sex, mature) %>%
  summarise(mean_SL = round(mean(SL_mm, na.rm = TRUE), 2),
            min_SL  = round(min(SL_mm, na.rm = TRUE), 2),
            max_SL  = round(max(SL_mm, na.rm = TRUE), 2),
            n       = n(), .groups = "drop")

# ══════════════════════════════════════════════════════════════════════════════
# 4. GSI BY SEX AND MONTH
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== GSI BY SEX AND MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(mean_GSI = round(mean(GSI, na.rm = TRUE), 3),
            sd_GSI   = round(sd(GSI, na.rm = TRUE), 3),
            min_GSI  = round(min(GSI, na.rm = TRUE), 3),
            max_GSI  = round(max(GSI, na.rm = TRUE), 3),
            var_GSI  = round(var(GSI, na.rm = TRUE), 3),
            n        = n(), .groups = "drop")

cat("\n=== GSI OVERALL VARIANCE BY SEX ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex) %>%
  summarise(variance = round(var(GSI, na.rm = TRUE), 3))

# ══════════════════════════════════════════════════════════════════════════════
# 5. HSI BY SEX AND MONTH
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== HSI BY SEX AND MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(mean_HSI = round(mean(HSI, na.rm = TRUE), 3),
            sd_HSI   = round(sd(HSI, na.rm = TRUE), 3),
            n        = n(), .groups = "drop")

# ══════════════════════════════════════════════════════════════════════════════
# 6. CONDITION FACTOR K BY SEX AND MONTH
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== CONDITION FACTOR K BY SEX AND MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(mean_K = round(mean(K, na.rm = TRUE), 4),
            sd_K   = round(sd(K, na.rm = TRUE), 4),
            n      = n(), .groups = "drop")

# ══════════════════════════════════════════════════════════════════════════════
# 7. REPRODUCTIVE STAGING
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== REPRODUCTIVE STAGING OVERALL ===\n")
table(fish$sex, fish$reproductive_status)

cat("\n=== REPRODUCTIVE STAGING BY SEX AND MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  count(sex, Month, reproductive_status) %>%
  group_by(sex, Month) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print(n = 50)

# ══════════════════════════════════════════════════════════════════════════════
# 8. COMBINED MODEL — ALL PREDICTORS TOGETHER
# ══════════════════════════════════════════════════════════════════════════════
p33 <- Chapter_4$p33
p7  <- Chapter_4$p7
Li  <- Chapter_4$Li
Gf  <- Chapter_4$mean_GSI_female
Gm  <- Chapter_4$mean_GSI_male

cat("\n=== COMBINED MODEL: FEMALE GSI ~ p33 + p7 + Li ===\n")
model_f_combined <- lmer(Gf ~ p33 + p7 + Li + (1|Location),
                         data = Chapter_4)
summary(model_f_combined)
vif(model_f_combined)

cat("\n=== COMBINED MODEL: MALE GSI ~ p33 + p7 + Li ===\n")
model_m_combined <- lmer(Gm ~ p33 + p7 + Li + (1|Location),
                         data = Chapter_4)
summary(model_m_combined)
vif(model_m_combined)

cat("\n=== CORRELATION BETWEEN RAINFALL VARIABLES ===\n")
cor(Chapter_4$p33, Chapter_4$p7, use = "complete.obs")

# ══════════════════════════════════════════════════════════════════════════════
# 9. FIGURES
# ══════════════════════════════════════════════════════════════════════════════

# Figure 1: Length-weight (log-log, outliers removed)
fig_LW <- bind_rows(
  females_no_outliers %>% mutate(sex = "F"),
  males_no_outliers   %>% mutate(sex = "M")
) %>%
  ggplot(aes(x = log(SL_mm), y = log(total_weight), colour = sex)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("F" = "#E07070", "M" = "#70B0C0"),
                      labels = c("Female", "Male")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "ln Standard length (mm)",
       y = "ln Total weight (g)",
       colour = "Sex")

fig_LW
ggsave("L_W_relationship.png", plot = fig_LW,
       width = 8, height = 5, dpi = 300)

# Figure 2: GSI seasonal pattern
fig_GSI <- fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(mean = mean(GSI, na.rm = TRUE),
            sd   = sd(GSI, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Month, y = mean, colour = sex, group = sex)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  scale_colour_manual(values = c("F" = "#E07070", "M" = "#70B0C0"),
                      labels = c("Female", "Male")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Mean GSI (%)", colour = "Sex")

fig_GSI
ggsave("GSI_seasonal_pattern.png", plot = fig_GSI,
       width = 8, height = 5, dpi = 300)

# Figure 3: GSI and HSI together
fig_GSI_HSI <- fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(GSI = mean(GSI, na.rm = TRUE),
            HSI = mean(HSI, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = c(GSI, HSI),
               names_to = "Index", values_to = "Value") %>%
  ggplot(aes(x = Month, y = Value, colour = Index, group = Index)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~sex, labeller = labeller(sex = c("F" = "Female", "M" = "Male"))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Index (%)", colour = "Index")

fig_GSI_HSI
ggsave("GSI_HSI_tradeoff.png", plot = fig_GSI_HSI,
       width = 10, height = 5, dpi = 300)

# Figure 4: Reproductive staging — corrected stage names
fig_staging <- fish %>%
  filter(sex %in% c("F","M")) %>%
  mutate(reproductive_status = factor(reproductive_status,
                                      levels = 1:5,
                                      labels = c("Virgin",
                                                 "Early maturation",
                                                 "Ready to spawn",
                                                 "Post-spawning",
                                                 "Resting"))) %>%
  count(sex, Month, reproductive_status) %>%
  group_by(sex, Month) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = Month, y = pct, fill = reproductive_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~sex, labeller = labeller(sex = c("F" = "Female", "M" = "Male"))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Percentage (%)", fill = "Stage")

fig_staging
ggsave("reproductive_staging.png", plot = fig_staging,
       width = 10, height = 5, dpi = 300)

# Figure 5: Condition factor K
fig_K <- fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(mean_K = mean(K, na.rm = TRUE),
            sd_K   = sd(K, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Month, y = mean_K, colour = sex, group = sex)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_K - sd_K,
                    ymax = mean_K + sd_K), width = 0.2) +
  scale_colour_manual(values = c("F" = "#E07070", "M" = "#70B0C0"),
                      labels = c("Female", "Male")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Fulton's K", colour = "Sex")

fig_K
ggsave("condition_factor_K.png", plot = fig_K,
       width = 8, height = 5, dpi = 300)

# Figure 6: Combined model scatterplots
plot_data <- Chapter_4 %>%
  select(Location, Month, mean_GSI_female, mean_GSI_male,
         p33, p7, Li) %>%
  rename(Gf = mean_GSI_female,
         Gm = mean_GSI_male)

pt <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11),
        axis.title = element_blank(),
        axis.text = element_text(size = 10))

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

pD <- ggplot(plot_data, aes(x = p33, y = Gm)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "D") +
  pt +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13)) +
  ylab("Mean male GSI (%)") +
  xlab("3-day precipitation (mm)")

pE <- ggplot(plot_data, aes(x = p7, y = Gm)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "E") +
  pt +
  theme(axis.title.x = element_text(size = 13)) +
  xlab("7-day precipitation (mm)")

pF <- ggplot(plot_data, aes(x = Li, y = Gm)) +
  geom_point(colour = "grey40", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "F - p = 0.052") +
  pt +
  theme(axis.title.x = element_text(size = 13)) +
  xlab("Levins index")

combined <- wrap_plots(pA, pB, pC,
                       pD, pE, pF,
                       nrow = 2)

combined
ggsave("GSI_rainfall_diet_models.png", plot = combined,
       width = 12, height = 8, dpi = 300)





library(tidyverse)
library(ggplot2)

# ── Prepare site-level mean GSI data ──────────────────────────────────────────
site_GSI <- fish %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(sex, Month, Location) %>%
  summarise(mean_GSI = mean(GSI, na.rm = TRUE), .groups = "drop")

# ── Figure A: Line plot — mean GSI per site across months, coloured by site ───
fig_site_GSI_f <- site_GSI %>%
  filter(sex == "F") %>%
  ggplot(aes(x = Month, y = mean_GSI,
             colour = Location, group = Location)) +
  geom_point(size = 2.5) +
  geom_line() +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Month",
       y = "Mean female GSI (%)",
       colour = "Site")

fig_site_GSI_m <- site_GSI %>%
  filter(sex == "M") %>%
  ggplot(aes(x = Month, y = mean_GSI,
             colour = Location, group = Location)) +
  geom_point(size = 2.5) +
  geom_line() +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Month",
       y = "Mean male GSI (%)",
       colour = "Site")
fig_site_GSI_f
fig_site_GSI_m

ggsave("GSI_by_site_female.png", plot = fig_site_GSI_f,
       width = 10, height = 5, dpi = 300)
ggsave("GSI_by_site_male.png", plot = fig_site_GSI_m,
       width = 10, height = 5, dpi = 300)

# ── Figure B: Correlation matrix — mean female GSI across sites ───────────────
library(reshape2)

# Pivot to wide format — one column per site
GSI_wide_f <- site_GSI %>%
  filter(sex == "F") %>%
  select(Month, Location, mean_GSI) %>%
  pivot_wider(names_from = Location, values_from = mean_GSI)

GSI_wide_m <- site_GSI %>%
  filter(sex == "M") %>%
  select(Month, Location, mean_GSI) %>%
  pivot_wider(names_from = Location, values_from = mean_GSI)

# Check the wide format
print(GSI_wide_f)
print(GSI_wide_m)

# Fix correlation — use pairwise complete observations
cor_f <- cor(GSI_wide_f[,-1], use = "pairwise.complete.obs")
cor_m <- cor(GSI_wide_m[,-1], use = "pairwise.complete.obs")

cat("=== FEMALE GSI CORRELATION MATRIX ACROSS SITES ===\n")
print(round(cor_f, 2))

cat("\n=== MALE GSI CORRELATION MATRIX ACROSS SITES ===\n")
print(round(cor_m, 2))

# ── Heatmap of correlation matrix ─────────────────────────────────────────────
cor_f_melt <- melt(cor_f)
cor_m_melt <- melt(cor_m)

fig_cor_f <- ggplot(cor_f_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "r")

fig_cor_m <- ggplot(cor_m_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "r")

fig_cor_f
fig_cor_m

ggsave("GSI_site_correlation_female.png", plot = fig_cor_f,
       width = 8, height = 6, dpi = 300)
ggsave("GSI_site_correlation_male.png", plot = fig_cor_m,
       width = 8, height = 6, dpi = 300)