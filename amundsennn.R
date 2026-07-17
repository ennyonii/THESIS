library(tidyverse)
library(readxl)
library(lme4)
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
# 3.1 SAMPLE COMPOSITION
# ══════════════════════════════════════════════════════════════════════════════

cat("=== OVERALL SAMPLE SIZE ===\n")
fish %>% filter(sex %in% c("F","M")) %>% count(sex)

cat("\n=== SEX RATIO OVERALL ===\n")
sex_counts <- table(fish$sex[fish$sex %in% c("F","M")])
print(sex_counts)
chisq.test(sex_counts)

cat("\n=== SEX RATIO BY MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  count(Month, sex) %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  mutate(ratio_F_M = round(F/M, 2))

cat("\n=== SAMPLE SIZE BY LOCATION ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  count(Location, sex) %>%
  pivot_wider(names_from = sex, values_from = n)

cat("\n=== SAMPLE SIZE BY MONTH AND LOCATION ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  count(Month, Location)

# ══════════════════════════════════════════════════════════════════════════════
# 3.2 DESCRIPTIVE REPRODUCTIVE PATTERNS
# ══════════════════════════════════════════════════════════════════════════════

# ── Length-weight relationship ─────────────────────────────────────────────────
cat("\n=== LENGTH-WEIGHT RELATIONSHIP (log-log) ===\n")
lm_f <- lm(log(total_weight) ~ log(SL_mm), data = females)
lm_m <- lm(log(total_weight) ~ log(SL_mm), data = males)

summary(lm_f)
summary(lm_m)

cat("Female: a =", round(exp(coef(lm_f)[1]), 6),
    "b =", round(coef(lm_f)[2], 3),
    "R² =", round(summary(lm_f)$r.squared, 3), "\n")
cat("Male:   a =", round(exp(coef(lm_m)[1]), 6),
    "b =", round(coef(lm_m)[2], 3),
    "R² =", round(summary(lm_m)$r.squared, 3), "\n")

# Isometry test (is b significantly different from 3?)
t_f <- (coef(lm_f)[2] - 3) / summary(lm_f)$coefficients[2, 2]
p_f <- 2 * pt(abs(t_f), df = lm_f$df.residual, lower.tail = FALSE)
t_m <- (coef(lm_m)[2] - 3) / summary(lm_m)$coefficients[2, 2]
p_m <- 2 * pt(abs(t_m), df = lm_m$df.residual, lower.tail = FALSE)

cat("\nFemale isometry test: t =", round(t_f, 3), "p =", round(p_f, 4), "\n")
cat("Male isometry test:   t =", round(t_m, 3), "p =", round(p_m, 4), "\n")

# ── Size at first maturity ─────────────────────────────────────────────────────
cat("\n=== SIZE AT FIRST MATURITY ===\n")

females_mat <- females %>%
  filter(!is.na(SL_mm), !is.na(reproductive_status)) %>%
  mutate(mature = ifelse(reproductive_status >= 3, 1, 0))

males_mat <- males %>%
  filter(!is.na(SL_mm), !is.na(reproductive_status)) %>%
  mutate(mature = ifelse(reproductive_status >= 3, 1, 0))

# Proportion mature
cat("Female maturity proportion:\n")
females_mat %>% count(mature) %>% mutate(pct = round(n/sum(n)*100, 1)) %>% print()

cat("Male maturity proportion:\n")
males_mat %>% count(mature) %>% mutate(pct = round(n/sum(n)*100, 1)) %>% print()

# Minimum size at maturity
cat("\nMinimum SL of mature females:", min(females_mat$SL_mm[females_mat$mature==1]), "mm\n")
cat("Minimum SL of mature males:",   min(males_mat$SL_mm[males_mat$mature==1]),   "mm\n")

# Logistic regression for L50
glm_f <- glm(mature ~ SL_mm, data = females_mat, family = binomial(link = "logit"))
glm_m <- glm(mature ~ SL_mm, data = males_mat,   family = binomial(link = "logit"))

summary(glm_f)
summary(glm_m)

# ── GSI summary ───────────────────────────────────────────────────────────────
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

# ── HSI summary ───────────────────────────────────────────────────────────────
cat("\n=== HSI BY SEX AND MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(mean_HSI = round(mean(HSI, na.rm = TRUE), 3),
            sd_HSI   = round(sd(HSI, na.rm = TRUE), 3),
            n        = n(), .groups = "drop")

# ── Condition factor K ────────────────────────────────────────────────────────
cat("\n=== CONDITION FACTOR K BY SEX AND MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(mean_K = round(mean(K, na.rm = TRUE), 4),
            sd_K   = round(sd(K, na.rm = TRUE), 4),
            n      = n(), .groups = "drop")

# ── Reproductive staging ──────────────────────────────────────────────────────
cat("\n=== REPRODUCTIVE STAGING OVERALL ===\n")
table(fish$sex, fish$reproductive_status)

cat("\n=== REPRODUCTIVE STAGING BY SEX AND MONTH ===\n")
fish %>%
  filter(sex %in% c("F","M")) %>%
  count(sex, Month, reproductive_status) %>%
  group_by(sex, Month) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print(n = 50)

# ── Outlier check ──────────────────────────────────────────────────────────────
cat("\n=== OUTLIER CHECK ===\n")
females_clean <- females %>% filter(!is.na(total_weight), !is.na(SL_mm))
males_clean   <- males   %>% filter(!is.na(total_weight), !is.na(SL_mm))

lm_f_log <- lm(log(total_weight) ~ log(SL_mm), data = females_clean)
lm_m_log <- lm(log(total_weight) ~ log(SL_mm), data = males_clean)

females_clean <- females_clean %>% mutate(resid = residuals(lm_f_log))
males_clean   <- males_clean   %>% mutate(resid = residuals(lm_m_log))

bind_rows(
  females_clean %>% filter(abs(resid) > 0.5),
  males_clean   %>% filter(abs(resid) > 0.5)
) %>%
  select(sample_id, sex, SL_mm, total_weight, Month, Location, resid) %>%
  arrange(desc(abs(resid))) %>%
  print()

# ══════════════════════════════════════════════════════════════════════════════
# FIGURES
# ══════════════════════════════════════════════════════════════════════════════

# ── Figure 1: Length-weight (log-log) ─────────────────────────────────────────
fig_LW <- fish %>%
  filter(sex %in% c("F","M"), !is.na(total_weight), !is.na(SL_mm)) %>%
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
ggsave("L_W_relationship.png", plot = fig_LW, width = 8, height = 5, dpi = 300)

# ── Figure 2: GSI by sex and month ────────────────────────────────────────────
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
ggsave("GSI_seasonal_pattern.png", plot = fig_GSI, width = 8, height = 5, dpi = 300)

# ── Figure 3: GSI and HSI together ────────────────────────────────────────────
fig_GSI_HSI <- fish %>%
  filter(sex %in% c("F","M")) %>%
  group_by(sex, Month) %>%
  summarise(GSI = mean(GSI, na.rm = TRUE),
            HSI = mean(HSI, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = c(GSI, HSI), names_to = "Index", values_to = "Value") %>%
  ggplot(aes(x = Month, y = Value, colour = Index, group = Index)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~sex, labeller = labeller(sex = c("F" = "Female", "M" = "Male"))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Month", y = "Index (%)", colour = "Index")

fig_GSI_HSI
ggsave("GSI_HSI_tradeoff.png", plot = fig_GSI_HSI, width = 10, height = 5, dpi = 300)

# ── Figure 4: Reproductive staging stacked bar ────────────────────────────────
fig_staging <- fish %>%
  filter(sex %in% c("F","M")) %>%
  mutate(reproductive_status = factor(reproductive_status,
                                      levels = 1:5,
                                      labels = c("Immature",
                                                 "Developing",
                                                 "Spawning capable",
                                                 "Actively spawning",
                                                 "Regressing"))) %>%
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
ggsave("reproductive_staging.png", plot = fig_staging, width = 10, height = 5, dpi = 300)

# ── Figure 5: Condition factor K ──────────────────────────────────────────────
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
ggsave("condition_factor_K.png", plot = fig_K, width = 8, height = 5, dpi = 300)

# ── Figure 6: Maturity ogive for males only ───────────────────────────────────
length_seq_m <- seq(min(males_mat$SL_mm, na.rm = TRUE),
                    max(males_mat$SL_mm, na.rm = TRUE), length.out = 200)

pred_m <- predict(glm_m,
                  newdata = data.frame(SL_mm = length_seq_m),
                  type = "response")

ogive_data <- data.frame(length = length_seq_m, prob = pred_m)

fig_L50 <- ggplot() +
  geom_point(data = males_mat,
             aes(x = SL_mm, y = mature),
             colour = "#70B0C0", alpha = 0.3, size = 1.5) +
  geom_line(data = ogive_data,
            aes(x = length, y = prob),
            colour = "#70B0C0", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Standard length (mm)",
       y = "Probability of maturity")

fig_L50
ggsave("L50_males.png", plot = fig_L50, width = 8, height = 5, dpi = 300)