library(lme4)
library(lmerTest)
library(car)

library(readxl)
Chapter_4 <- read_excel("raw_data/Variables_for_chapter_4.xlsx")
View(Variables_for_chapter_4)
p33 <- Chapter_4$precip_p3_MERGE
p7  <- Chapter_4$precip_p7_MERGE
Li  <- Chapter_4$Levins_index
Gf  <- Chapter_4$mean_GSI_female
Gm  <- Chapter_4$mean_GSI_male

# ── Step 1: Does rainfall predict diet? ───────────────────────────────────────
# (rainfall → Li)
model_diet_p33 <- lmer(Li ~ p33 + (1|Location), data = Chapter_4)
model_diet_p7  <- lmer(Li ~ p7  + (1|Location), data = Chapter_4)
model_diet_both <- lmer(Li ~ p33 + p7 + (1|Location), data = Chapter_4)

summary(model_diet_p33)
summary(model_diet_p7)
summary(model_diet_both)

AIC(model_diet_p33, model_diet_p7, model_diet_both)

# ── Step 2: Does diet predict GSI beyond rainfall? ────────────────────────────
# (rainfall + diet → GSI)
# Female
model_f_rain      <- lmer(Gf ~ p33 + p7 + (1|Location), data = Chapter_4)
model_f_diet      <- lmer(Gf ~ Li + (1|Location), data = Chapter_4)
model_f_mediation <- lmer(Gf ~ p33 + p7 + Li + (1|Location), data = Chapter_4)

summary(model_f_mediation)
AIC(model_f_rain, model_f_diet, model_f_mediation)

# Male
model_m_rain      <- lmer(Gm ~ p33 + p7 + (1|Location), data = Chapter_4)
model_m_diet      <- lmer(Gm ~ Li + (1|Location), data = Chapter_4)
model_m_mediation <- lmer(Gm ~ p33 + p7 + Li + (1|Location), data = Chapter_4)

summary(model_m_mediation)
AIC(model_m_rain, model_m_diet, model_m_mediation)

# ── Step 3: VIF check for mediation models ────────────────────────────────────
vif(model_f_mediation)
vif(model_m_mediation)

# ── Step 4: Informal mediation logic ──────────────────────────────────────────
# For mediation to hold ALL THREE must be true:
# 1. Rainfall significantly predicts Li (Step 1)
# 2. Li significantly predicts GSI (Step 2 diet-only model)
# 3. Li remains significant when rainfall is included (Step 2 mediation model)
# Print a clear summary of which links hold

cat("\n=== MEDIATION PATHWAY ASSESSMENT ===\n")
cat("Link 1 - Rainfall → Diet (Li):\n")
summary(model_diet_both)$coefficients

cat("\nLink 2 - Diet → Female GSI:\n")
summary(model_f_diet)$coefficients

cat("\nLink 2 - Diet → Male GSI:\n")
summary(model_m_diet)$coefficients

cat("\nLink 3 - Diet effect on GSI when rainfall included (Female):\n")
summary(model_f_mediation)$coefficients

cat("\nLink 3 - Diet effect on GSI when rainfall included (Male):\n")
summary(model_m_mediation)$coefficients