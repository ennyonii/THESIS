############INFLUENCE OF RAINFALL AND TERRESTRIAL BIOMASS ON NICHE WIDTH, RESOURCE RICHNESS, BIOMASS RATIO#############
############ENIOLA OLU-AYORINDE###################
############2026-29-04##############################
#TO CALCULATE how EVI and rainfall(p7) affect our 3 dependent variables

#lmer with latest precipitation and EVI data
# Niche width model -------------------------------------------------------
library(lme4)
library(car)
library(lmerTest)
library(readr)

all_data <- read_csv("raw_data/Latest_Modelling_complete_list_PFF (1).csv")
View(all_data)
p <- all_data$precip_p1_MERGE
p1 <- all_data$precip_p3_MERGE
p2 <- all_data$precip_p7_MERGE
p3 <- all_data$precip_p15_MERGE
model_18 <- lmer(Levins_index ~ p + p3 + evi_mean + (1|site), data = all_data)
summary(model_18)

#Check for Multi-collinearity
vif(model_18)

#Model diagnostics
library("DHARMa")
res_3 <- simulateResiduals(fittedModel = model_18)
plot(res_3)
testDispersion(res_3)
testOutliers(res_3)

#plot
library(patchwork)
library(visreg)
library(ggplot2)

plot1 <- visreg(model_18, "evi_mean", gg = TRUE) +
  theme_bw() +
  labs(title = "A",
       x = "Enhanced Vegetation Index (EVI)",
       y = "Levin's Index (Niche Width)")

plot2 <- visreg(model_18, "p", gg = TRUE) +
  theme_bw() +
  labs(title = "B",
       x = "1-day Precipitation (mm)",
       y = "Levin's Index (Niche Width)")

plot3 <- visreg(model_18, "p3", gg = TRUE) +
  theme_bw() +
  labs(title = "B",
       x = "15-day Precipitation (mm)",
       y = "Levin's Index (Niche Width)")


# Resource richness model -------------------------------------------------
View(all_data)
model_19 <- lmer(Resource_richness ~ p + p3 + evi_mean + (1|site), data = all_data)
summary(model_19)

#Model diagnostics
library("DHARMa")
res_4 <- simulateResiduals(fittedModel = model_19)
plot(res_4)
testDispersion(res_4)
testOutliers(res_4)

# Create Plot A: EVI
plot4 <- visreg(model_19, "evi_mean", gg = TRUE) +
  theme_bw() +
  labs(title = "A",
       x = "Enhanced Vegetation Index (EVI)",
       y = "Resource_richness")

plot5 <- visreg(model_19, "p", gg = TRUE) +
  theme_bw() +
  labs(title = "B",
       x = "1-day Precipitation (mm)",
       y = "Resosurce_richness")

# Create Plot B: Rain
plot6 <- visreg(model_19, "p3", gg = TRUE) +
  theme_bw() +
  labs(title = "B",
       x = "15-day Precipitation (mm)",
       y = "Resource_richness")


# Biomass ratio model -----------------------------------------------------
data_no_inf <- all_data[-1, ]
View(data_no_inf)
data_no_inf$ratioo <- log(data_no_inf$ratio)
data_no_inf$p <- data_no_inf$precip_p7_MERGE
data_no_inf$p3 <- data_no_inf$precip_p15_MERGE
data_no_inf$evi_mean <- data_no_inf$evi_mean
model_20 <- lmer(ratioo ~ p + p3 + evi_mean + (1|site), data = data_no_inf)
summary(model_20)

#Model diagnostics
library("DHARMa")
res_5 <- simulateResiduals(fittedModel = model_20)
plot(res_5)
testDispersion(res_5)
testOutliers(res_5)

plot7 <- visreg(model_20, "evi_mean", gg = TRUE) +
  theme_bw() +
  labs(title = "A",
       x = "Enhanced Vegetation Index (EVI)",
       y = "Biomass_ratio")

plot8 <- visreg(model_20, "p", gg = TRUE) +
  theme_bw() +
  labs(title = "B",
       x = "1-day Precipitation (mm)",
       y = "Resosurce_richness")


plot9 <- visreg(model_20, "p3", gg = TRUE) +
  theme_bw() +
  labs(title = "B",
       x = "15-day Precipitation (mm)",
       y = "Biomass_ratio")



# Display them side by side
combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 + plot9
ggsave(
  filename = "Combined plot.png",  # file name
  plot = combined_plot,
  width = 10,
  height = 6,
  dpi = 300
)
getwd()
