#lmer with latest precipitation and EVI data
# Niche width model -------------------------------------------------------
library(lme4)  
library(car)
library(lmerTest)

library(readr)
all_data <- read_csv("raw_data/Modelling_complete_list_PFF.csv")
View(all_data)
p <- all_data$precip_p7_MERGE
model_18 <- lmer(Levins_index ~ p + evi_mean + (1|site), data = all_data)
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
# install.packages("patchwork")
library(patchwork)
library(visreg)
library(ggplot2)

# Create Plot A: EVI
p1 <- visreg(model_18, "evi_mean", gg = TRUE) + 
  theme_bw() + 
  labs(title = "A", 
       x = "Enhanced Vegetation Index (EVI)", 
       y = "Levin's Index (Niche Width)")

# Create Plot B: Rain
p2 <- visreg(model_18, "p", gg = TRUE) + 
  theme_bw() + 
  labs(title = "B", 
       x = "7-day Precipitation (mm)", 
       y = "Levin's Index (Niche Width)")

# Display them side by side
combined_plot_1 <- p1 + p2

ggsave(
  filename = "Niche width_lmer.png",  # file name
  plot = combined_plot_1,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)
getwd()
#ctrl + shift + R to create new sessions
# Resource richness model -------------------------------------------------
View(all_data)
model_19 <- lmer(Resource_richness ~ p + evi_mean + (1|site), data = all_data)
summary(model_19)
View(p)
#Model diagnostics
library("DHARMa")
res_4 <- simulateResiduals(fittedModel = model_19)
plot(res_4)
testDispersion(res_4)
testOutliers(res_4)

# Create Plot A: EVI
p3 <- visreg(model_19, "evi_mean", gg = TRUE) + 
  theme_bw() + 
  labs(title = "A", 
       x = "Enhanced Vegetation Index (EVI)", 
       y = "Resource_richness")

# Create Plot B: Rain
p4 <- visreg(model_19, "p", gg = TRUE) + 
  theme_bw() + 
  labs(title = "B", 
       x = "7-day Precipitation (mm)", 
       y = "Resource_richness")

# Display them side by side
combined_plot_2 <- p3 + p4

ggsave(
  filename = "Resource richness_lmer.png",  # file name
  plot = combined_plot_2,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)
# Biomass ratio model -----------------------------------------------------
View(all_data)
data_no_inf <- all_data[-1, ]
data_no_inf$ratioo <- log(data_no_inf$ratio)
model_20 <- lmer(ratioo ~ p[-1] + evi_mean + (1|site), data = data_no_inf)
summary(model_20)

#Model diagnostics
library("DHARMa")
res_5 <- simulateResiduals(fittedModel = model_20)
plot(res_5)
testDispersion(res_5)
testOutliers(res_5)

# Create Plot A: EVI
p5 <- visreg(model_20, "evi_mean", gg = TRUE) + 
  theme_bw() + 
  labs(title = "A", 
       x = "Enhanced Vegetation Index (EVI)", 
       y = "Biomass_ratio")

# Create Plot B: Rain
p6 <- visreg(model_20, "p", gg = TRUE) + 
  theme_bw() + 
  labs(title = "B", 
       x = "7-day Precipitation (mm)", 
       y = "Biomass_ratio")

# Display them side by side
combined_plot_3 <- p5 + p6

ggsave(
  filename = "Biomass ratio_lmer.png",  # file name
  plot = combined_plot_3,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)


plot(p3~evi_mean, data = all_data)




# 1. Prepare the data (removing the Inf/first row and logging the ratio)
data_no_inf <- all_data[-1, ]
data_no_inf$ratioo <- log(data_no_inf$ratio)

# 2. Re-assign 'p' explicitly to the new dataframe so it has exactly 23 rows
# This ensures 'p' and 'evi_mean' are exactly the same length
data_no_inf$p <- all_data$precip_p7_MERGE[-1] 

# 3. Run the model using the clean names inside the dataframe
model_20 <- lmer(ratioo ~ p + evi_mean + (1|site), data = data_no_inf)
summary(model_20)

# 4. Diagnostics
library("DHARMa")
res_5 <- simulateResiduals(fittedModel = model_20)
plot(res_5)

# 5. Create Plot A: EVI
p5 <- visreg(model_20, "evi_mean", gg = TRUE) + 
  theme_bw() + 
  labs(title = "A", 
       x = "Enhanced Vegetation Index (EVI)", 
       y = "Log Biomass Ratio (Aquatic/Terrestrial)")

# 6. Create Plot B: Rain
p6 <- visreg(model_20, "p", gg = TRUE) + 
  theme_bw() + 
  labs(title = "B", 
       x = "7-day Precipitation (mm)", 
       y = "Log Biomass Ratio (Aquatic/Terrestrial)")

# 7. Combine and Save
library(patchwork) # or gridExtra
combined_plot_3 <- p5 + p6

ggsave(
  filename = "Biomass_ratio_lmer_v2.png", 
  plot = combined_plot_3, 
  width = 10, 
  height = 6, 
  dpi = 300
)
