library(readxl)
data_bp <- read_excel("C:/Desktop/MY THESIS/data_bp.xlsx")
View(data_bp)
Levins_index <- data_bp$Levins_index
Season <- data_bp$Season
Rainfall <- data_bp$Precipitation
Month <- data_bp$Month
biomass_densityy <- data_bp$biomass_density

#STAT ANALYSIS FOR THE NICHE WIDTH DEPENDENCE ON SEASON AND BIOME QUESTION
anova(lm(Levins_index~Biome)) #DEPENDENT VARIABLE *INDEPENDENT VARIABLE

anova(lm(Levins_index~Biome*Season))


#Boxplot FOR THE ANOVA(NB~SB)
library(ggplot2)
ggplot(data_bp, aes(x = Biome, y = Levins_index, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(
    y = "Levins_Index",
    x = "Biome",
    title = "Effect of Biome and Season on Niche Width"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Increase title size
  )
View(Rainfall)

anova(lm(Levins_index ~ Rainfall + biomass_densityy))




anova(lm(Levins_index ~ Rainfall*biomass_densityy))

model_1 <- lm(Levins_index ~ Rainfall*biomass_densityy)
summary(model_1)
library(ggplot2)

ggplot(data_bp, aes(x = biomass_densityy, y = Levins_index, color = Rainfall)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction of Rainfall and Biomass Density on Niche Width",
    x = "Biomass Density",
    y = "Levins Index",
    color = "Precipitation"
  ) +
  scale_color_gradient(low = "blue", high = "red") +  # More distinct color gradient
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8.7)
  )




anova(lm(Levins_index ~ biomass_densityy))
Model_2 <- lm(Levins_index ~ biomass_densityy)
summary(Model_2)
library(ggplot2)

ggplot(data_bp, aes(x = biomass_densityy, y = Levins_index)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Effect of Biomass Density on Niche Width",
    x = "Biomass Density",
    y = "Levins Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9.0)
  )

model_6 <- lm(Levins_index ~ Rainfall)
summary(model_6)





ggplot(data_bp, aes(x = Precipitation, y = Levins_index)) +
  geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~Biome) +
  labs(
    title = "Effect of Precipitation on Niche Breadth by Biome",
    x = "Precipitation (mm)",
    y = "Levins Index"
  ) +
  theme_minimal(base_size = 8)

#to find ratio of aquatic to terrestial biomass
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)
library(tidyverse)
latest_biomass_dataaAA <- latest_biomass_dataaAA %>% mutate(ratio = Aquatic_Biomass/Terrestrial_Biomass)
View(latest_biomass_dataaAA)

library(ggplot2)
Main_Source <- 
  ggplot(latest_biomass_dataa, aes(x = as.factor(Season), y = log(ratio), fill = Main_Source)) +
  geom_boxplot() +
  facet_wrap(~ BIOME) +
  labs(
    title = "Boxplot of Biomass Source Ratios by Season and Biome",
    x = "Season", y = "Biomass Ratio"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Aquatic" = "blue", "Terrestrial" = "pink", "Equal" = "gray"))


latest_biomass_dataaAA$Season
levels(latest_biomass_dataaAA$Season)
levels(as.factor(latest_biomass_dataaAA$Season))
levels(as.factor(latest_biomass_dataaAA$BIOME))
levels(as.factor(latest_biomass_dataaAA$Precipitation))

#geom_point, jitter, and maybe coloring the points with the `level of allochthnoy`

ggplot(latest_biomass_dataaAA, aes(x = as.factor(Season), y = log(ratio), fill = Main_source_by_season)) +
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~ BIOME) +
  labs(
    title = "Main source of biomass by season",
    x = "Season", y = "Log(Biomass Ratio)",
    fill = "Main_source_by_season"
  ) 
View(latest_biomass_dataa)

#NEW- There is no difference in the main_source in each biome, but according to season, there is a difference in the main source.

#TO CALCULATE 2 WAY ANOVA OF WHAT IS THE MAIN SOURCE OF BIOMASS IN THE BIOME AND SEASON
clean_data <- latest_biomass_dataaAA[-1, ] 
ratio <- clean_data$ratio
numeric_data <- ratio[sapply(ratio, is.numeric)]

anova_result <- aov(ratio ~ BIOME*Season, data = clean_data)
summary(anova_result)

anova_result <- aov(ratio ~ Season, data = clean_data)
summary(anova_result)

anova_result <- aov(ratio ~ BIOME, data = clean_data)
summary(anova_result)

ggplot(latest_biomass_dataa, aes(x = as.factor(Season), y = log(ratio), fill = Main_source_by_season)) +
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~ BIOME) +
  labs(
    title = "Main source of biomass by season",
    x = "Season", y = "Log(Biomass Ratio)",
    fill = "Main_source_by_season"
  ) 

#TO CALCULATE USING PRECIPITATION(continous variable)

anova_result_1 <- aov(ratio ~ BIOME * Precipitation, data = clean_data)
# Create a new dataframe, keeping only the finite values in the 'ratio' column
clean_dataa <- latest_biomass_dataaAA[is.finite(latest_biomass_dataaAA$ratio), ]

# You can verify the number of rows you removed
nrow(latest_biomass_dataaAA) - nrow(clean_data)
nrow(clean_dataa)
ratioo <- log(ratio)
model_3 <- lm(ratioo ~ log(biomass_density[-1]) * log(Precipitation[-1]), data = latest_biomass_dataaAAP)
summary(model_3)
anova_result_2 <- aov(ratio ~ BIOME + Precipitation, data = clean_data)
View(ratio)
data_bp_clean <- data_bp[-1, ]  # removes first row from data_bp

anova_result_1 <- aov(ratio ~ Precipitation, data = data_bp_clean)
summary(anova_result_1)
anova_result_2 <- aov(ratio ~ biomass_density, data = data_bp_clean)
summary(anova_result_2)
model_4 <- lm(ratioo ~ biomass_density[-1], data = latest_biomass_dataaAAP)
summary(model_4)
model_5 <- lm(ratioo ~ Precipitation[-1], data = latest_biomass_dataaAAP)
summary(model_5)
library(ggplot2)

ggplot(data_bp_clean, aes(x = biomass_density, y = ratioo)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Effect of Biomass Density on Ratio",
    x = "Biomass Density",
    y = "Biomass Ratio(Aquatic/Terrestial)"
  ) +
  theme_minimal()


ggplot(data_bp_clean, aes(x = Precipitation, y = ratioo)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Effect of precipitation on Ratio",
    x = "Precipitation",
    y = "Biomass Ratio(Aquatic/Terrestial)"
  ) +
  theme_minimal()

anova_result_3 <- aov(ratio ~ biomass_density * Precipitation, data = data_bp_clean)
summary(anova_result_3)

ggplot(data_bp_clean, aes(x = biomass_density, y = log(ratio), color = Precipitation)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Effeect of Biomass Density and Precipitation on Biomass Ratio",
    x = "Biomass Density",
    y = "Biomass Ratio(Aquatic/Terrestial)",
    color = "Precipitation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8.5)
  )




anova_result <- aov(ratio ~ Precipitation, data = clean_data)
anova_result <- aov(ratio ~ BIOME, data = clean_data)
summary(anova_result)

#for ratio*precipitation
library(ggplot2)

ggplot(clean_data, aes(x = Precipitation, y = ratio, color = BIOME)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  scale_color_manual(
    values = c("CAATINGA" = "#E69F00", "ATLANTIC" = "#56B4E9"),
    labels = c("Atlantic Forest", "Caatinga")
  ) +
  labs(
    title = "Interaction Between Biome 
  And Precipitation on Biomass Ratio",
    x = "Precipitation (mm)",
    y = "Biomass Ratio",
    color = "Biome"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )


#MAIN SOURCE BASED ON PRECIPITATION, LINEAR REGRESSION
#linear model
lm(log(ratio) ~ Precipitation, data = clean_data)

#plotting the analysis results
library(ggplot2)
ggplot(clean_data, aes(x = Precipitation, y = log(ratio))) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship Between Precipitation and Biomass Ratio",
    x = "Precipitation (mm)",
    y = "Log(Biomass Ratio)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9.5))





#OR
ggplot(clean_data, aes(x = Precipitation, log(ratio))) +
  geom_point(size = 3, color = "darkblue") +  # scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # regression line
  labs(
    title = "Effect of Rainfall on Aquatic:Terrestrial Biomass Ratio",
    x = "Rainfall (mm)",
    y = "Log(Aquatic / Terrestrial Biomass)"
  ) +
  theme_minimal(base_size = 14)


view(clean_data)

#does the source of biomass become more aquatic or terrestial as rainfall increases or reduces?
ggplot(clean_data, aes(x = Main_Source, y = Precipitation, fill = BIOME)) +
  geom_boxplot(alpha = 0.7, outlier.color = "black", outlier.size = 1.5) +
  labs(
    x = "Main Source of Biomass",
    y = "Precipitation (mm)",
    fill = "Biome"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("CAATINGA" = "orange", "ATLANTIC" = "blue"))

#Regression model
clean_data$Main_Source <- factor(clean_data$Main_Source)
View(clean_data)

# Fit the logistic regression model glm
model <- glm(Main_Source ~ Precipitation, data = clean_data, family = binomial)
summary(model)

#plotting the Analysis results
library(ggplot2)

# Create predicted probabilities
clean_data$predicted_prob <- predict(glm(Main_Source ~ Precipitation, 
                                         family = binomial, data = clean_data), 
                                     type = "response")

# Plot
ggplot(clean_data, aes(x = Precipitation, y = predicted_prob)) +
  geom_point(aes(color = Main_Source), size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "blue") +
  labs(
    title = "Effect of Precipitation on Biomass Source",
    x = "Precipitation (mm)",
    y = "Probability that Main Source is Aquatic"
  ) +
  theme_minimal()

#NEW LINEAR REGRESSION- didn't work
# For aquatic biomass
model_aquatic <- lm(Aquatic_Biomass ~ Precipitation, data = clean_data)
summary(model_aquatic)

# For terrestrial biomass
model_terrestrial <- lm(Terrestrial_Biomass ~ Precipitation, data = clean_data)
summary(model_terrestrial)
clean_data$Dominant_Source <- ifelse(df$terrestrial_biomass > df$aquatic_biomass, "terrestrial", "aquatic")
clean_data$Dominant_Source <- factor(clean_data$Dominant_Source)

# Logistic regression
model_dom <- glm(Dominant_Source ~ Rainfall_mm, data = df, family = "binomial")
summary(model_dom)


#MAIN SOURCE OF BIOMASS EXACTLY, AND GRAPH
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)
# Rename columns for easier use (remove the leading X)
names(latest_biomass_dataaAA)[1] <- "Sample_ID"

# --- 2. DATA PREPARATION: CATEGORIZE PRECIPITATION ---
# Updated breaks and labels to match the user's request (0-50, 50-100, 100-150, >150)
latest_biomass_dataaAA$Precipitation_Category <- cut(latest_biomass_dataaAA$Precipitation, 
                                                     breaks = c(-Inf, 50, 100, 150, Inf),
                                                     labels = c("Low (0-50 mm)", "Moderate (50-100 mm)", "High (100-150 mm)", "Very High (>150 mm)"),
                                                     right = TRUE,
                                                     include.lowest = TRUE)

library(tidyverse)
install.packages("reshape2")
library(reshape2)

# Remove the Month column that was duplicated in the original data and caused an error
latest_biomass_dataaAA <- select(latest_biomass_dataaAA, -Month.1)

# --- 3. CALCULATE TOTAL BIOMASS CONTRIBUTION (unchanged) ---

# Calculate the grand totals (similar to the Python script)
total_aquatic_biomass <- sum(latest_biomass_dataaAA$Aquatic_Biomass)
total_terrestrial_biomass <- sum(latest_biomass_dataaAA$Terrestrial_Biomass)
grand_total_biomass <- total_aquatic_biomass + total_terrestrial_biomass

# Calculate the percentage contribution
percent_aquatic <- (total_aquatic_biomass / grand_total_biomass) * 100
percent_terrestrial <- (total_terrestrial_biomass / grand_total_biomass) * 100

# Print the overall result
cat("--- Overall Biomass Contribution ---\n")
cat(sprintf("Total Aquatic (Autochthonous) Biomass: %.2f (%.2f%%)\n", total_aquatic_biomass, percent_aquatic))
cat(sprintf("Total Terrestrial (Allochthonous) Biomass: %.2f (%.2f%%)\n", total_terrestrial_biomass, percent_terrestrial))
cat("\nConclusion: The main source of biomass overall is Aquatic.\n")
cat("--------------------------------------\n\n")

# --- 4. PREPARE DATA FOR BOXPLOT (Long Format) ---

# We need to transform the data from 'wide' format (two columns for biomass) 
# into 'long' format (one column for the value, one column for the source type).

latest_biomass_dataaAA_long <- melt(latest_biomass_dataaAA, 
                                    id.vars = c("Sample_ID", "Location", "Month", "Season", "Precipitation"),
                                    measure.vars = c("Aquatic_Biomass", "Terrestrial_Biomass"),
                                    variable.name = "Biomass_Source",
                                    value.name = "Biomass_Value")

# Clean up the variable names in the new column
latest_biomass_dataaAA_long$Biomass_Source <- gsub("_Biomass", "", latest_biomass_dataaAA_long$Biomass_Source)

# --- 5. GENERATE BOXPLOT BY PRECIPITATION CATEGORY ---

cat("\n--- Boxplot by Precipitation Category ---\n")
# Plot to show the distribution split by the new Precipitation Category

boxplot_precip <- ggplot(latest_biomass_dataaAA_long, aes(x = Biomass_Source, y = Biomass_Value, fill = Biomass_Source)) +
  
  # Add the boxplot layer
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "red") +
  geom_jitter(width = 0.2, color = "black", alpha = 0.6) +
  
  # Facet by the new Precipitation Category
  facet_wrap(~ Precipitation, ncol = 4) +
  
  # Title and Labels
  labs(
    title = "Biomass Distribution by Source across Precipitation Categories",
    subtitle = "Visualizing the impact of increasing rainfall on diet source",
    x = "Biomass Source Type",
    y = "Biomass Value (Units)",
    fill = "Source"
  ) +
  
  # Set colors
  scale_fill_manual(values = c("Aquatic" = "#0072B2", "Terrestrial" = "#D55E00")) +
  
  # Set the theme
  theme_bw() +
  
  # Customize appearance
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Print the seasonal boxplot object
print(boxplot_precip)

library(ggplot2)
library(reshape2)
library(dplyr)

# --- Transform data to long format ---
latest_biomass_dataaAA_long <- melt(
  latest_biomass_dataaAA,
  id.vars = c("Sample_ID", "Location", "Month", "Season", "Precipitation"),
  measure.vars = c("Aquatic_Biomass", "Terrestrial_Biomass"),
  variable.name = "Biomass_Source",
  value.name = "Biomass_Value"
)

# Clean up the variable names
latest_biomass_dataaAA_long$Biomass_Source <- gsub("_Biomass", "", latest_biomass_dataaAA_long$Biomass_Source)

# --- Boxplot comparing sources across all 7 sites ---
boxplot_site <- ggplot(latest_biomass_dataaAA_long,
                       aes(x = Location, y = Biomass_Value, fill = Biomass_Source)) +
  
  geom_boxplot(alpha = 0.85, outlier.shape = 21, outlier.fill = "red",
               position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              color = "black", alpha = 0.4, size = 1.5) +
  
  labs(
    title = "Comparison of Aquatic vs Terrestrial Biomass Across 7 Freshwater Sites",
    subtitle = "Determining dominant biomass source by location",
    x = "Sampling Site",
    y = "Biomass Value (units)",
    fill = "Biomass Source"
  ) +
  
  scale_fill_manual(values = c("Aquatic" = "#0072B2", "Terrestrial" = "#D55E00")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(boxplot_site)
# Save the plot as a high-quality PNG file
ggsave(
  filename = "Biomass_Source_by_Site.png",  # file name
  plot = boxplot_site,                      # your ggplot object    # folder path (change to where you want it saved)
  width = 10,                               # width in inches
  height = 6,                               # height in inches
  dpi = 300                                 # resolution for print quality
)
getwd()
0
#TO CALCULATE PERMANOVA FOR  HOW SEASON AND BIOME AFFECT PREY COMPOSITION
library(readxl)
HMR_IAI <- read_excel("C:/Users/eniol/OneDrive/Desktop/MY THESIS/HMR_IAI.xlsx")
library(vegan)

HMR_IAI_df <- as.data.frame(HMR_IAI)
#TO Set the first column as row names and remove it from columns
rownames(HMR_IAI_df) <- HMR_IAI_df[[1]]
HMR_IAI_df <- HMR_IAI_df[, -1]  # Drop the 'Prey' column
View(HMR_IAI_df)

prey<-t(HMR_IAI_df)
write.csv(prey, "prey.csv")
View(prey)
#TO MEASURE HOW DISSIMILAR THE TWO BIOMES ARE BASED ON THE FOOD ITEMS (DIET COMPOSITION)
prey_bray<-vegan::vegdist(prey,method="bray")
nmds<- metaMDS(prey_bray)


prey_hellinger <- decostand(prey, method = "hellinger")
dist_matrix <- vegdist(prey_hellinger, method = "bray")
nmds_result <- metaMDS(prey_hellinger, distance = "bray", autotransform = FALSE)
prey_bray<-vegan::vegdist(prey,method="bray")
view(prey_bray)
nmds<- metaMDS(prey_bray)
View(nmds)
vec<-c("CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC",
       "ATLANTIC","ATLANTIC")
colors <- c("blue", "red")  # adjust for your groups
group_colors <- colors[vec]  # assigns color to each point
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)
Season <- latest_biomass_dataaAA$Season
Precipitation <- latest_biomass_dataaAA$Precipitation
View(latest_biomass_dataaAA)
#NMDS ORDINATION PLOT
plot(nmds$points, col = as.factor(vec), pch = 19)
text(nmds$points, labels = rownames(nmds$points), cex = 0.8, pos = 3)
length(nmds$points)


library(ggplot2)

ggplot(latest_biomass_dataaAA, aes(x = NMDS1, y = NMDS2)) +
  geom_point(
    aes(
      color = `Biomass density`,
      size  = Precipitation
    ),
    alpha = 0.9
  ) +
  scale_color_gradient(
    low = "blue",
    high = "red",
    name = "Biomass Density"
  ) +
  scale_size_continuous(
    name = "Precipitation",
    range = c(3, 12)
  ) +
  labs(
    title = "NMDS plot of prey composition",
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )


#PERMANOVA
adonis2(prey~vec)
adonis2(prey~season)
adonis2(prey~vec*Season)

View(biomass_densityy)
adonis2(prey~vec*Precipitation)
adonis2(prey~biomass_densityy*Precipitation)
adonis2(prey~biomass_densityy)

#analysis for two continous variables (better than using permanova which is for categorical)
db_rda_model <- capscale(prey_bray ~ biomass_density + Precipitation, data = latest_biomass_dataaAA)
summary(db_rda_model)
db_rda_model_2 <- capscale(prey_bray ~ biomass_density * Precipitation, data = latest_biomass_dataaAA)
summary(db_rda_model_2)
db_rda_model_3 <- capscale(prey_bray ~ biomass_density, data = latest_biomass_dataaAA)
summary(db_rda_model_3)
db_rda_model_4 <- capscale(prey_bray ~ Precipitation, data = latest_biomass_dataaAA)
summary(db_rda_model_4)
#TO CHECK THE P VALUE OF THE ABOVE ANALYSIS
print(anova(db_rda_model_4))


View(latest_biomass_dataaAA)
# Load necessary libraries
library(readxl)
latest_biomass_dataaAAP <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAAP)

library(vegan)

# 1. Run CAP analysis (using original column names)
cap_model <- capscale(prey_bray ~ biomass_density + Precipitation, data = latest_biomass_dataaAAP)

# 2. Extract site scores
site_scores <- scores(cap_model, display = "sites", choices = 1:2)

# 3. Prepare site labels from Location
site_labels <- latest_biomass_dataaAAP$Location

# 4. Prepare colors for each unique Location
locations <- as.factor(latest_biomass_dataaAAP$Location)
location_levels <- levels(locations)
location_colors <- rainbow(length(location_levels))[locations]

# 5. Make a copy of the environmental data with short names
env_short <- latest_biomass_dataaAAP[, c("biomass_density", "Precipitation")]
colnames(env_short) <- c("BD", "Prep")

# 6. Fit environmental vectors with short names
envfit_res <- envfit(cap_model, env_short)

# 7. Plot the ordination
plot(site_scores, type = "n", main = "CAP Ordination (labels: Location)",
     xlab = "CAP1", ylab = "CAP2")

# 8. Add points, colored by location
points(site_scores, pch = 21, bg = location_colors, cex = 1.3)


# 10. Add environmental vectors with short labels and small font
plot(envfit_res, col = "red", add = TRUE, cex = 0.6)

# 11. Add a legend for locations
legend("bottomright", legend = location_levels, pt.bg = rainbow(length(location_levels)),
       pch = 21, title = "Location", cex = 0.7)


# 1. Set a wider right margin and enable plotting outside plot region
par(mar = c(5, 4, 3, 7), xpd = TRUE)  # 8 gives enough space on the right

# 2. Plot your ordination as usual
plot(site_scores, type = "n", main = "CAP Ordination",
     xlab = "CAP1", ylab = "CAP2")
points(site_scores, pch = 21, bg = location_colors, cex = 1.3)
plot(envfit_res, col = "red", add = TRUE, cex = 0.6)

# 3. Add the legend outside the plot
legend("bottomright", inset = c(-0.25, 0),  # Adjust -0.25 as needed for your window
       legend = location_levels, pt.bg = rainbow(length(location_levels)),
       pch = 21, title = "Location", cex = 0.7)

ggsave("CAP.png", width = 12, height = 6, units = "in", dpi = 300)


nrow(prey)
length(vec)

#to calculate SIMPER -to determine which species contribute most to the observed dissimilarity between groups of samples 
species_data <- prey[, 1:64]
View(species_data)
vec<-c("CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA","CAATINGA",
       "ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC","ATLANTIC",
       "ATLANTIC","ATLANTIC")
simper_result <- simper(species_data, vec)
summary(simper_result)
# View all group comparisons
names(simper_result)
# Extract the result for the Forest vs Caatinga comparison
res <- simper_result[["CAATINGA_ATLANTIC"]]  # use the actual name shown by names(simper_result)

# Convert to data frame
res_df <- as.data.frame(res)
res_df$species <- rownames(res_df)
top_species <- res_df[order(-res_df$average), ][1:10, ]


#NMDS 1, NMDS2 AND SIMPER ON ONE PLOT TO SEE THE SPECIES(DIET COMPOSITION) CONTRIBUTING MAJORLY TO THE SIMILARITIES BETWEEN THE 2 BIOMES

library(vegan)
library(ggplot2)
library(dplyr)

# Your species abundance data (samples × species)
# Assume 'species_data' is your data, and 'group' is your factor vector

nmds <- metaMDS(species_data, distance = "bray", k = 2, trymax = 100)

# Group info: group should be a factor matching the rows of species_data
group <- factor(vec)  # e.g., c("Forest", "Forest", ..., "Caatinga", ...)
site_scores <- as.data.frame(scores(nmds, display = "sites"))
site_scores$group <- group
species_fit <- envfit(nmds, species_data, permutations = 999)
species_vectors <- as.data.frame(species_fit$vectors$arrows)
View(species_vectors)
# Keep only significant species if you want:
species_vectors$pvals <- species_fit$vectors$pvals
top_simper <- res_df[order(-res_df$average), ][1:10, ]
top_species <- rownames(top_simper)

# Subset species_vectors to only those species
top_species_vectors <- species_vectors %>%
  filter(rownames(species_vectors) %in% top_species) %>%
  mutate(species = rownames(.))
ggplot() +
  # NMDS points
  geom_point(data = site_scores, 
             aes(x = NMDS1, y = NMDS2, color = group), size = 3) +
  
  # Species arrows
  geom_segment(data = top_species_vectors, 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "darkgreen") +
  
  # Species labels
  geom_text(data = top_species_vectors, 
            aes(x = NMDS1, y = NMDS2, label = species), 
            hjust = 0.5, vjust = -0.8, color = "darkgreen", size = 3) +
  
  # Theme
  theme_minimal() +
  labs(title = "Top Species causing dissimilarity",
       x = "NMDS1", y = "NMDS2", color = "Biome") +
  theme(legend.position = "right")

install.packages("ggrepel")
library("ggrepel") # for smarter text placement

ggplot() +
  # NMDS points
  geom_point(data = site_scores, 
             aes(x = NMDS1, y = NMDS2, color = group), size = 3) +
  
  # Species arrows
  geom_segment(data = top_species_vectors, 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "darkgreen") +
  
  # Improved labels with ggrepel to avoid overlap
  ggrepel::geom_text_repel(data = top_species_vectors, 
                           aes(x = NMDS1, y = NMDS2, label = species),
                           color = "darkgreen", size = 3, 
                           box.padding = 0.3, point.padding = 0.2,
                           segment.color = "grey50", max.overlaps = Inf) +
  
  # Axis and title
  labs(
    title = "Top Species Causing Dissimilarity",
    x = "NMDS1", y = "NMDS2", color = "Biome"
  ) +
  
  # Theme adjustments
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

#for SPECIE RICHNESS ANALYSIS
library(readxl)
HMR_IAI <- read_excel("C:/Desktop/MY THESIS/HMR_IAI.xlsx")
View(HMR_IAI)

non_zero_counts <- sapply(HMR_IAI[, c("CJ", "CA", "CJL", "CO", "IJ", "IA", "IJL", "IO", "NA", "NJ",
                                      "NJL", "NO","UJ", "UJL", "UO", "AA", "AJ", "AJL", 
                                      "GA", "GJL", "GO", "PA", "PJ", "PJL")], function(x) sum(x != 0))


View(non_zero_counts)
#non_zero_counts = specie richness, it counts the numbers of columns that have numbers more than zero, for biomass, it adds them)
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)

anova_model <- aov(Specie_richness ~ BIOME * Season, data = latest_biomass_dataaAA)
summary(anova_model)

library(ggplot2)

ggplot(latest_biomass_dataa, aes(x = BIOME, y = Specie_richness, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(
    y = "Specie_richness",
    x = "Biome",
    title = "Effect of Biome and Season on Specie Richness"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, face = "bold")  # Increase title size
  )

anova_model_4 <- aov(Specie_richness ~ biomass_density * Precipitation, data = latest_biomass_dataaAA)
summary(anova_model_4)
Model_6 <- lm(Specie_richness ~ biomass_density * Precipitation, data = latest_biomass_dataaAA)
summary(Model_6)
library(ggplot2)

library(ggplot2)

ggplot(latest_biomass_dataaAA, aes(x = biomass_density, y = Resource_richness, color = Precipitation)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Effect of Biomass Density and Precipitation on Resource richness",
    x = "Biomass Density",
    y = "Resource richness",
    color = "Precipitation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8.7, face = "bold")  # Increase title size
  )

library(ggplot2)
library(readxl)
latest_biomass_dataaAA <- read_excel("C:/Desktop/MY THESIS/latest_biomass_dataaAA.xlsx")
View(latest_biomass_dataaAA)
ggplot(latest_biomass_dataaAA, aes(x = biomass_density, y = Specie_richness, color = Precipitation)) +
  geom_point(size = 1) +
  # Add geom_text() to label the points with location names
  geom_text(
    aes(label = Location_Month), # Use the 'Location' column for the text label
    vjust = -0.8,          # Adjust vertical position to place text slightly above the point
    hjust = 0.5,           # Center text horizontally
    size = 3               # Control the size of the text labels
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Effect of Biomass Density and Precipitation on 
    Specie Richness by Study sites_Month",
    x = "Biomass Density",
    y = "Specie richness",
    color = "Precipitation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold") # Slightly increase title size for clarity
  )




ggplot(latest_biomass_dataaAA, aes(x = Precipitation, y = Specie_richness)) +
  geom_point(size = 3, alpha = 0.7, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ BIOME) +
  labs(
    title = "Species Richness vs Precipitation in Each Biome",
    x = "Precipitation (mm)",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 10)

#BARCHART FOR RATIO VS PREP FOR ALL LOCATIONS
library(ggplot2)
mydata <- data.frame(
  ratiooo <- c("16.47058824","4.428571429","0.018583043"),
  rain <- c("7.05", "25.65","174.25")
)
print(mydata)
mydata$ratiooo <- as.numeric(mydata$ratiooo)
ggplot(mydata, aes(x=rain, y= log(ratiooo)))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Rainfall VS Resource dominance in Propriá",
    x = "Precipitation",
    y = "Biomass ratio",
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 9.5, face = "bold"), # Center the title
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1) # Angle the x-axis labels if they overlap
  )




#% IAI FOR ALL LOCATIONS IN RAINY AND DRY SEASON
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
HMR_IAI <- read_excel("C:/Desktop/MY THESIS/HMR_IAI.xlsx")
View(HMR_IAI)

first_col_name <- colnames(HMR_IAI)[1]
data_prepared <- HMR_IAI %>%
  rename(Food_Item = all_of(first_col_name))

location_codes <- colnames(data_prepared)[-1]

data_long <- data_prepared %>%
  pivot_longer(
    cols = all_of(location_codes), # Gather all columns except the 'Food_Item' column
    names_to = "Code",           # This new column will contain the headers 'CJ', 'CA', etc.
    values_to = "IAI"            # This new column will contain the IAI values
  ) %>%
  # Remove rows where IAI is not a number or is zero, as they don't contribute.
  filter(!is.na(IAI) & IAI > 0)
print("--- Data correctly reshaped into long format ---")
print(head(data_long))
View(data_long)

data_mapped <- data_long %>%
  mutate(
    True_Location = case_when(
      Code %in% c("CJ", "CA", "CJL", "CO") ~ "Curralinho",
      Code %in% c("IJ", "IA", "IJL", "IO") ~ "Ilha do Ouro",
      Code %in% c("NA", "NJ", "NJL", "NO") ~ "Niterói",
      Code %in% c("UJ", "UJL", "UO")       ~ "UHE Xingó",
      Code %in% c("AA", "AJL", "AJ")       ~ "Amparo",
      Code %in% c("GA", "GJL", "GO")       ~ "Gararu",
      Code %in% c("PA", "PJL", "PJ")       ~ "Própria",
      TRUE                                 ~ "Unknown"
    ),
    True_Season = case_when(
      Code %in% c("CJ", "CA", "IJ", "IA", "NA", "NJ", "UJ", "AA", "AJL", "GA", "GJL", "PA", "PJL") ~ "Rainy",
      Code %in% c("CJL", "CO", "IJL", "IO", "NJL", "NO", "UJL", "UO", "AJ", "GO", "PJ")          ~ "Dry",
      TRUE                                                                                      ~ "Unknown"
    )
  )
appendix_table <- data_mapped %>%
  filter(True_Location != "Unknown" & True_Season != "Unknown") %>%
  # Group by the new clean Location and Season columns.
  group_by(True_Location, True_Season) %>%
  # Calculate the total IAI for each group (e.g., total for Curralinho_Rainy).
  mutate(Total_IAI_in_Group = sum(IAI, na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate the percentage for each food item within its group.
  mutate(IAI_Percentage = round((IAI / Total_IAI_in_Group) * 100, 2)) %>%
  # Create the final column headers (e.g., "Curralinho_Rainy").
  mutate(Location_Season = paste(True_Location, True_Season, sep = "_")) %>%
  # Handle cases where multiple codes belong to the same group (e.g., CJ and CA).
  group_by(Food_Item, Location_Season) %>%
  summarise(IAI_Percentage = sum(IAI_Percentage), .groups = 'drop') %>%
  # Pivot back to the final wide format for the appendix.
  pivot_wider(
    names_from = Location_Season,
    values_from = IAI_Percentage,
    values_fill = 0 # Fill empty cells with 0
  ) %>%
  # Arrange alphabetically for a clean look.
  arrange(Food_Item)

print(appendix_table) # Use print(n=...) to show more rows if needed
View(appendix_table) 
write.csv(appendix_table, "IAI % IN 7 LOCATIONS.csv", row.names = FALSE) 
getwd()

#MIXED MODEL
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(dplyr)
library(readr)
library(dplyr)
library(readr)
library(purrr)

latest_biomass_dataaAA$ratio <- as.numeric(latest_biomass_dataaAA$ratio)
data_clean <- latest_biomass_dataaAA %>%
  filter(!is.infinite(ratio)) %>%
  filter(!is.na(ratio))

mixed_model <- lmer(ratio ~ Precipitation + (1 | Location_Month), data = data_clean)
summary(mixed_model)

#for each location
latest_biomass_dataaAA$ratio <- as.numeric(latest_biomass_dataaAA$ratio)
data_clean <- latest_biomass_dataaAA %>%
  filter(!is.infinite(ratio)) %>%
  filter(!is.na(ratio))

unique_locations <- unique(data_clean$Location_Month)
View(unique_locations)

location_models <- map(unique_locations, function(loc) {
  
  # Print which location we are currently analyzing
  print(paste("--- Analyzing Location:", loc, "---"))
  
  # Create a temporary subset of data for only the current location
  location_data <- filter(data_clean, Location_Month == loc)
  
  # Check if there are enough data points to run a model
  # You need at least 3 points for a simple linear regression.
  if (nrow(location_data) < 3) {
    print("Not enough data points to run a model for this location.")
    return(NULL) # Skip to the next location
  }
  
  # Fit a simple linear model (lm) for this location
  model <- lm(ratio ~ Precipitation, data = location_data)
  
  # Print the summary of the model
  print(summary(model))
  
  # Return the model object (optional)
  return(model)
})
mixed_model <- lmer(ratio ~ Precipitation + (1 | Location_Month), data = data_clean)
summary(mixed_model)




#AQUATIC/TERRESTIAL
library(dplyr)
library(tibble)

food_items <- c(
  "Acari", "Amphipoda", "Aranae", "Baetidae", "Caenidae", "Calamoceratidae", "Ceratopogonidae", 
  "Chaoboridae", "Chironomidae (L)", "Chironomidae(pupa)", "Cladocera", "Colembola", 
  "Coleoptera", "Coleoptera (larva)", "Copepoda", "Corixidae", "Diptera (adult)", 
  "Diptera (pupa)", "Dytiscidae", "Elmidae", "Empididae", "Ephemeroptera", "Filamentous Algae", 
  "Formicidae", "Gerrridae", "Helicopsychidae", "Hemiptera", "Hydracarina", "Hydropsychidae", 
  "Hymenoptera", "Insect Remains", "Invertebrate eggs", "Isoptera", "Lampyridae", "Leptoceridae", 
  "Leptohyphes", "Leptophlebiidae", "Libellulidae", "Naucoridae", "Noteridae", "Odonata (N)", 
  "Organic matter", "Orthoptera", "Ostracoda", "Philopotamidae", "Plant Matter", "Plecoptera", 
  "Pleidae", "Pseudoscorpionida", "Psocoptera", "Psychodidae", "Pyralidae", "Scales", 
  "Sediments", "Seeds and Fruits", "Shrimp (L)", "Simullidae", "Staphynilidae", "Thysanura", 
  "Tingidae", "Trichoptera", "Trichoptera (L)", "Vellidae", "Vespidae"
)

# Define aquatic items (everything else will be classified as terrestrial)
aquatic_items <- c(
  "Amphipoda", "Baetidae", "Caenidae", "Calamoceratidae", "Ceratopogonidae", "Chaoboridae", 
  "Chironomidae (L)", "Chironomidae(pupa)", "Cladocera", "Copepoda", "Corixidae", 
  "Dytiscidae", "Elmidae", "Empididae", "Ephemeroptera", "Filamentous Algae", 
  "Gerrridae", "Helicopsychidae", "Hemiptera", "Hydracarina", "Hydropsychidae", 
  "Leptoceridae", "Leptohyphes", "Leptophlebiidae", "Libellulidae", "Naucoridae", 
  "Noteridae", "Odonata (N)", "Ostracoda", "Philopotamidae", "Plecoptera", 
  "Pleidae", "Shrimp (L)", "Simullidae", "Trichoptera", "Trichoptera (L)", "Vellidae"
)

# Create dataframe and classify
food_df <- tibble(Food_Item = food_items) %>%
  mutate(Habitat = ifelse(Food_Item %in% aquatic_items, "Aquatic", "Terrestrial"))

# View the result
print(food_df)
write.csv (food_df, "food_df.csv")
library(readxl)
IAI <- read_excel("C:/Desktop/MY THESIS/IAI % IN 7 LOCATIONS.xlsx")
View(IAI)
IAI  <- IAI  %>%
  mutate(Habitat = ifelse(Food_Item %in% aquatic_items, "Aquatic", "Terrestrial")) %>%
  arrange(desc(Habitat)) 
write.csv(IAI, "IAI.csv")

print(IAI)
view(IAI)



#Amundsen graph
#Calculate Pi
# Load libraries
library(readxl)
library(dplyr)
library(tidyr)

# Load dataset
library(readxl)
H_M_R_new <- read_excel("C:/Desktop/MY THESIS/H_M R_new.xlsx")
View(H_M_R_new)

library(dplyr)

library(dplyr)
#TO GET PI

# Convert all prey columns to numeric
H_M_R_new[, first_prey_col:ncol(H_M_R_new)] <- lapply(
  H_M_R_new[, first_prey_col:ncol(H_M_R_new)],
  function(x) as.numeric(as.character(x))
)


# --- 1. Define prey, site, and month ---
first_prey_col <- 8  # adjust to your first prey column
all_prey <- names(H_M_R_new)[first_prey_col:ncol(H_M_R_new)]

site_col <- "Local"   # name of site column
month_col <- "Month"  # name of month column

# --- 2. Calculate Pi per prey per site per month ---
Pi_site_month <- H_M_R_new %>%
  group_by(!!sym(site_col), !!sym(month_col)) %>%
  group_modify(~{
    site_month_data <- .
    
    Pi_results <- lapply(all_prey, function(prey) {
      # Stomachs that contain this prey
      stomachs_with_prey <- site_month_data[site_month_data[[prey]] > 0, ]
      
      if (nrow(stomachs_with_prey) > 0) {
        sum_Si <- sum(stomachs_with_prey[[prey]], na.rm = TRUE)
        sum_St <- stomachs_with_prey %>%
          select(all_of(all_prey)) %>%
          rowSums(na.rm = TRUE) %>%
          sum()
        Pi <- (sum_Si / sum_St) * 100
      } else {
        Pi <- 0
      }
      
      data.frame(Prey = prey, Pi_percent = Pi)
    })
    
    bind_rows(Pi_results)
  }) %>%
  ungroup()

# --- 3. Expand to include all prey for every site-month ---
all_combinations <- expand.grid(
  Month = unique(H_M_R_new[[month_col]]),
  Local = unique(H_M_R_new[[site_col]]),
  Prey = all_prey
)

# Join with Pi results
Pi_complete <- all_combinations %>%
  left_join(Pi_site_month, by = c("Month", "Local", "Prey")) %>%
  mutate(Pi_percent = ifelse(is.na(Pi_percent), 0, Pi_percent)) %>%
  arrange(Month, Local, Prey)  # optional: sort nicely




# --- 4. View the final results ---
View(Pi_complete)


library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# 1. Load your files
library(readxl)
foi_wide <- read_excel("C:/Desktop/MY THESIS/FOI.xlsx")
View(foi_wide)
pi_long <- read_csv("Pi_precent_redone.csv")

# 2. Reshape FOI to Long format immediately
foi_long <- foi_wide %>%
  pivot_longer(cols = -Sample, names_to = "Prey", values_to = "Foi")

# 3. AUTOMATICALLY create the Mapping Key
# Instead of rep(), we pull the unique Sample IDs directly from your file
sample_map <- data.frame(Sample = unique(foi_wide$Sample))

# Now we manually assign the correct Local and Month to those specific IDs
# This ensures that even if a site only has 3 months, it maps correctly.
sample_map <- sample_map %>%
  mutate(
    Local = case_when(
      str_detect(Sample, "Curr") ~ "Curralinho",
      str_detect(Sample, "Gara") ~ "Gararu",
      str_detect(Sample, "Ampa") ~ "Amparo",
      str_detect(Sample, "Ilha") ~ "Ilha do Ouro",
      str_detect(Sample, "Nite") ~ "Niteroi",
      str_detect(Sample, "Prop") ~ "Propria",
      str_detect(Sample, "Xing") ~ "UHE Xingo",
      TRUE ~ NA_character_
    ),
    Month = case_when(
      str_detect(Sample, "Jan") ~ "Jan",
      str_detect(Sample, "Apr") ~ "Apr",
      str_detect(Sample, "Jul") ~ "Jul",
      str_detect(Sample, "Oct") ~ "Oct",
      TRUE ~ NA_character_
    )
  )

# 4. Merge using the dynamic map
merged_data <- foi_long %>%
  left_join(sample_map, by = "Sample") %>%
  left_join(pi_long, by = c("Local", "Month", "Prey")) %>%
  # Replace NA with 0 for prey that didn't appear in one of the files
  mutate(
    Pi_percent = replace_na(Pi_percent, 0),
    Foi = replace_na(Foi, 0)
  )

# 5. Filter out any rows where mapping failed (if any)
merged_data <- merged_data %>% filter(!is.na(Local))

write_csv(merged_data, "Pi_complete_final.csv")




# View Pi (%) by site and prey
View(Pi_complete)

write.csv(Pi_complete, "Pi_complete.csv")
getwd()

#standardize (Relative Proportions (e.g., instead of "5mg of ants," it becomes "25% ants").)
prey_rel <- decostand(prey, method = "total")
prey_bray <- vegdist(prey_rel, method = "bray")



library(readxl)
library(dplyr)
library(tidyr)

# 1. Load Data
H_M_R_new <- read_excel("C:/Desktop/MY THESIS/H_M R_new.xlsx")
View(H_M_R_new)

# 2. Define your columns FIRST
# Make sure "Local" and "Month" match your Excel column names exactly
site_col <- "Local"
month_col <- "Month"
first_prey_col <- 8 

# 3. Robust Numeric Conversion
# We define which columns are prey items
all_prey <- names(H_M_R_new)[first_prey_col:ncol(H_M_R_new)]

# Convert prey columns to numeric, replacing any "text" or blanks with NA, then 0
H_M_R_new <- H_M_R_new %>%
  mutate(across(all_of(all_prey), ~as.numeric(as.character(.)))) %>%
  mutate(across(all_of(all_prey), ~replace_na(., 0)))

# 4. Calculate Pi per prey per site per month
# Using a "Tidy" approach (pivot_longer) is much faster and avoids the 'sum' error
Pi_results <- H_M_R_new %>%
  # Convert from wide to long format
  pivot_longer(cols = all_of(all_prey), names_to = "Prey", values_to = "Value") %>%
  group_by(Local, Month, Prey) %>%
  summarise(
    # sum_Si: Sum of prey i in stomachs where it occurs
    sum_Si = sum(Value[Value > 0], na.rm = TRUE),
    # sum_St: Sum of ALL prey in those same stomachs
    # We find which Fish IDs (rows) had this prey
    .groups = "drop"
  )

# --- Advanced Pi Calculation (matches Amundsen formula exactly) ---
# Because Pi requires the sum of the WHOLE stomach for only specific individuals, 
# we use this specific logic:

Pi_complete <- H_M_R_new %>%
  pivot_longer(cols = all_of(all_prey), names_to = "Prey", values_to = "Volume") %>%
  # Calculate total stomach volume for every single fish
  group_by(Local, Month, g) %>% # Replace 'Fish_ID' with your ID column name
  mutate(Total_Stomach = sum(Volume, na.rm = TRUE)) %>%
  ungroup() %>%
  # Now calculate Pi: (Sum of Prey Vol) / (Sum of Total Vol of those specific fish)
  filter(Volume > 0) %>%
  group_by(Local, Month, Prey) %>%
  summarise(
    Pi_percent = (sum(Volume) / sum(Total_Stomach)) * 100,
    .groups = "drop"
  )

# 5. Expand to include zeros (for preys that weren't eaten at a site/month)
all_combinations <- expand.grid(
  Month = unique(H_M_R_new[[month_col]]),
  Local = unique(H_M_R_new[[site_col]]),
  Prey = all_prey
)

Pi_final <- all_combinations %>%
  left_join(Pi_complete, by = c("Month", "Local", "Prey")) %>%
  mutate(Pi_percent = replace_na(Pi_percent, 0))

# View results
View(Pi_final)


#TO PLOT GRAPH
# 1. Load Required Libraries
# If you don't have these, run: install.packages(c("tidyverse", "ggrepel"))
library(tidyverse)
library(ggrepel)

# 2. Read and Clean the Data
# The 'Pi_complete.csv' file has empty rows and columns that need cleaning.
library(readr)
Pi_complete <- read_csv("C:/Desktop/MY THESIS/Pi_complete.csv")
View(Pi_complete)
Pi_complete <- read_csv("Pi_complete.csv") %>%
  select(Month, Local, Prey, Pi, Foi) %>%
  # 2. Then, use drop_na() to remove rows missing Pi or Foi values
  drop_na(Pi, Foi)

# 3. Calculate Quadrant Medians
pi_median <- median(data$Pi, na.rm = TRUE)
foi_median <- median(data$Foi, na.rm = TRUE)

# 4. Categorize Prey Items and Filter Zeros
data_categorized <- data %>%
  mutate(
    Category = case_when(
      Pi >= pi_median & Foi >= foi_median ~ "Dominant Prey (High Pi, High Foi)",
      Pi < pi_median & Foi >= foi_median ~ "Secondary Prey (Low Pi, High Foi)",
      Pi >= pi_median & Foi < foi_median ~ "Accessory Prey (High Pi, Low Foi)",
      TRUE ~ "Rare Prey (Low Pi, Low Foi)"
    )
  ) %>%
  # Filter out rows where Pi and Foi are exactly 0, as they clutter the origin
  filter(Pi > 0 | Foi > 0)


# 5. Generate the Amundsen Plot (using ggplot2)
amundsen_plot <- ggplot(data_categorized, aes(x = Foi, y = Pi)) +
  
  # A. Add a reference rectangle for the background of the quadrants
  annotate("rect", xmin = foi_median, xmax = Inf, ymin = pi_median, ymax = Inf, fill = "green", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = foi_median, ymin = pi_median, ymax = Inf, fill = "orange", alpha = 0.1) +
  annotate("rect", xmin = foi_median, xmax = Inf, ymin = -Inf, ymax = pi_median, fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = foi_median, ymin = -Inf, ymax = pi_median, fill = "gray", alpha = 0.1) +
  
  # B. Add the median lines (Quadrant dividers)
  geom_hline(yintercept = pi_median, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = foi_median, linetype = "dashed", color = "red", linewidth = 0.8) +
  
  # C. Add the scatter points, colored by their category
  geom_point(aes(color = Category), size = 3) +
  
  # D. Label the points using ggrepel to prevent overlap
  geom_text_repel(aes(label = Prey), size = 3, max.overlaps = 100) +
  
  # E. Set Labels and Theme
  labs(
    title = "Amundsen Graph (Prey Importance vs. Frequency of Occurrence)",
    subtitle = paste0("Quadrant Lines: Median Pi = ", round(pi_median, 2), ", Median Foi = ", round(foi_median, 2)),
    x = "Frequency of Occurrence Index (Foi)",
    y = "Prey Importance Index (Pi)",
    color = "Prey Category"
  ) +
  theme_bw() +
  # Remove legend for simplicity, as labels are on the plot
  theme(legend.position = "none")

# 6. Display the plot
print(amundsen_plot)


# --- Load libraries ---
library(tidyverse)
library(ggrepel)

# --- 1. Read your data ---
data <- read_csv("C:/Desktop/MY THESIS/Pi_complete.csv") %>%
  select(Month, Local, Prey, Pi, Foi) %>%
  drop_na(Pi, Foi)

# --- 2. Subset for one site (replace "Site1" with actual site name) ---
site_name <- "Propria"  
data_site <- data %>%
  filter(Local == site_name)

# --- 3. Calculate medians for that site ---
pi_median <- median(data_site$Pi, na.rm = TRUE)
foi_median <- median(data_site$Foi, na.rm = TRUE)

# --- 4. Categorize prey items ---
data_site <- data_site %>%
  mutate(
    Category = case_when(
      Pi >= pi_median & Foi >= foi_median ~ "Dominant Prey (High Pi, High Foi)",
      Pi < pi_median & Foi >= foi_median ~ "Secondary Prey (Low Pi, High Foi)",
      Pi >= pi_median & Foi < foi_median ~ "Accessory Prey (High Pi, Low Foi)",
      TRUE ~ "Rare Prey (Low Pi, Low Foi)"
    )
  ) %>%
  filter(Pi > 0 | Foi > 0)   # remove zeros cluttering the plot

# --- 5. Limit labels to dominant prey only ---
# You can define “dominant” as Pi or Foi > 5, or use both
data_labelled <- data_site %>%
  filter(Pi > 5 | Foi > 5)

# --- 6. Plot Amundsen graph for the site ---
amundsen_plot <- ggplot(data_site, aes(x = Foi, y = Pi)) +
  annotate("rect", xmin = foi_median, xmax = Inf, ymin = pi_median, ymax = Inf, fill = "green", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = foi_median, ymin = pi_median, ymax = Inf, fill = "orange", alpha = 0.1) +
  annotate("rect", xmin = foi_median, xmax = Inf, ymin = -Inf, ymax = pi_median, fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = foi_median, ymin = -Inf, ymax = pi_median, fill = "gray", alpha = 0.1) +
  geom_hline(yintercept = pi_median, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = foi_median, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(aes(color = Category), size = 3) +
  geom_text_repel(aes(label = Prey), data = data_labelled, size = 3, max.overlaps = 20) +
  labs(
    title = paste("Amundsen Graph for", site_name),
    subtitle = paste0("Median Pi = ", round(pi_median, 2), ", Median Foi = ", round(foi_median, 2)),
    x = "Frequency of Occurrence Index (Foi)",
    y = "Prey Importance Index (Pi)",
    color = "Prey Category"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# --- 7. Display the plot ---
print(amundsen_plot)


# --- Load Libraries ---
library(tidyverse)
library(ggrepel)

# --- 1. Read and Clean the Data ---
data <- read_csv("C:/Desktop/MY THESIS/Pi_complete.csv") %>%
  select(Month, Local, Prey, Pi, Foi) %>%
  drop_na(Pi, Foi)

# --- 2. Subset for One Site ---
site_name <- "Propria"   # 🟢 Replace with the name of your site (e.g., "River_A")
data_site <- data %>%
  filter(Local == site_name)

# --- 3. Calculate Median Values for Pi and Foi (for that site) ---
pi_median <- median(data_site$Pi, na.rm = TRUE)
foi_median <- median(data_site$Foi, na.rm = TRUE)

# --- 4. Identify Dominant Preys Only (High Pi & High Foi) ---
dominant_preys <- data_site %>%
  filter(Pi >= pi_median & Foi >= foi_median)

# --- 5. Plot the Amundsen Graph (Dominant Prey Only) ---
amundsen_plot_dominant <- ggplot(dominant_preys, aes(x = Foi, y = Pi)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text_repel(aes(label = Prey), size = 3.2, max.overlaps = 30) +
  geom_hline(yintercept = pi_median, linetype = "dashed", color = "red") +
  geom_vline(xintercept = foi_median, linetype = "dashed", color = "red") +
  labs(
    title = paste("Dominant Preys - Amundsen Graph for", site_name),
    subtitle = paste0("Median Pi = ", round(pi_median, 2), ", Median Foi = ", round(foi_median, 2)),
    x = "Frequency of Occurrence Index (Foi)",
    y = "Prey Importance Index (Pi)"
  ) +
  theme_bw()

# --- 6. Display the Plot ---
print(amundsen_plot_dominant)

# Optional: Save the plot
ggsave(paste0("Amundsen_Dominant_", site_name, ".png"), amundsen_plot_dominant, width = 8, height = 6)





# Load data
library(readr)
dataa <- read_csv("C:/Desktop/MY THESIS/Pi_complete.csv")
View(dataa)
# Choose one study site (e.g., "Propria")
site_name <- "Propria"
site_data <- subset(dataa, Local == site_name)

# --- Filter for dominant preys only ---
# Option 1: Use 75th percentile as threshold for both Pi and Foi
threshold_pi <- quantile(site_data$Pi, 0.95, na.rm = TRUE)
threshold_foi <- quantile(site_data$Foi, 0.95, na.rm = TRUE)

dominant_preys <- subset(site_data, Pi >= threshold_pi | Foi >= threshold_foi)

# --- Check results ---
print(dominant_preys)


# --- Optional: Amundsen Plot for this subset ---
library(ggplot2)
library(ggrepel)

ggplot(dominant_preys, aes(x = Foi, y = Pi, label = Prey)) +
  geom_point(color = "forestgreen") +
  geom_text_repel(size = 3, max.overlaps = 15) +
  labs(
    title = paste("Dominant Preys - Amundsen Graph for", site_name),
    x = "Frequency of Occurrence (FOi)",
    y = "Prey Importance (Pi)"
  ) +
  theme_bw()

library(ggplot2)
library(ggrepel)

ggplot(
  dominant_preys,
  aes(x = Foi, y = Pi, label = prey) # Assuming your column names are 'FOi', 'Pi', and 'Resource_Item'
) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text_repel(size = 3) +
  
  # --- Add the necessary lines for Amundsen Graph ---
  
  # 1. Vertical line at FOi = 0.50 (50%)
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "red", linewidth = 0.5) +
  
  # 2. Diagonal line at Pi = FOi (slope = 1, intercept = 0)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.5) +
  
  # --- Your existing code ---
  labs(
    title = paste("Dominant Preys - Amundsen Graph for Propria"),
    x = "Frequency of Occurrence (FOi)",
    y = "Prey Importance (Pi)"
  ) +
  theme_bw()







#THE ONE I USED, FOR EACH 7 LOCATIONS
site_name <- "UHE Xingo"
site_data <- subset(data, Local == site_name)

# --- Filter for dominant preys only ---
# Option 1: Use 95th percentile as threshold for both Pi and Foi
threshold_pi <- quantile(site_data$Pi, 0.95, na.rm = TRUE)
threshold_foi <- quantile(site_data$Foi, 0.95, na.rm = TRUE)

dominant_preys <- subset(site_data, Pi >= threshold_pi | Foi >= threshold_foi)

# --- Check results ---
print(dominant_preys)

# --- Amundsen Plot with Dividing Lines ---
library(ggplot2)
library(ggrepel)

ggplot(dominant_preys, aes(x = Foi, y = Pi, label = Prey)) +
  
  # 1. Plot the data points and labels
  geom_point(color = "forestgreen") +
  geom_text_repel(size = 3, max.overlaps = 30) +
  
  # 2. Add the Amundsen dividing lines
  # Vertical line at FOi = 0.50 (50%)
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "red", linewidth = 0.8) +
  
  # Diagonal line at Pi = FOi (slope = 1, intercept = 0)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) +
  
  # 3. Customize labels and theme
  labs(
    title = paste("Dominant Preys - Amundsen Graph for", site_name),
    x = "Frequency of Occurrence (FOi)",
    y = "Prey_specific Abundance (Pi)"
  ) +
  # Ensure both axes range from 0 to 1 for proper quadrant visualization
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + 
  theme_bw()

ggsave("Amundsen_UHE Xingo.png", width = 12, height = 6, units = "in", dpi = 300)




#FOR ALL 7, 99TH PERCENTILE
# Load necessary libraries
library(ggplot2)
library(ggrepel)

# --- 1. Determine Global Dominant Prey Thresholds (95th Quartile) ---
# Calculate the 95th percentile using the full 'data' (all sites)
threshold_pi <- quantile(dataa$Pi, 0.99, na.rm = TRUE)
threshold_foi <- quantile(dataa$Foi, 0.99, na.rm = TRUE)

# --- 2. Filter for Dominant Preys Across All Sites ---
# A prey is "dominant" if it meets the 95th percentile threshold for Pi OR Foi
# in *any* of the sampled sites/rows.
dominant_preys_all_sites <- subset(dataa, Pi >= threshold_pi | Foi >= threshold_foi)

# --- 3. Generate the Combined Amundsen Graph ---
ggplot(dominant_preys_all_sites, aes(x = Foi, y = Pi, label = Prey, color = Local)) +
  
  # Plot the data points, colored by site (Local)
  geom_point(size = 3) +
  # Use ggrepel for point labels, using the 'Prey' name
  geom_text_repel(aes(label = Prey), size = 3, max.overlaps = 15, show.legend = FALSE) +
  
  # Add the Amundsen dividing lines (these apply to the overall population/niche)
  # Vertical line at FOi = 0.50 (50%)
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "red", linewidth = 0.8) +
  # Diagonal line at Pi = FOi (slope = 1, intercept = 0)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) +
  
  # Customize labels and theme
  labs(
    title = "Amundsen Graph of Dominant Preys Across All 7 Sites (99th Percentile)",
    x = "Frequency of Occurrence (FOi)",
    y = "Prey Importance (Pi)",
    color = "Site Name" # Title for the color legend
  ) +
  # Ensure axes are scaled correctly for interpretation (0-1 for Foi, 0-100 for Pi)
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + 
  theme_bw() +
  # Move legend to a useful position for a presentation
  theme(legend.position = "right")

ggsave("Amundsen_Combined_Graph.png", width = 12, height = 6, units = "in", dpi = 300)

getwd()





#3D GRAPH FOR RESOURCE RICHNESS
## Plot ##
library(lme4)
library(ggplot2)
library(ggfortify)
library(data.table)
library(metafor)
library(gridExtra)
library(tidyverse)
library(visreg)
library(ggpubr)



# Your Model:
Model_6 <- lm(Resource_richness ~ biomass_density * Precipitation, data = latest_biomass_dataaAA)

library(visreg)
library(ggplot2)

g1 <- visreg2d(
  fit = Model_6,
  xvar = "biomass_density",
  yvar = "Precipitation",
  plot.type = "gg"
) + 
  geom_point(
    data = Model_6$model, # Access the data used in the model
    aes(x = biomass_density, y = Precipitation),
    colour = "black", alpha = 0.6, size = 2
  ) +
  theme_bw() + 
  ylab("Precipitation") + 
  xlab("Biomass Density") + 
  theme(axis.title = element_text(colour = "black", size = 12, face = "bold")) + 
  theme(axis.text = element_text(colour = "black", size = 12)) + 
  guides(fill = guide_legend(title = "Resource Richness")) + 
  theme(legend.title = element_text(size = 12, colour = "black", face = "bold")) + 
  theme(legend.text = element_text(size = 10, colour = "black")) + 
  ggtitle("Resource Richness across Biomass and Precipitation") + 
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  scale_fill_gradient(low = "white", high = "darkorange")

g1

ggsave("Resource richness 3d.png", width = 12, height = 6, units = "in", dpi = 300)

