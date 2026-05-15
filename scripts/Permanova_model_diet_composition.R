############INFLUENCE OF RAINFALL AND TERRESTRIAL BIOMASS ON PREY COMPOSITION#############
############ENIOLA OLU-AYORINDE###################
############2026-29-04##############################
#TO CALCULATE how EVI and rainfall(p7) affect PREY COMPOSITION (bray curtis and NMDS) of Hemigrammus marginatus

#PERMANOVA
library(readxl)
library(readr)
library(dplyr)
library(vegan)
HMR_IAI <- read_excel("generated_data/HMR_IAI.xlsx")
View(HMR_IAI)
HMR_IAI_df <- as.data.frame(HMR_IAI)
#TO Set the first column as row names and remove it from columns
rownames(HMR_IAI_df) <- HMR_IAI_df[[1]]
HMR_IAI_df <- HMR_IAI_df[, -1]  # Drop the 'Prey' column
View(HMR_IAI_df)

prey<-t(HMR_IAI_df)
write.csv(prey, "prey.csv")
View(prey)
library(readr)
prey_with_location <- read_csv("raw_data/prey_with_location.csv")
View(prey_with_location)
#Hellinger transformation for IAI to standardize it
library(vegan)
prey_hellinger <- decostand(prey, method = "hellinger")
nmds <- metaMDS(prey_hellinger, distance = "bray", autotransform = FALSE)
dist_matrix <- vegdist(prey_hellinger, method = "bray")
View(nmds)

prey_data <- read_csv("raw_data/prey_with_location.csv")
env_data <- read_csv("raw_data/biomass_with_ratio.csv")
View(prey_data)
View(env_data)
#Join the data using BOTH Location and Month
env_data_aligned <- prey_data %>%
  left_join(env_data, by = c("Location", "Month"))

View(env_data_aligned)

en <- envfit(nmds, env_data_aligned[, c("Precip_p15_MERGE", "EVI_mean")], 
             permutations = 999, na.rm = TRUE)
print(en)

site_scores <- as.data.frame(scores(nmds, display = "sites"))

site_scores$Location <- env_data_aligned$Location

p15 <-  env_data_aligned$Precip_p15_MERGE


model_26 <- adonis2(prey_hellinger ~ EVI_mean + p15, data = env_data_aligned, method = "bray")
print(model_26)

library(ggplot2)

# 2. Extract scores for plotting
plot_df <- as.data.frame(scores(nmds, display = "sites"))
plot_df$EVI <- env_data_aligned$EVI_mean
plot_df$p15 <- env_data_aligned$Precip_p15_MERGE

New_NMDS <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = EVI, size = p15), alpha = 0.8) +
  scale_color_gradientn(
    colors = c("blue", "purple", "red"), 
    name = "Woody Biomass Density\n(EVI)"
  ) +
  scale_size_continuous(
    name = "Precipitation\n(mm)", 
    range = c(2, 10)
  ) +
  theme_bw() +
  labs(
    title = "NMDS plot of prey composition",
    subtitle = paste("Stress:", round(nmds$stress, 3))
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold")
  ) +
  coord_fixed()
New_NMDS
ggsave(
  filename = "New NMDS.png",  
  plot = New_NMDS,                      
  width = 10,                               
  height = 6,                               
  dpi = 300                                 
)
