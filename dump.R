library(ggplot2)

# 1. Run the PCoA
pcoa_res <- wcmdscale(dist_matrix, eig = TRUE)
var_exp <- round(100 * pcoa_res$eig / sum(pcoa_res$eig), 1)

# 2. Build the plotting dataframe 
# We use 'p' here so it matches your ggplot call below
plot_data <- data.frame(
  PCoA1 = pcoa_res$points[,1],
  PCoA2 = pcoa_res$points[,2],
  Location = env_data_aligned$Location,
  p = as.numeric(env_data_aligned$p) 
)

# 3. Generate the Plot
p <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2, color = Location, size = p)) +
  geom_point(alpha = 0.6) + 
  # This adds the 95% confidence circles for each of your 7 locations
  stat_ellipse(aes(group = Location), linetype = 2, linewidth = 0.5, show.legend = FALSE) +
  labs(
    x = paste0("PCoA1 (", var_exp[1], "%)"),
    y = paste0("PCoA2 (", var_exp[2], "%)"),
    title = "PCoA of Diet Composition",
    size = "Precipitation (mm)",
    color = "Location"
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") + 
  scale_size_continuous(range = c(2, 8)) +
  coord_fixed()

# 4. Display
print(p)
ggsave(
  "prey_composition.png",
  plot = p,       
  width = 10,     
  height = 7,      
  dpi = 300
)

# 1. Run the PCoA
pcoa_res <- wcmdscale(dist_matrix, eig = TRUE)
var_exp <- round(100 * pcoa_res$eig / sum(pcoa_res$eig), 1)

# 2. Build the plotting dataframe 
# We use 'p' here so it matches your ggplot call below
plot_data <- data.frame(
  PCoA1 = pcoa_res$points[,1],
  PCoA2 = pcoa_res$points[,2],
  evi_mean = env_data_aligned$evi_mean,
  p = as.numeric(env_data_aligned$p) 
)
evi_mean <-  env_data_aligned$evi_mean
View(evi_mean)
# 3. Generate the Plot
pp <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2, color = evi_mean, size = p)) +
  geom_point(alpha = 0.6) + 
  # This adds the 95% confidence circles for each of your 7 locations
  stat_ellipse(aes(group = evi_mean), linetype = 2, linewidth = 0.5, show.legend = FALSE) +
  labs(
    x = paste0("PCoA1 (", var_exp[1], "%)"),
    y = paste0("PCoA2 (", var_exp[2], "%)"),
    title = "PCoA of Diet Composition",
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") + 
  scale_size_continuous(range = c(2, 8)) +
  coord_fixed()

# 4. Display
print(pp)
ggsave(
  "prey_composition.png",
  plot = p,       
  width = 10,     
  height = 7,      
  dpi = 300
)



library(vegan)
library(ggplot2)
library(ggrepel)

# 1. Run the db-RDA (Constrained Ordination)
# This matches your adonis2 formula
db_rda_mod <- dbrda(prey_hellinger ~ Location + p, 
                    data = env_data_aligned, 
                    dist = "bray")

# 2. Extract scores for plotting
site_scores <- as.data.frame(scores(db_rda_mod, display = "sites"))
# We specifically want the EVI and Rain influence
env_scores <- as.data.frame(scores(db_rda_mod, display = "bp")) 

# Add metadata back for coloring
site_scores$EVI <- env_data_aligned$evi_mean
site_scores$Location <- env_data_aligned$Location

# 3. Calculate Variance Explained
perc <- round(100 * summary(db_rda_mod)$cont$importance[2, 1:2], 1)

# 4. Create the Plot
ggplot() +
  # Points colored by EVI to show the environmental gradient
  geom_point(data = site_scores, aes(x = dbRDA1, y = dbRDA2, color = EVI), size = 4, alpha = 0.8) +
  # Add the arrows (the "Drivers")
  geom_segment(data = env_scores, aes(x = 0, y = 0, xend = dbRDA1, yend = dbRDA2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 1) +
  geom_text_repel(data = env_scores, aes(x = dbRDA1, y = dbRDA2, label = rownames(env_scores)), 
                  fontface = "bold") +
  scale_color_viridis_c(option = "mako") + 
  theme_bw() +
  labs(x = paste0("dbRDA1 (", perc[1], "%)"), 
       y = paste0("dbRDA2 (", perc[2], "%)"),
       title = "db-RDA: Diet Constrained by Environment",
       color = "Terrestrial Biomass (EVI)")

Location <- env_data_aligned$Location

ggplot(plot_data, aes(x = PCoA1, y = PCoA2, color = evi_mean, size = p)) +
  geom_point(alpha = 0.7) + 
  # Circles show the Location grouping from your PERMANOVA
  stat_ellipse(aes(group = Location), linetype = 2, color = "grey40", linewidth = 0.5) +
  # Continuous color scale for EVI (The "Forest Pantry")
  scale_color_viridis_c(option = "mako", name = "EVI (Forest)") +
  scale_size_continuous(name = "Rain (mm)", range = c(2, 8)) +
  labs(
    title = "Dietary Variation Across Environmental Gradients",
    subtitle = "Points colored by forest productivity; Ellipses represent Study Locations"
  ) +
  theme_bw() +
  coord_fixed()


library(vegan)
library(ggplot2)
library(viridis)

# 1. Run the NMDS (using Bray-Curtis to match your adonis2 call)
nmds_obj <- metaMDS(prey_hellinger, distance = "bray", k = 2, trymax = 100)

# 2. Extract the coordinates for plotting
plot_df <- as.data.frame(scores(nmds_obj, display = "sites"))
plot_df$Location <- env_data_aligned$Location
plot_df$EVI <- env_data_aligned$evi_mean
plot_df$p <- env_data_aligned$p

# 3. Calculate Centroids (the average point for each Location)
centroids <- aggregate(cbind(NMDS1, NMDS2) ~ Location, data = plot_df, FUN = mean)

# 4. Generate the Arrow-Free Plot
ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  # Add 95% confidence ellipses for Locations (represents the Model grouping)
  stat_ellipse(aes(group = Location), linetype = 2, color = "grey70", linewidth = 0.5) +
  # Add points: Color = EVI (Forest), Size = p (Rain)
  geom_point(aes(color = EVI, size = p), alpha = 0.7) +
  # Add labels for the Centroids so it's clear where each Site sits
  geom_text(data = centroids, aes(x = NMDS1, y = NMDS2, label = Location), 
            fontface = "bold", size = 4, check_overlap = TRUE) +
  # Use a continuous scale for EVI (Environmental Gradient)
  scale_color_viridis_c(option = "mako", name = "EVI (Forest)") +
  scale_size_continuous(name = "Rainfall (p)", range = c(2, 8)) +
  theme_bw() +
  labs(
    title = "NMDS of Dietary Composition",
    subtitle = paste("Stress =", round(nmds_obj$stress, 3)),
    x = "NMDS1",
    y = "NMDS2"
  ) +
  coord_fixed()