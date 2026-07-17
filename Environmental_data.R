
# ============================================================
# Environmental characteristics – multi-panel figure
# 3 variables (P15, P1, EVI) × 7 sites, faceted by site
# Sites grouped and labelled by biome
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1. Enter data
# ------------------------------------------------------------
env <- data.frame(
  Biome = c(
    rep("Caatinga", 16),
    rep("Atlantic Forest", 8)
  ),
  Site = c(
    rep("Curralinho", 4),
    rep("Ilha do ouro", 4),
    rep("Niterói", 4),
    rep("UHE Xingó", 4),
    rep("Amparo", 3),
    rep("Gararu", 3),
    rep("Propriá", 2)
  ),
  Month = c(
    "January","July","October","April",          # Curralinho (note: April missing, using order from table)
    "January","April","July","October",          # Ilha do ouro
    "January","April","July","October",          # Niterói
    "January","April","July","October",          # UHE Xingó
    "January","April","July",                    # Amparo
    "April","July","October",                    # Gararu
    "January","April"                            # Propriá (July missing from table)
  ),
  P15 = c(
    0.125, 14.375, 10.063, 17.563,
    0.063, 13.688, 10.375, 11.188,
    0.000, 20.313,  8.875,  9.375,
    0.000, 24.500,  9.750,  0.000,
    0.875, 22.313, 24.000,
    6.625, 22.563,  7.188,
    0.688, 17.250
  ),
  P1 = c(
    0.000,  1.063,  0.000,  0.313,
    0.000,  0.250,  2.688,  0.000,
    0.000,  0.000,  0.938,  0.000,
    0.000,  0.000,  0.000,  0.000,
    0.000,  0.000,  5.375,
    0.000,  8.938,  0.000,
    0.000,  0.000
  ),
  EVI = c(
    0.1227, 0.2740, 0.1260, 0.1260,
    0.1410, 0.1360, 0.2120, 0.1640,
    0.1430, 0.1570, 0.2670, 0.1530,
    0.1270, 0.3410, 0.1784, 0.1270,
    0.1950, 0.1940, 0.3040,
    0.2140, 0.3850, 0.1930,
    0.0830, 0.0900
  )
)

# Fix month order
env$Month <- factor(env$Month,
                    levels = c("January","April","July","October"))

# Site order: Caatinga first, then Atlantic Forest
env$Site <- factor(env$Site, levels = c(
  "Curralinho", "Ilha do ouro", "Niterói", "UHE Xingó",
  "Amparo", "Gararu", "Propriá"
))

env$Biome <- factor(env$Biome,
                    levels = c("Caatinga", "Atlantic Forest"))

# ------------------------------------------------------------
# 2. Reshape to long for faceting by variable
# ------------------------------------------------------------
long <- env %>%
  pivot_longer(cols = c(P15, P1, EVI),
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable = factor(Variable,
                           levels = c("P15", "P1", "EVI"),
                           labels = c(
                             "P15 (mm)",
                             "P1 (mm)",
                             "EVI"
                           )))

# ------------------------------------------------------------
# 3. Plot
# ------------------------------------------------------------
pppp <- ggplot(long, aes(x = Month, y = Value, group = 1)) +
  
  geom_line(colour = "grey50", linewidth = 0.5, na.rm = TRUE) +
  geom_point(aes(colour = Biome), size = 2.5, na.rm = TRUE) +
  
  facet_grid(Variable ~ Site, scales = "free_y", switch = "y") +
  
  scale_colour_manual(
    values = c("Caatinga" = "#E69F00",
               "Atlantic Forest" = "#0072B2"),
    name = "Biome"
  ) +
  
  scale_x_discrete(drop = FALSE) +
  
  labs(x = "Sampling period", y = NULL) +
  
  theme_classic(base_size = 12) +
  theme(
    strip.background   = element_blank(),
    strip.text.x       = element_text(face = "bold", size = 11),
    strip.text.y.left  = element_text(face = "bold", size = 11,
                                      angle = 90),
    strip.placement    = "outside",
    axis.text.x        = element_text(angle = 40, hjust = 1, size = 9),
    axis.text.y        = element_text(size = 7.5),
    axis.title.x       = element_text(size = 10),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 11),
    legend.text        = element_text(size = 9),
    panel.border       = element_rect(colour = "grey70", fill = NA,
                                      linewidth = 0.5),
    panel.spacing      = unit(0.4, "lines")
  )
pppp
# ------------------------------------------------------------
# 4. Save
# ------------------------------------------------------------
ggsave("Fig2_1_env_characteristics.png", plot = pppp,
       width = 13, height = 6, dpi = 300, bg = "white")

ggsave("Fig2_1_env_characteristics.pdf", plot = pppp,
       width = 13, height = 6, bg = "white")

