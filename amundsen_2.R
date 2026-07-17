# ============================================================
# Feeding Strategy Plot for Hemigrammus marginatus
# Amundsen et al. (1996) / Costello (1990) method
# FOi = Frequency of Occurrence; Pi = Prey-Specific Abundance
# ============================================================

library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------
diet <- read_csv("Pi_Foii.csv")

# Clean up any NA rows (e.g. blank rows at bottom of file)
diet <- diet %>% filter(!is.na(Local), !is.na(Prey))

# Rescale Pi to 0–1 to match FOi axis (Pi is in % so divide by 100)
diet <- diet %>% mutate(Pi_scaled = Pi / 100)

# ------------------------------------------------------------
# 2. Identify prey items to label
#    Label prey with FOi > 0.10 OR Pi_scaled > 0.10 (visible items)
# ------------------------------------------------------------
to_label <- diet %>%
  group_by(Prey) %>%
  summarise(FOi = mean(FOi), Pi = mean(Pi_scaled)) %>%
  filter(FOi > 0.10 | Pi > 0.10)

# ------------------------------------------------------------
# 3. Build the plot
# ------------------------------------------------------------
p <- ggplot(diet, aes(x = FOi, y = Pi_scaled)) +
  
  # --- diagonal reference line (1:1) ---
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey40", linewidth = 0.6) +
  
  # --- data points ---
  geom_point(colour = "#E69F00", alpha = 0.7, size = 2.2) +
  
  # --- strategy zone labels ---
  # Upper-left zone: Specialist prey (high Pi, low FOi)
  annotate("text", x = 0.05, y = 0.95,
           label = "Specialists", fontface = "bold.italic",
           size = 3.8, colour = "grey25", hjust = 0) +
  
  # Lower-right zone: Generalist prey (high FOi, low Pi)
  annotate("text", x = 0.75, y = 0.05,
           label = "Generalists", fontface = "bold.italic",
           size = 3.8, colour = "grey25", hjust = 0) +
  
  # Upper-right zone: Dominant prey (high FOi AND high Pi)
  annotate("text", x = 0.60, y = 0.95,
           label = "Dominant prey", fontface = "bold.italic",
           size = 3.8, colour = "grey25", hjust = 0) +
  
  # Lower-left zone: Rare/unimportant prey
  annotate("text", x = 0.00, y = 0.04,
           label = "Rare prey", fontface = "bold.italic",
           size = 3.8, colour = "grey25", hjust = 0) +
  
  # --- prey item labels (ggrepel to avoid overlap) ---
  geom_text_repel(
    data = to_label,
    aes(x = FOi, y = Pi, label = Prey),
    size = 2.4,
    box.padding = 0.3,
    point.padding = 0.2,
    segment.colour = "grey60",
    segment.size = 0.4,
    max.overlaps = 30,
    seed = 42,
    nudge_x = ifelse(to_label$Prey == "Ostracoda", 0.08, 0),
    nudge_y = ifelse(to_label$Prey == "Ostracoda", 0.02, 0)
  ) +
  
  # --- axes & theme ---
  scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01),
                     breaks = c(0, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01),
                     breaks = c(0, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00")) +
  
  labs(
    x = "Frequency of Occurrence (FOi)",
    y = "Prey-Specific Abundance (Pi)"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    axis.title  = element_text(size = 12),
    axis.text   = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(10, 15, 10, 10)
  )
p
# ------------------------------------------------------------
# 4. Save the figure
# ------------------------------------------------------------
ggsave("Fig2_3_feeding_strategy.png", plot = p,
       width = 7, height = 6, dpi = 300, bg = "white")

ggsave("Fig2_3_feeding_strategy.pdf", plot = p,
       width = 7, height = 6, bg = "white")

message("Done! Files saved: Fig2_3_feeding_strategy.png / .pdf")
