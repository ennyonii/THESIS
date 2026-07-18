library(dplyr)
library(tidyr)

# 1. Read data and instantly remove any column with a blank name
df <- read.csv("env_data_aligned.csv", check.names = FALSE) %>%
  select(-which(names(.) == ""))

# 2. Find median to separate low vs high EVI
median_evi <- median(df$EVI_mean)

# 3. Process population by population safely
population_evi_summary <- df %>%
  # Label each population based on its specific EVI value
  mutate(EVI_Group = ifelse(EVI_mean <= median_evi, "Low EVI", "High EVI")) %>%
  # Keep population info and diet items
  select(Location, Month, EVI_mean, EVI_Group, Acari:Vespidae) %>%
  # Reshape to easily extract top items per population row
  pivot_longer(cols = Acari:Vespidae, names_to = "Item", values_to = "Abundance") %>%
  group_by(Location, Month, EVI_mean, EVI_Group) %>%
  # Sort items inside each population from highest to lowest abundance
  arrange(desc(Abundance)) %>%
  # Extract the top 3 items for each unique population
  slice_head(n = 3) %>%
  # Format into a clean string text
  summarise(
    Top_Predominant_Items = paste0(Item, " (", round(Abundance * 100, 1), "%)", collapse = ", "),
    .groups = "drop"
  ) %>%
  # Order the list from lowest EVI to highest EVI
  arrange(EVI_mean)

# 4. Print the entire breakdown
print(population_evi_summary, n = Inf)
write.csv(population_evi_summary, "population_evi_summary.csv")
