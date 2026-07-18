############INFLUENCE OF RAINFALL AND TERRESTRIAL BIOMASS ON PREY COMPOSITION#############
############ENIOLA OLU-AYORINDE###################
############2026-12-05##############################
#TO CALCULATE the variables I will be using for my thesis (Biomass ratio, resource richness) and the overall diet table


#to find ratio of aquatic to terrestial biomass
library(readr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
latest_biomass_dataaAA <- read_csv("raw_data/latest_biomass_dataa.csv")
View(latest_biomass_dataaAA)
latest_biomass_data_n <- latest_biomass_dataaAA %>% mutate(ratio = Aquatic_Biomass/Terrestrial_Biomass)
View(latest_biomass_data_n)
write.csv(latest_biomass_data_n, "biomass_with_ratio.csv" , row.names = FALSE)
getwd()

#for Resource RICHNESS
library(readxl)
HMR_IAI <-read_excel("generated_data/HMR_IAI.xlsx")
View(HMR_IAI)

non_zero_counts <- sapply(HMR_IAI[, c("CJ", "CA", "CJL", "CO", "IJ", "IA", "IJL", "IO", "NA", "NJ",
                                      "NJL", "NO","UJ", "UJL", "UO", "AA", "AJ", "AJL", 
                                      "GA", "GJL", "GO", "PA", "PJ", "PJL")], function(x) sum(x != 0))


View(non_zero_counts)
write.csv(non_zero_counts, "resource_richness.csv")
#non_zero_counts = resource richness, it counts the numbers of columns that have numbers more than zero for each sampling unit, so, out of the 68 food items, how many were consumed in CJ, CO, etc, for biomass, it adds them)



#Overall diet table - TO CALCULATE THE IAI, FO, V% tables (grand total, combining all the values from all 24 samples (site-month) for all food items)
#IAI
library(dplyr)
library(readxl)
HMR_IAI <- read_excel("generated_data/HMR_IAI.xlsx")
View(HMR_IAI)
HMR_IAI_df <- as.data.frame(HMR_IAI)
# 2. Calculate the General Behavior
# We take the mean of all columns (except the 'Prey' column)
general_iai <- HMR_IAI_df %>%
  mutate(Mean_IAI = rowMeans(select(., -Prey), na.rm = TRUE)) %>%
  # Standardize the result so it sums to 100%
  mutate(IAi = (Mean_IAI / sum(Mean_IAI)) * 100) %>%
  # Select the columns we want and sort by importance
  select(Prey, IAi) %>%
  arrange(desc(IAi))

# 3. View and Save
View(general_iai)
write.csv(general_iai, "General_IAI_Behavior.csv", row.names = FALSE)

#FOI
library(readxl)
foi_raw <- read_excel("C:/Desktop/MY THESIS/FOI.xlsx")
foi_df <- as.data.frame(foi_raw)

# 2. Calculate General Behavior for FOi
# Since your FOI table has Samples in rows, we take the mean of each COLUMN
general_foi <- foi_df %>%
  summarise(across(-Sample, ~ mean(. , na.rm = TRUE))) %>%
  # Flip the table so Prey are in rows (Long format)
  pivot_longer(cols = everything(), names_to = "Prey", values_to = "FOi") %>%
  # Sort from most frequent to least frequent
  arrange(desc(FOi))

# 3. View and Save
View(general_foi)
write.csv(general_foi, "General_FOi_Behavior.csv", row.names = FALSE)


#FP (VO%)
library(readxl)
library(dplyr)
library(tidyr)

# 1. Load your FP data
fp_raw <- read_excel("C:/Desktop/MY THESIS/FP.xlsx")
fp_df <- as.data.frame(fp_raw)

general_fp <- fp_df %>%
  summarise(across(-Sample, ~ mean(. , na.rm = TRUE))) %>%
  # Flip the table so Prey are in rows (Long format)
  pivot_longer(cols = everything(), names_to = "Prey", values_to = "General_FP") %>%
  # Re-standardize to 100% (or 1.0) to ensure the total is consistent
  mutate(General_FP_Percent = (General_FP / sum(General_FP)) * 100) %>%
  # Sort from most preferred to least preferred
  arrange(desc(General_FP_Percent))

# 3. View and Save
View(general_fp)
write.csv(general_fp, "General_FP_Behavior.csv", row.names = FALSE)
