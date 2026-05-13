# Load the tidyverse library for data manipulation
library(tidyverse)

# 1. LOAD RAW DATA --------------------------------------------------------
# Main file with food items and basic specimen info
df_raw <- read_csv("Hemigrammus marginatus.xlsx - Food (1).csv")

# Supplemental dissection files
atl_source <- read_csv("Dissecção Hemigrammus marginatus com Alimentaçao EBI.xlsx - M. Atlântica (1).csv")
caa_source <- read_csv("Dissecção Hemigrammus marginatus com Alimentaçao EBI.xlsx - Caatinga (1).csv")

# 2. CREATE TABLE: LOCATIONS ----------------------------------------------
# Extract unique site and biome combinations
locations <- df_raw %>%
  select(Local, Biome) %>%
  mutate(Local = str_to_title(str_trim(Local)),
         Biome = str_trim(Biome)) %>%
  distinct() %>%
  drop_na(Local) %>%
  mutate(location_id = row_number()) %>%
  select(location_id, location_name = Local, biome = Biome)

# 3. CREATE TABLE: SAMPLING EVENTS ----------------------------------------
# Link dates and rainfall to the location_id
sampling_events <- df_raw %>%
  mutate(Local = str_to_title(str_trim(Local))) %>%
  left_join(locations, by = c("Local" = "location_name")) %>%
  select(location_id, Date, `AVG Rainfall`) %>%
  distinct() %>%
  mutate(event_id = row_number()) %>%
  select(event_id, location_id, Date, avg_rainfall = `AVG Rainfall`)

# 4. CREATE TABLE: TAXA ---------------------------------------------------
# Create a dictionary of all 65 food item columns
taxa_names <- colnames(df_raw)[9:ncol(df_raw)]
taxa <- data.frame(
  taxon_id = 1:length(taxa_names),
  taxon_name = taxa_names
)

# 5. PREPARE DISSECTION DATA (Source for Specimens) -----------------------
# Combine Atlantic and Caatinga logs and create the matchable ID
prepare_dissection <- function(df) {
  df %>%
    filter(!is.na(CIUFS) & !is.na(Nº)) %>%
    mutate(
      sample_id = paste0(CIUFS, "-", sprintf("%02d", as.integer(Nº))),
      # Map Portuguese source to English database columns
      total_lenght = as.numeric(CT),
      total_weight = as.numeric(PT),
      reproductive_status = as.numeric(Estádio),
      gonad_weight = as.numeric(PG),
      stomach_weight = as.numeric(PE),
      liver_weight = as.numeric(PF),
      fixed = Fixado
    )
}

combined_dissection <- bind_rows(
  prepare_dissection(atl_source),
  prepare_dissection(caa_source)
)

# 6. CREATE TABLE: SPECIMENS ----------------------------------------------
# Join the original specimen list with sampling events and dissection data
specimens <- df_raw %>%
  mutate(Local = str_to_title(str_trim(Local))) %>%
  left_join(locations, by = c("Local" = "location_name")) %>%
  left_join(sampling_events, by = c("location_id", "Date", "AVG Rainfall" = "avg_rainfall")) %>%
  select(sample_id = Number, event_id, standard_length_cm = `Standard lenght (cm)`, 
         sex = Sex, stomach_fullness = `Degree of stomach fullness`) %>%
  # Merge the biological data from the combined_dissection source
  left_join(combined_dissection %>% 
              select(sample_id, total_lenght, total_weight, reproductive_status, 
                     gonad_weight, stomach_weight, liver_weight, fixed), 
            by = "sample_id") %>%
  # Calculate Trophic/Biological Indices
  mutate(
    `gonad_weight/total_weight` = (gonad_weight / total_weight) * 100,
    `liver_weight/total_weight` = (liver_weight / total_weight) * 100
  )

# 7. CREATE TABLE: DIETARY RECORDS (THE LONG FORMAT) ----------------------
# Pivot the wide food columns into a single "value" column
dietary_records <- df_raw %>%
  select(sample_id = Number, all_of(taxa_names)) %>%
  pivot_longer(cols = -sample_id, names_to = "taxon_name", values_to = "value") %>%
  # Optimization: Remove zeros (empty diet entries)
  filter(value > 0) %>%
  left_join(taxa, by = "taxon_name") %>%
  mutate(record_id = row_number()) %>%
  select(record_id, sample_id, taxon_id, value)

# 8. EXPORT ALL 5 RELATIONAL FILES ----------------------------------------
write_csv(locations, "relational_locations.csv")
write_csv(sampling_events, "relational_events.csv")
write_csv(specimens, "relational_specimens.csv")
write_csv(taxa, "relational_taxa.csv")
write_csv(dietary_records, "relational_diet_records.csv")