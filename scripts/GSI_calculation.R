############INFLUENCE OF RAINFALL & NICHE WIDTH, ON REPRODUUCTIVE INVESTMENT (GSI)#############
############ENIOLA OLU-AYORINDE###################
############2026-10-06##############################
#All FSI related calculations

library(readr)
library(dplyr)
library (tidyverse)
Fish_info <- read_csv("data_paper_csvs/H.marginatus_food_items - fish info (3).csv")
View(Fish_info)
Fish_info <- Fish_info %>%
  mutate(GSI = (gonad_weight * 100) / total_weight)
View(Fish_info)

sum(is.na(Fish_info$GSI))
sum(Fish_info$total_weight == 0, na.rm = TRUE)


summary(fish_info$GSI)

mean_GSI_by_event <- Fish_info %>%
  group_by(event_id, sex) %>%
  summarise(
    n = n(),
    mean_GSI = mean(GSI, na.rm = TRUE),
    sd_GSI = sd(GSI, na.rm = TRUE)
  )%>%
  arrange(as.numeric(gsub("E", "", event_id)))
View(mean_GSI_by_event)
write.csv(mean_GSI_by_event, "mean_GSI_by_event.csv")

#Separate male from female
mean_GSI_females <- mean_GSI_by_event %>%
  filter(sex == "F")

mean_GSI_males <- mean_GSI_by_event %>%
  filter(sex == "M")
View(mean_GSI_females)


mean_GSI_combined <- merge(mean_GSI_females, mean_GSI_males, 
                           by = "event_id", 
                           suffixes = c("_female", "_male"))%>%
  arrange(as.numeric(gsub("E", "", event_id)))

View(mean_GSI_combined)

write.csv(mean_GSI_combined, "mean_GSI_combined.csv")

library(ggplot2)

# Boxplot of GSI by sex
ggplot(Fish_info, aes(x = sex, y = GSI, fill = sex)) +
  geom_boxplot() +
  labs(title = "GSI by Sex",
       x = "Sex",
       y = "Gonadosomatic Index (GSI)") +
  theme_classic()


