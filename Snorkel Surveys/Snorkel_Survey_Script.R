# 1. Setup ---------------------------------------------------------------------

## Load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyr, 
               dplyr,
               cowsay,
               lubridate,
               tidyverse,
               openxlsx,
               readxl,
               corrplot,
               cowplot,
               changepoint,
               strucchange,
               ggpubr,
               stats,
               ggfortify,
               vegan,
               wesanderson,
               ggrepel,
               ggformula)

# Aesthetics color palette
pal <- wes_palette("AsteroidCity1", 2, type = "continuous")

## Set working directory for KK WORK
setwd("~/GitHub/PGST-Natural-Resources/Snorkel Surveys")

## Set working directory for KK HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Snorkel Surveys")

# 2. Reading in & formatting BlueView Time in Beam Files -----------------------

# Reading in file
snorkel <- read_csv("Portage_Canal_Snorkel_Survey.csv")

# Selecting columns of interest
snorkel <- snorkel %>% 
  select(Date,
         Habitat_Type,
         Kelp_Height_1_m,
         Kelp_Height_2_m,
         Kelp_Height_3_m,
         Kelp_at_Surface,
         Overall_Kelp_Health,
        Kelp_Blades,
        Pneumatocyst,
        Sorus,
        Senescence,
        Bryozoan_Epibiont,
        Filamentous_Epiphyte,
        Macroalgae_Epiphyte,
        Kelp_Crab_Epifauna,
        Chum,
        Coho,
        Pink,
        Cutthroat,
        Chinook,
        Unknown_Salmon,
        Herring,
        Sand_Lance,
        Surf_Smelt,
        Three_Spine_Stickleback,
        Anchovy,
        Unknown_Forage_Fish)

#Reformatting the dates
snorkel$Date <- as.Date(snorkel$Date, format = "%m/%d/%Y")

## 3. Removing Outliers --------------------------------------------------------

# remove na's
# snorkel <- na.omit(snorkel)

## 4. Selecting ----------------------------------------------------------------
high_density <- snorkel %>% 
  filter(Habitat_Type == "High Density Kelp")

low_density <- snorkel %>% 
  filter(Habitat_Type == "Low Density Kelp")

sandy <- snorkel %>% 
  filter(Habitat_Type == "Sandy Area")

rip_rap <- snorkel %>% 
  filter(Habitat_Type == "Rip Rap")

all_habitats <- snorkel %>% 
  filter(Habitat_Type %in% c("High Density Kelp", "Low Density Kelp", "Sandy Area", "Rip Rap"))
## 5. Plotting -----------------------------------------------------------------

# Facet panels for Habitat Type vs. Overall Kelp Health Metric
ggplot(all_habitats, aes(x = Date, y = Overall_Kelp_Health, color = Habitat_Type)) +
  geom_line() +  
  labs(x = "Date", y = "Overall Kelp Health", title = "Kelp Health Over 2024 Growing Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")

# Facet panel for Habitat Type vs. Bryozoa
ggplot(all_habitats, aes(x = Date, y = Bryozoan_Epibiont, color = Habitat_Type)) +
  geom_line() +  
  labs(x = "Date", y = "Overall Kelp Health", title = "Kelp Health Over 2024 Growing Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")

#Juvenile salmon
ggplot(all_habitats, aes(x = Date, y = Bryozoan_Epibiont, color = Habitat_Type)) +
  geom_line() +  
  labs(x = "Date", y = "Overall Kelp Health", title = "Kelp Health Over 2024 Growing Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")


## 6. Heat Map -----------------------------------------------------------------

# Load the necessary libraries
library(tidyverse)

# Assuming your dataframe is named snorkel
# Reshape the data to long format for juvenile salmon
juvenile_salmons_long <- snorkel %>%
  select(Date, Chum, Coho, Pink, Cutthroat, Chinook, Unknown_Salmon) %>%
  pivot_longer(cols = -Date, names_to = "Juvenile_Salmon", values_to = "Presence") %>%
  mutate(Presence = ifelse(Presence == "Yes", 1, 0))  # Convert Yes/No to 1/0

# Reshape the data to long format for forage fish
forage_fish_long <- snorkel %>%
  select(Date, Herring, Sand_Lance, Surf_Smelt, Three_Spine_Stickleback, Anchovy, Unknown_Forage_Fish) %>%
  pivot_longer(cols = -Date, names_to = "Forage_Fish", values_to = "Presence") %>%
  mutate(Presence = ifelse(Presence == "Yes", 1, 0))  # Convert Yes/No to 1/0

# Combine both datasets
combined_data <- bind_rows(
  juvenile_salmons_long %>% mutate(Type = "Juvenile Salmon"),
  forage_fish_long %>% mutate(Type = "Forage Fish")
)

# Plotting the heatmap
ggplot(combined_data, aes(x = Date, y = reorder(Juvenile_Salmon, Juvenile_Salmon), fill = Presence)) +
  geom_tile(color = "yellow") +  # Create the tiles for the heatmap
  scale_fill_gradient(low = "yellow", high = "blue", na.value = "grey50", name = "Presence") +
  facet_wrap(~ Type, scales = "free_y") +  # Create separate facets for Juvenile Salmon and Forage Fish
  labs(x = "Date", y = "Fish Type", title = "Presence of Fish Species Over Time") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
