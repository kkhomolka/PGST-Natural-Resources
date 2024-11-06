# 1. Setup ---------------------------------------------------------------------

## Load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyr, 
               dplyr,
               cowsay,
               lubridate,
               tidyverse,
               changepoint,
               strucchange,
               ggpubr,
               stats,
               ggfortify,
               vegan,
               wesanderson,
               ggrepel,
               ggformula,
               reshape2)

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

## HABITATS
high_density <- snorkel %>% 
  filter(Habitat_Type == "High Density Kelp")

low_density <- snorkel %>% 
  filter(Habitat_Type == "Low Density Kelp")

sandy <- snorkel %>% 
  filter(Habitat_Type == "Sandy Area")

rip_rap <- snorkel %>% 
  filter(Habitat_Type == "Rip Rap")

all_habitats <- snorkel %>% 
  filter(Habitat_Type %in% c("High Density Kelp", "Low Density Kelp", 
                             "Sandy Area", "Rip Rap"))

# FISH
salmon <- snorkel %>% 
  select("Date","Chum", "Coho", "Pink", "Cutthroat", "Chinook", 
         "Unknown_Salmon") %>% 
  mutate(across(Chum:Unknown_Salmon, ~ ifelse(. == "Yes", 1, 0)))

salmon_long <- salmon %>% 
  pivot_longer(cols = Chum:Unknown_Salmon,
               names_to = "Juvenile_Salmon_Species",
               values_to = "Presence")

forage_fish <- snorkel %>% 
  select("Date", "Herring", "Sand_Lance", "Surf_Smelt", 
         "Three_Spine_Stickleback","Anchovy", "Unknown_Forage_Fish") %>% 
  mutate(across(Herring:Unknown_Forage_Fish, ~ ifelse(. == "Yes", 1, 0)))

forage_fish_long <- forage_fish %>% 
  pivot_longer(cols = Herring:Unknown_Forage_Fish,
               names_to = "Forage_Fish_Species",
               values_to = "Presence")

fish_long_combined <- bind_rows(salmon_long %>% mutate(
  Type = "Juvenile_Salmon_Species"),
  forage_fish_long %>% mutate(Type = "Forage_Fish_Species"))

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

#Bryozoa
ggplot(all_habitats, aes(x = Date, y = Bryozoan_Epibiont, color = Habitat_Type)) +
  geom_line() +  
  labs(x = "Date", y = "Overall Kelp Health", title = "Kelp Health Over 2024 Growing Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")


## 6. Heat Map -----------------------------------------------------------------

# Create a dot matrix for the salmon_long dataframe
ggplot(salmon_long, aes(x = Date, y = Juvenile_Salmon_Species)) +
  geom_point(aes(color = as.factor(Presence)), size = 3) +  # Add points for presence/absence
  scale_color_manual(values = c("grey", "blue"), labels = c("Absent", "Present"), name = "Presence") +  # Color for presence/absence
  labs(x = "Date", y = "Salmon Species", title = "Presence of Juvenile Salmon Over Time") +  # Labels for the plot
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

ggplot(forage_fish_long, aes(x = Date, y = Forage_Fish_Species)) +
  geom_point(aes(color = as.factor(Presence)), size = 3) +  # Add points for presence/absence
  scale_color_manual(values = c("grey", "blue"), labels = c("Absent", "Present"), name = "Presence") +  # Color for presence/absence
  labs(x = "Date", y = "Forage Fish Species", title = "Presence of Forage Fish Over Time") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b/%d")
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





