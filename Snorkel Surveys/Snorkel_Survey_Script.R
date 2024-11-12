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

# 5. Reshaping Data ------------------------------------------------------------

# Salmon pivot longer
salmon <- snorkel %>% 
  select("Habitat_Type","Date","Chum", "Coho", "Pink", "Cutthroat", "Chinook", 
         "Unknown_Salmon") %>% 
  mutate(across(Chum:Unknown_Salmon, ~ ifelse(. == "Yes", 1, 0)))

salmon_long <- salmon %>% 
  pivot_longer(cols = Chum:Unknown_Salmon,
               names_to = "Juvenile_Salmon_Species",
               values_to = "Presence")

# Forage fish pivot longer
forage_fish <- snorkel %>% 
  select("Habitat_Type", "Date", "Herring", "Sand_Lance", "Surf_Smelt", 
         "Three_Spine_Stickleback","Anchovy", "Unknown_Forage_Fish") %>% 
  mutate(across(Herring:Unknown_Forage_Fish, ~ ifelse(. == "Yes", 1, 0)))

forage_fish_long <- forage_fish %>% 
  pivot_longer(cols = Herring:Unknown_Forage_Fish,
               names_to = "Forage_Fish_Species",
               values_to = "Presence")

# Bind both dfs
fish_long_combined <- bind_rows(salmon_long %>% mutate(
  Type = "Juvenile_Salmon_Species"),
  forage_fish_long %>% mutate(Type = "Forage_Fish_Species"))

## 6. Plotting -----------------------------------------------------------------

# Facet panels for Habitat Type vs. Overall Kelp Health
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
  labs(x = "Date", y = "Overall Kelp Health", title = "Bryozoa Presence Over 2024 Growing Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")

# Facet panel for Habitat Type vs. Sorus
ggplot(all_habitats, aes(x = Date, y = Sorus, color = Habitat_Type)) +
  geom_line() +  
  labs(x = "Date", y = "Sorus Development", title = "Sorus Development Over 2024 Growing Season") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")

# Facet panel for Habitat Type vs. Kelp Crab
ggplot(all_habitats, aes(x = Date, y = Kelp_Crab_Epifauna, color = Habitat_Type)) +
  geom_line() +  
  labs(x = "Date", y = "Kelp Crab Coverage", title = "Kelp Crab Coverage Over 2024 Growing Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")

# Facet panel for Habitat Type vs. Filamentous Algae
ggplot(all_habitats, aes(x = Date, y = Filamentous_Epiphyte, color = Habitat_Type)) +
  geom_line() +  
  labs(x = "Date", y = "Filamentous Epiphytic Algae Growth", title = "Filamentous Epiphytic Algal Growth Over 2024 Growing Season") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(1,5), breaks = 1:5)+
  theme_bw()+
  facet_wrap(~ Habitat_Type, scales = "free_y")

# Missing rows from geom_line()?
# Rows are removed from geom)line() because the df contains NAs, which is OK

# Look for NA values in the relevant columns
sum(is.na(all_habitats$Overall_Kelp_Health))
sum(is.na(all_habitats$Date))

# Or see which rows have NA values
missing_rows <- all_habitats[is.na(all_habitats$Overall_Kelp_Health) | is.na(all_habitats$Date), ]
print(missing_rows)

## 6. Dot Matrices -------------------------------------------------------------

# Dot Matrix for Forage Fish with Points Jittered
ggplot(forage_fish_long, aes(x = Date, y = Forage_Fish_Species)) +
  geom_point(aes(color = as.factor(Presence)), size = 3,
             position = position_jitter(width = 0, height = 0.5)) +  # Add points for presence/absence
  scale_color_manual(values = c("grey", "blue"), labels = c("Absent", "Present"), name = "Presence") +  # Color for presence/absence
  labs(x = "Date", y = "Forage Fish Species", title = "Presence of Forage Fish Over Time") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b/%d")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dot Matrix for Juvenile Salmon with Points Jittered
  ggplot(salmon_long, aes(x = Date, y = Juvenile_Salmon_Species)) +
    geom_point(aes(color = as.factor(Presence)), 
               size = 3,
               position = position_jitter(width = 0, height = 0.5)) +  # Add slight vertical jitter
    scale_color_manual(values = c("grey", "blue"), 
                       labels = c("Absent", "Present"), 
                       name = "Presence") +
    scale_x_date(date_breaks = "2 week", date_labels = "%b/%d")+
    labs(x = "Date", 
         y = "Salmon Species", 
         title = "Presence of Juvenile Salmon Over Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Salmon and Habitat Type Facet
  ggplot(salmon_long, aes(x = Date, y = Juvenile_Salmon_Species, color = as.factor(Presence))) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = c("grey", "navy"), labels = c("Absent", "Present"), name = "Presence") +
    scale_x_date(date_breaks = "2 week", date_labels = "%b %d")+
    facet_wrap(~ Habitat_Type, ncol = 2) +
    labs(x = "Date", y = "Salmon Species", title = "Juvenile Salmon Presence During 2024 Field Season") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Forage Fish and Habitat Facet
  ggplot(forage_fish_long, aes(x = Date, y = Forage_Fish_Species, color = as.factor(Presence))) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = c("grey", "navy"), labels = c("Absent", "Present"), name = "Presence") +
    scale_x_date(date_breaks = "2 week", date_labels = "%b/%d")+
    facet_wrap(~ Habitat_Type, ncol = 2) +
    labs(x = "Date", y = "Forage Fish Species", title = "Forage Fish Presence During 2024 Field Season") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
