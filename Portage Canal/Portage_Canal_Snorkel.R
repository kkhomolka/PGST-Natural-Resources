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
               showtext,
               ggeffects)

font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#Use for windows operating systems
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

# 2. Read and clean-------------------------------------------------------------
df <- read_csv("Portage Canal Snorkel Survey.csv")

#Make sure the date is a date object
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

#Remove pesky trailing spaces for snorkel
names(df) <- str_trim(names(df))

#Make sure habitat_type is a factor for plotting
df <- df %>%
  mutate(Habitat_Type = factor(Habitat_Type,
            levels = c("High Density Kelp", "Low Density Kelp",
                 "Sandy Area", "Rip Rap")))


# 3. Kelp condition scores over time (1-5 scale)--------------------------------

# Reshape the five condition score columns to long format for faceting
condition_long <- df %>%
  select(Date, Habitat_Type,
         Overall_Kelp_Health, Kelp_Blades, Pneumatocyst, Sorus, Senescence) %>%
  pivot_longer(
    cols = c(Overall_Kelp_Health, Kelp_Blades, Pneumatocyst, Sorus, Senescence),
    names_to = "Metric", values_to = "Score") %>%
  filter(!is.na(Score))

p_condition <- ggplot(condition_long, aes(x = Date, y = Score, color = Habitat_Type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9, linewidth = 0.8) +
  facet_wrap(~ Metric, ncol = 2) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(x = "Month", y = "Qualitative Score (1-5)", color = "Habitat Type") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("kelp_condition_over_time.png", p_condition, width = 10, height = 8, dpi = 300)


# 4. Kelp height by depth position over time------------------------------------

height_long <- df %>%
  select(Date, Habitat_Type, Kelp_Height_1_m, Kelp_Height_2_m, Kelp_Height_3_m) %>%
  pivot_longer(
    cols = starts_with("Kelp_Height"),
    names_to = "Depth_Class", values_to = "Height_m"
  ) %>%
  filter(!is.na(Height_m)) %>%
  mutate(Depth_Class = recode(Depth_Class,
                              "Kelp_Height_1_m" = "Shallow",
                              "Kelp_Height_2_m" = "Mid",
                              "Kelp_Height_3_m" = "Deep"))

p_height <- ggplot(height_long, aes(x = Date, y = Height_m, color = Depth_Class)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9) +
  facet_wrap(~ Habitat_Type) +
  labs(x = "Survey Date", y = "Kelp Height (m)", color = "Depth Position") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("kelp_height_over_time.png", p_height, width = 9, height = 7, dpi = 300)


# 4. Epibiont / epiphyte scores over time---------------------------------------

epibiont_long <- df %>%
  select(Date, Habitat_Type,
         Bryozoan_Epibiont, Filamentous_Epiphyte, Macroalgae_Epiphyte, Kelp_Crab_Epifauna) %>%
  pivot_longer(
    cols = c(Bryozoan_Epibiont, Filamentous_Epiphyte, Macroalgae_Epiphyte, Kelp_Crab_Epifauna),
    names_to = "Epibiont_Type", values_to = "Score"
  ) %>%
  filter(!is.na(Score))

p_epibiont <- ggplot(epibiont_long, aes(x = Date, y = Score, color = Habitat_Type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9, linewidth = 0.8) +
  facet_wrap(~ Epibiont_Type, ncol = 2) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs( x = "Survey Date", y = "Qualitative Score (1-5)", color = "Habitat Type") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("epibiont_scores_over_time.png", p_epibiont, width = 9, height = 7, dpi = 300)


# 5. Salmonid presence by date--------------------------------------------------

salmon_cols <- c("Chum", "Coho", "Pink", "Cutthroat", "Chinook", "Unknown_Salmon")

salmon_long <- df %>%
  mutate(Month = floor_date(Date, "month")) %>%
  select(Month, Habitat_Type, all_of(salmon_cols)) %>%
  pivot_longer(cols = all_of(salmon_cols), names_to = "Species", values_to = "Present") %>%
  filter(Present == "Yes") %>%
  count(Month, Habitat_Type, Species, name = "Sightings")

p_salmon <- ggplot(salmon_long, aes(x = Month, y = Sightings, fill = Species)) +
  geom_col(position = "stack") +
  facet_wrap(~Habitat_Type)+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  labs(x = "Month", y = "Number of Observations", fill = "Species") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("salmonid_sightings_by_date.png", p_salmon, width = 9, height = 6, dpi = 300)


#Trying to visualize the data in a different way with a weird "heat map"
salmon_presence <- df %>%
  select(Date, Habitat_Type, all_of(salmon_cols)) %>%
  pivot_longer(cols = all_of(salmon_cols), names_to = "Species", values_to = "Present") %>%
  mutate(Present = replace_na(Present, "No"))  # treat NA as absent/not recorded

p_salmon_heat <- ggplot(salmon_presence, aes(x = Date, y = Species, fill = Present)) +
  geom_tile(color = "grey90", linewidth = 0.2) +
  facet_wrap(~ Habitat_Type, nrow = 4, axes = "all", scales = "free_x") +
  scale_fill_manual(values = c("No" = "white", "Yes" = "darkblue"), guide = "none",) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  labs(x = "Survey Date", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("salmonid_presence_heatmap.png", p_salmon_heat, width = 10, height = 7, dpi = 300)

# 6. Forage fish presence by survey date----------------------------------------

forage_cols <- c("Herring", "Sand_Lance", "Surf_Smelt", "Three_Spine_Stickleback",
                 "Anchovy", "Unknown_Forage_Fish")

forage_long <- df %>%
  mutate(Month = floor_date(Date, "month")) %>%
  select(Month, Habitat_Type, all_of(forage_cols)) %>%
  pivot_longer(cols = all_of(forage_cols), names_to = "Species", values_to = "Present") %>%
  filter(Present == "Yes") %>%
  count(Habitat_Type, Month, Species, name = "Sightings")

p_forage <- ggplot(forage_long, aes(x = Month, y = Sightings, fill = Species)) +
  geom_col(position = "stack") +
  facet_wrap(~ Habitat_Type) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Number of Sightings", fill = "Species") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("forage_fish_sightings_by_month_habitat.png", p_forage, width = 9, height = 7, dpi = 300)

#Trying to plot this the same as the salmonid "heat map"
forage_presence <- df %>%
  select(Date, Habitat_Type, all_of(forage_cols)) %>%
  pivot_longer(cols = all_of(forage_cols), names_to = "Species", values_to = "Present") %>%
  mutate(Present = replace_na(Present, "No"))

p_forage_heat <- ggplot(forage_presence, aes(x = Date, y = Species, fill = Present)) +
  geom_tile(color = "grey90", linewidth = 0.2) +
  facet_wrap(~ Habitat_Type, nrow = 4, axes = "all", scale = "free_x") +
  scale_fill_manual(values = c("No" = "white", "Yes" = "darkred"), guide = "none") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  labs(x = "Survey Date", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("forage_fish_presence_heatmap.png", p_forage_heat, width = 10, height = 7, dpi = 300)

