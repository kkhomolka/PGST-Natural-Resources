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

# 2. Read in files--------------------------------------------------------------
df <- read_excel("Zooplankton_microscopy_counts.xlsx")

#Reformatting the dates
df$Date <- as.Date(df$Date)

#Lump the rare species together to make plotting easier

mutate(df, `Zooplankton Species` = fct_lump_n(
  `Zooplankton Species`, n = 6,
  w = `Species Concentration (Individuals/L)`,
  other_level = "Other"))

# 4. Sum lifestages within species, per replicate ------------------------------

## each row is a species x lifestage, so sum before averaging
## fill absent species with 0 so the mean across the reps isn't messed up
rep_conc <- df |>
  group_by(Location, Date, `Sample Number`, `Zooplankton Species`) |>
  summarise(
    `Species Concentration (Individuals/L)` = sum(`Species Concentration (Individuals/L)`),
    .groups = "drop") |>
  complete(
    nesting(Location, Date, `Sample Number`),
    `Zooplankton Species`,
    fill = list(`Species Concentration (Individuals/L)` = 0))

# 5. Mean and SE across the 3 replicates ---------------------------------------
summ <- rep_conc |>
  group_by(Location, Date, `Zooplankton Species`) |>
  summarise(
    Mean = mean(`Species Concentration (Individuals/L)`),
    SE   = sd(`Species Concentration (Individuals/L)`) / sqrt(n()),
    .groups = "drop")


# 6. Plot ----------------------------------------------------------------------
ggplot(summ, aes(Date, Mean, color = `Zooplankton Species`, group = `Zooplankton Species`)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0, alpha = 0.6) +
  facet_wrap(~ Location, nrow = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(y = "Species Concentration (Individuals/L)", color = "Zooplankton Species") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))


# 7. Plot with rare species-----------------------------------------------------

rep_conc <- df |>
  group_by(Location, Date, `Sample Number`, `Zooplankton Species`) |>
  summarise(
    `Species Concentration (Individuals/L)` = sum(`Species Concentration (Individuals/L)`),
    .groups = "drop") |>
  complete(
    nesting(Location, Date, `Sample Number`),
    `Zooplankton Species`,
    fill = list(`Species Concentration (Individuals/L)` = 0)) |>
  mutate(Date = factor(Date))

#boxplot
ggplot(rep_conc,
       aes(Date, `Species Concentration (Individuals/L)`,
           fill = `Zooplankton Species`, color = `Zooplankton Species`)) +
  geom_boxplot(alpha = 0.35, outliers = FALSE,
               position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 1, alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ Location, nrow = 1, scales = "free_x") +   
  labs(y = "Species Concentration (Individuals/L)",
       fill = "Zooplankton Species", color = "Zooplankton Species") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
