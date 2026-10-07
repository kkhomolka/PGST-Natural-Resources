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
               ggeffects,
               glmmTMB,
               DHARMa,
               emmeans,
               permute)
               
font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#Use for windows operating systems
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

# 2. Read in files--------------------------------------------------------------
df <- read_excel("Zooplankton_microscopy_counts.xlsx")
df$`Zooplankton Species` <- tools::toTitleCase(tolower(df$`Zooplankton Species`))

#Reformatting the dates
df$Date <- as.Date(df$Date)
time_part <- format(df$Time, format = "%H:%M:%S")
df <- df %>% mutate(DateTime = as.POSIXct(paste(df$Date, time_part), format = "%Y-%m-%d %H:%M:%S"))

# 3. Collapse rare species------------------------------------------------------
df_top6 <- df |>
  mutate(
    `Zooplankton Species` = fct_lump_n(
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
rep_conc_box <- df_top6 |>
  group_by(Location, Date, `Sample Number`, `Zooplankton Species`) |>
  summarise(
    `Species Concentration (Individuals/L)` = sum(`Species Concentration (Individuals/L)`),
    .groups = "drop") |>
  complete(
    nesting(Location, Date, `Sample Number`),
    `Zooplankton Species`,
    fill = list(`Species Concentration (Individuals/L)` = 0)) |>
  mutate(Date = factor(Date))

ggplot(rep_conc_box,
       aes(Date, `Species Concentration (Individuals/L)`,
           fill = `Zooplankton Species`, color = `Zooplankton Species`)) +
  geom_boxplot(alpha = 0.35, outliers = FALSE,
               position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 1, alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ Location, nrow = 1, scales = "free_x") +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %d")) +
  labs(y = "Species Concentration (Individuals/L)",
       fill = "Zooplankton Species", color = "Zooplankton Species") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

# 8. Total concentration per replicate -----------------------------------------
## one row per replicate: totals across all species and lifestages
total <- df |>
  group_by(Location, Date, `Sample Number`) |>
  summarise(
    Count = sum(Count),
    `Species Concentration (Individuals/L)` = sum(`Species Concentration (Individuals/L)`),
    `Volume Filtered (L)` = first(`Volume Filtered (L)`),
    .groups = "drop")

#Plotting total concentration over time by station
ggplot(total, aes(Date, `Species Concentration (Individuals/L)`,
                color = Location, group = Location)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(y = "Total Species Concentration (Individuals/L)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plotting total concentration by station 
ggplot(total, aes(Location, `Species Concentration (Individuals/L)`)) +
  geom_boxplot(outliers = FALSE, fill = NA) +
  geom_jitter(aes(color = Date), width = 0.2, size = 4) +
  labs(y = "Total Species Concentration (Individuals/L)") +
  theme_bw()
