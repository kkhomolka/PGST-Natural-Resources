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
               permute,
               gt)

font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#Use for windows operating systems
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

# 2. Read in files--------------------------------------------------------------
df_0329 <- read_csv("dtx_exports/20240329.csv")
df_0405 <- read_csv("dtx_exports/20240405.csv")
df_0412 <- read_csv("dtx_exports/20240412.csv")
df_0419 <- read_csv("dtx_exports/20240419.csv")
df_0426 <- read_csv("dtx_exports/20240426.csv")
df_0524 <- read_csv("dtx_exports/20240524.csv")

# Select the filenames I want to keep
keep_files <- c("20240329_140635.dt4", "20240405_122218.dt4", "20240412_103611.dt4",
                "20240419_111531.dt4", "20240426_111537.dt4", "20240524_193424.dt4")

# Bind the rows together into one df
df<- bind_rows(df_0329, df_0405, df_0412, df_0419, df_0426, df_0524) %>%
  filter(FileName %in% keep_files) %>%
  mutate(survey_date = ymd(str_sub(FileName, 1, 8)))

#Reformatting the dates
time_part <- format(df$Time, format = "%H:%M:%S")
df <- df %>% mutate(DateTime = as.POSIXct(paste(df$Time, time_part), format = "%Y-%m-%d %H:%M:%S"))

# 3. Simple Stats---------------------------------------------------------------
height_summary <- df %>%
  group_by(survey_date) %>%
  summarise(
    n_pings     = n(),
    n_veg       = sum(PlantHeight_m > 0, na.rm = TRUE),
    pct_veg     = 100 * n_veg / n_pings,
    mean_height = mean(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    sd_height   = sd(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    min_height  = min(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    max_height  = max(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE))

height_summary

# Creating a df with the zero pings removed, only vegetated pings remain
df_veg <- df %>% filter(PlantHeight_m > 0)

