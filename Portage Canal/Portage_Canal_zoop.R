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

#3. Dataframe transformations---------------------------------------------------

#Reformatting the dates
df$Date <- as.Date(df$Date)

# Extracting the time component and pasting it with the correct date because
# of Excel adding 1899-12-31 to each time entry...
time_part <- format(df$Time, format = "%H:%M:%S")
df$DateTime <- as.POSIXct(paste(df$Date, time_part), format = "%Y-%m-%d %H:%M:%S")

#Select columns of interest
df <- df |> select(Location, 
                   DateTime, 
                   `Environmental Plankton Concentration`, 
                   `Zooplankton Species`, 
                   Lifestage,
                   Count)
