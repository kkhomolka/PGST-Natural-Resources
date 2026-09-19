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

#3. Dataframe cleaning----------------------------------------------------------

#Reformatting the dates
df$Date <- as.Date(df$Date)

#Select columns of interest for zoop
df <- df %>% select(`Sample Number`, 
                   Location, 
                   Date,
                   `Zooplankton Species`, 
                   Lifestage,
                   Count,
                   `Species Concentration (Individuals/L)`,
                   `Species Concentration Multiplier (Individuals/L)`)



