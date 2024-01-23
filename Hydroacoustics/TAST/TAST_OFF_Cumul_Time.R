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
               readxl)

## Set working directory
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

## 2. Loading OFF Files --------------------------------------------------------

## read in file and remove empty rows
TAST_OFF_Times <- read_csv("Seal_presence_time_TAST_OFF_KH.csv")
TAST_OFF_Times_clean <- head(TAST_OFF_Times, 320)

## reformat the date 
TAST_OFF_Times_clean$Date <- as.Date(TAST_OFF_Times_clean$Date, format = "%m/%d/%y")

##remove NAs
#TAST_OFF_Times_clean <- TAST_OFF_Times_clean %>%
  #select_if(function(col) any(!is.na(col)))

# 3. Read in ON Files ----------------------------------------------------------

## read in file and remove empty rows
TAST_ON_Times <- read_csv("Seal_presence_time_TAST_ON_KH.csv")
TAST_ON_Times_clean <- head(TAST_ON_Times, 343)

##reformat the date 
TAST_ON_Times_clean$Date <- as.Date(TAST_ON_Times_clean$Date, format = "%m/%d/%y")

##remove NAs
#TAST_ON_Times_clean <- TAST_ON_Times_clean %>%
  #select_if(function(col) any(!is.na(col)))

#4. Check for duplicates or redundancies ---------------------------------------

#check OFF
duplicates_OFF <- TAST_OFF_Times_clean[duplicated(TAST_OFF_Times_clean[, c('File')]), ]

#check ON
duplicates_ON <- TAST_ON_Times_clean[duplicated(TAST_ON_Times_clean[, c('File')]), ]

# 5. Combine datasets ----------------------------------------------------------

combined_ON_OFF <- rbind(TAST_OFF_Times_clean, TAST_ON_Times_clean)




                         