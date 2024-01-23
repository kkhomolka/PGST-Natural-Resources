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

## read in files and remove empty rows
TAST_OFF_Times <- read_csv("Seal_presence_time_spreadsheet_tast_OFF_KH.csv")
TAST_OFF_Times_clean <- head(TAST_OFF_Times, 320)

## reformat the date 
TAST_OFF_Times_clean$Date <- as.Date(TAST_OFF_Times_clean$Date, format = "%m/%d/%y")

##remove NAs
TAST_OFF_Times_clean <- TAST_OFF_Times_clean %>%
  select_if(function(col) any(!is.na(col)))

##changing column header 

# 2. Graphing -----------------------------------------------------------



                         