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

## Set working directory for KK WORK
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

## Set working directory for KK HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

# 2. Read in Files -------------------------------------------------------------

TAST_ON <- read.csv("TAST_ON_EV_Export_combined.csv")
TAST_OFF <- read.csv("TAST_OFF_EV_Export_combined.csv")

# reformat the dates 
TAST_ON$Date_M <- as.Date(TAST_ON$Date_M, format = "%m/%d/%Y")
TAST_OFF$Date_M <- as.Date(TAST_OFF$Date_M, format = "%m/%d/%Y")

# 3. Combine dfs and filter ----------------------------------------------------

TAST_combined <- rbind(TAST_OFF, TAST_ON)

TAST_combined <- TAST_combined %>% 
  select(Target_depth_mean, 
         Target_range_mean, 
         Fish_track_change_in_range,
         Speed_4D_mean_unsmoothed,
         Time_in_beam,
         Time_M,
         TAST_Status,
         Date_M,
         Process_ID)

TAST_combined %>% mutate(TAST_combined, Normalized_time_in_beam = Time)  
  
  
  
  
  
  
  
  
  
  



