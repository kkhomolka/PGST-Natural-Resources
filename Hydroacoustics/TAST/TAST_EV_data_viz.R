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
  select(Process_ID,
         Date_M,
         Time_M,
         TAST_Status,
         Target_depth_mean, 
         Target_range_mean, 
         Fish_track_change_in_range,
         Speed_4D_mean_unsmoothed,
         Time_in_beam)

# create normalization variables
ON_norm <- 154200
OFF_norm <- 147060

# create normalization column for time in beam 
TAST_combined <- TAST_combined %>% 
  mutate(Normalized_time_in_beam = if_else(TAST_Status == "ON", 
                                           Time_in_beam / ON_norm, 
                                           Time_in_beam / OFF_norm))

# 4. Time Conversions ----------------------------------------------------------

# need to convert the time from UTC to PST
# Combine Date and Time_M columns into a single datetime column in UTC
TAST_combined$DateTime_UTC <- as.POSIXct(paste(TAST_combined$Date_M, TAST_combined$Time_M), tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")

# Convert UTC to PST
TAST_combined$DateTime_PST <- as.POSIXct(TAST_combined$DateTime_UTC, tz = "America/Los_Angeles")

# remove date and just have Time_PST
TAST_combined$Time_PST <- format(TAST_combined$DateTime_PST, format = "%H:%M:%S")

# round time to the nearest hour
TAST_combined$DateTime_PST_rounded <- hour(TAST_combined$DateTime_PST)



# 5. Plotting ------------------------------------------------------------------

# Time_in_beam vs. Foraging Time Window
TAST_combined %>% 
  ggplot(aes(DateTime_PST_rounded, Normalized_time_in_beam, color = TAST_Status))+
  geom_jitter(aes(color = TAST_Status), size = 2.5, alpha = 0.4)+
  ggtitle("Normalized Seal Time in Beam by Peak Foraging Time Window")+
  scale_color_manual(values = c("ON" = "darkgreen", "OFF" = "darkred")) +
  labs(x = "Foraging Time (24 hrs)", y = "Normalized Seal Time in Beam (s)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(min(TAST_combined$DateTime_PST_rounded), 
                                  max(TAST_combined$DateTime_PST_rounded)))
  
# Time_in_beam density plot
TAST_combined %>% 
  ggplot(aes(Time_in_beam, color = TAST_Status))+
  geom_density()+
  ggtitle("Normalized Seal Time in Beam")

# Time_in_beam vs. TAST_Status
TAST_combined %>% 
  ggplot(aes(TAST_Status, Normalized_time_in_beam))

  
# Time_in_beam  
TAST_combined %>% 
  ggplot(aes(Time_in_beam, color = TAST_Status, fill = TAST_Status))+
  geom_dotplot()+
  ggtitle("Normalized Seal Time in Beam")
  
  
  
  
  



