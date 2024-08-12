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
               purrr)

## Set working directory for KK WORK - old wd() greyed out below 
#setwd("Z:/NR-Research/2023 Hood Canal Bridge/2023 TAST project/TAST Echoview Analysis/EV_Data_Export/EV_data_exports_Tast_ON")
setwd("Z:/NR-Research/2023 Hood Canal Bridge/2023 TAST project/TAST Echoview Analysis/EV_Data_Export/EV_data_exports_full_day")

# 2. ON Files ---------------------------------------------------------------------

files <- list.files(pattern = "*.csv")

combined_data <- data.frame()

for (file in files) {
  data <- read.csv(file, header = TRUE)
  combined_data <- rbind(combined_data, data)
}

#previous export name greyed out below 
#write.csv(combined_data, "TAST_ON_EV_Export_combined.csv", row.names = FALSE)

write.csv(combined_data, "TAST_Combined_EV_Export_fullday.csv", row.names = FALSE)

# 3. OFF Files -----------------------------------------------------------------

#change the wd() to read all files in the OFF folder 
setwd("Z:/NR-Research/2023 Hood Canal Bridge/2023 TAST project/TAST Echoview Analysis/EV_Data_Export/EV_data_exports_Tast_OFF")

files <- list.files(pattern = "*.csv")

combined_data <- data.frame()

for (file in files) {
  data <- read.csv(file, header = TRUE)
  combined_data <- rbind(combined_data, data)
}

write.csv(combined_data, "TAST_OFF_EV_Export_combined.csv", row.names = FALSE)

# DONE!

