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
               ggformula)

# Aesthetics color palette
clrblind_pal <- c(
  "#edbd00",  # golden yellow
  "#1dd2d3",  # teal
  "#78b41f",  # green
  "#7487ff",  # periwinkle
  "#b41f78"   # magenta
)

clrblind_pal_fun <- function(n) {
  if (n > length(clrblind_pal)) stop("Palette only has ", length(clrblind_pal), " colors.")
  clrblind_pal[1:n]}


## Set working directory for KK WORK
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/Fillet/2023")

## Set working directory for KK HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics/Fillet/2023")


# 2. Reading in & formatting BlueView Time in Beam Files -----------------------

# Reading in file
BV_fullday <- read_csv("2023_FGS_full_days_seal_presence_final.csv", 
                       col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# Selecting columns of interest
BV_fullday <- BV_fullday %>% 
  select(File,
         File_Timestamp,
         Date,
         Cumulative_Time_s,
         FGS_Status)

#Reformatting the dates
BV_fullday$Date <- as.Date(BV_fullday$Date)

# Extracting the time component and pasting it with the correct date because
# of Excel adding 1899-12-31 to each time entry...
time_part <- format(BV_fullday$File_Timestamp, format = "%H:%M:%S")
BV_fullday$DateTime <- as.POSIXct(paste(BV_fullday$Date, time_part), format = "%Y-%m-%d %H:%M:%S")

## 3. Removing Outliers --------------------------------------------------------

# remove na's
BV_fullday <- na.omit(BV_fullday)

# Function to identify and remove the top 3 outliers for a given vector
remove_top_outliers <- function(data, column_name) {
  # Calculate the first quartile (Q1) and third quartile (Q3)
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  
  # Calculate the interquartile range (IQR)
  IQR <- Q3 - Q1
  
  # Define the lower and upper bounds for identifying outliers
  lower_bound <- Q1 - 3.0 * IQR
  upper_bound <- Q3 + 3.0 * IQR
  
  # Identify outliers
  outliers <- data[[column_name]] < lower_bound | data[[column_name]] > upper_bound
  
  # Identify the top 3 outliers
  top_outliers <- head(sort(data[[column_name]][outliers], decreasing = TRUE), 3)
  
  # Remove the top 3 outliers from the data
  data <- data[!data[[column_name]] %in% top_outliers, ]
  
  return(data)
}

# Apply the function to each TAST_Status group
BV_fullday <- BV_fullday %>%
  group_by(TAST_Status) %>%
  group_modify(~ remove_top_outliers(.x, "Cumulative_Time_s")) %>%
  ungroup()

## 4. Normalizing --------------------------------------------------------------

# Need to determine the cumulative time in beam for ON/OFF Status to normalize,
# and also find the average for each status (not counting the zero values)
BV_cumul <- BV_fullday %>% 
  group_by(TAST_Status) %>% 
  summarise(Total_Beam_Time_s = sum(Cumulative_Time_s),
            Avg_Beam_Time_s = mean(Cumulative_Time_s[Cumulative_Time_s !=0]))

# use same normalization variables as EV files
ON_norm <- 1.12 #this was calculated by dividing total OFF time / total ON time
OFF_norm <- 1 #this is 1 because it was total OFF time / total OFF time

# create normalization column for time in beam and multiplied by 10^5 to
# improve the readability when plotting 
BV_fullday <- BV_fullday %>% 
  mutate(BV_Normalized_time_in_beam = if_else(TAST_Status == "ON", 
                                              Cumulative_Time_s * ON_norm, 
                                              Cumulative_Time_s * OFF_norm))

## 5. Create non-zero values for BV normalized and non-normalized --------------

# Filter out non-zero values for boxplot
BV_non_zero_data <- BV_fullday[BV_fullday$Cumulative_Time_s != 0, ]
