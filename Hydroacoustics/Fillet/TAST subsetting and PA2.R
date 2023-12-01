# 1. Setup ---------------------------------------------------------------------

## Load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyr, 
               dplyr,
               cowsay,
               lubridate,
               tidyverse,
               openxlsx)

## Set working directory
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")


# 2. Power analysis using 'pwr' package ----------------------------------------

## Two-sample t-test analysis
pwr.t.test(n = 400, #leaving n blank because that is the desired outcome
           d = , #assuming a medium-ish effect size 
           sig.level = 0.05, 
           power = 0.9, 
           type = c("two.sample"), 
           alternative = "two.sided")


# 3. Loading and cleaning data--------------------------------------------------

##read in files and remove empty rows
TAST_files <- read_csv("BlueView footage datasheet (OI).csv")
TAST_files_clean <- head(TAST_files, 6684)

## reformat the date 
TAST_files_clean$Date <- as.Date(TAST_files_clean$Date, format = "%m/%d/%y")

##create ON and OFF stratum if needed
TAST_ON <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "ON")
TAST_OFF <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "OFF")

# 4. Sub-setting by date -------------------------------------------------------

## filter data by desired date ranges
bydate_TAST <- TAST_files_clean %>% 
  filter((Date >= as.Date("2023-05-30") & Date <= as.Date("2023-05-31")) |
           Date >= as.Date("2023-06-05") & Date <= as.Date("2023-06-07")|
           Date >= as.Date("2023-06-12") & Date <= as.Date("2023-06-14")|
          Date >= as.Date("2023-06-20") & Date <= as.Date("2023-06-21"))

## filter by seal foraging time windows 
bydate_forage <- bydate_TAST %>%
  filter(`File timestamp`>= as.difftime("05:00:00") & `File timestamp`<= as.difftime("10:00:00")|
           `File timestamp`>= as.difftime("15:00:00") & `File timestamp`<= as.difftime("21:00:00"))

##create ON and OFF stratum using the forage time df
Forage_TAST_ON <- subset(bydate_forage, bydate_forage$`TAST status` == "ON")
Forage_TAST_OFF <- subset(bydate_forage, bydate_forage$`TAST status` == "OFF")


##removing 6/14/23 because there are no ON times on that day
Forage_TAST_ON <- Forage_TAST_ON %>% 
  filter((Date >= as.Date("2023-05-30") & Date <= as.Date("2023-05-31")) |
           Date >= as.Date("2023-06-05") & Date <= as.Date("2023-06-06")|
           Date >= as.Date("2023-06-12") & Date <= as.Date("2023-06-13")|
           Date >= as.Date("2023-06-20") & Date <= as.Date("2023-06-21"))

## removing overlapping times that were not filtered out for ON
rows_remove_ON <- c(85:89,181)
Forage_TAST_ON <- Forage_TAST_ON[-rows_remove_ON, ]

## removing overlapping dates for OFF
Forage_TAST_OFF <- Forage_TAST_OFF %>% 
  filter((Date >= as.Date("2023-05-30") & Date <= as.Date("2023-05-31")) |
           Date >= as.Date("2023-06-06") & Date <= as.Date("2023-06-07")|
           Date >= as.Date("2023-06-13") & Date <= as.Date("2023-06-14")|
           Date >= as.Date("2023-06-20") & Date <= as.Date("2023-06-21"))

## removing overlapping times that were not filtered out for OFF
rows_remove_off <- c(202:249)
Forage_TAST_OFF <- Forage_TAST_OFF[-rows_remove_off, ]

## export the final dataframes
write.csv(Forage_TAST_ON, "TAST ON Forage Times by Date.csv")
write.csv(Forage_TAST_OFF, "TAST OFF Forage Times by Date.csv")

# 6. Graphing ON/OFF by Date & Time --------------------------------------------

## calculate the duration of each status 
bytime_TAST_files <- bydate_TAST_files %>%
  arrange(`File timestamp`) %>% #this will order the times chronologically so there's no negative values
  group_by(Date, `TAST status`) %>%
  summarise(Duration = sum(difftime(lead(`File timestamp`, default = last(`File timestamp`)), `File timestamp`, units = "hours"), na.rm = TRUE))

## pivot wider
wide_bytime_TAST_files <- bytime_TAST_files %>%
  pivot_wider(names_from = `TAST status`, values_from = Duration)

## replace the NAs with 0 but we have to do it in duration format
wide_bytime_TAST_files[is.na(wide_bytime_TAST_files)] <- as.difftime(0, units = "hours")

## cumsum will only work for numeric values, not difftime. So we need to convert
wide_bytime_TAST_files$ON_numeric <- as.numeric(wide_bytime_TAST_files$ON)
wide_bytime_TAST_files$OFF_numeric <- as.numeric(wide_bytime_TAST_files$OFF)

## pivot longer 
forplot_bytime_TAST <- wide_bytime_TAST_files %>%
  pivot_longer(cols = c(ON_numeric, OFF_numeric), names_to = "Status", values_to = "Cumulative_Time")

## let's graph cumulative ON/OFF times by day 
ggplot(forplot_bytime_TAST, aes(x = Date, y = Cumulative_Time, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Cumulative ON/OFF Times by Day",
       x = "Date",
       y = "Cumulative Time (hours)",
       fill = "Status") +
  scale_fill_manual(values = c("ON_numeric" = "green", "OFF_numeric" = "pink")) +
  scale_x_date(breaks = unique(forplot_bytime_TAST$Date), date_labels = "%m-%d")+
  theme_minimal()

