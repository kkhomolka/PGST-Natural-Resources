# 1. Setup ---------------------------------------------------------------------

## Load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyr, 
               dplyr,
               cowsay,
               lubridate)

## Welcome
say("Welcome To All Things TAST!", 
    by = "shark", 
    what_color = "green", 
    by_color = "blue")

## Set working directory
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")


# 2. Power analysis using 'pwr' package ----------------------------------------

## Two-sample t-test analysis
pwr.t.test(n =, #leaving n blank because that is the desired outcome
           d = 0.3, #assuming a medium-ish effect size 
           sig.level = 0.05, 
           power = 0.9, 
           type = c("two.sample"), 
           alternative = "two.sided")


# 3. Loading and subsetting data-----------------------------------------------

##read in files and remove empty rows
TAST_files <- read_csv("BlueView footage datasheet (OI).csv")
TAST_files_clean <- head(TAST_files, 6684)

## reformat the date 
TAST_files_clean$Date <- as.Date(TAST_files_clean$Date, format = "%m/%d/%y")

## filter data by date 
bydate_TAST_files <- TAST_files_clean %>% 
  filter((Date >= as.Date("2023-05-30") & Date <= as.Date("2023-06-01")) |
           Date >= as.Date("2023-06-05") & Date <= as.Date("2023-06-09")|
           Date >= as.Date("2023-06-05") & Date <= as.Date("2023-06-07")|
           Date >= as.Date("2023-06-12") & Date <= as.Date("2023-06-16")|
           Date >= as.Date("2023-06-20") & Date <= as.Date("2023-06-22"))

##create ON and OFF stratum
TAST_ON <- subset(bydate_TAST_files, bydate_TAST_files$`TAST status` == "ON")
TAST_OFF <- subset(bydate_TAST_files, bydate_TAST_files$`TAST status` == "OFF")

# 4. Graphing ------------------------------------------------------------------

## let's try and graph the number of ON and OFF hours 
#bytime_TAST_files <- bydate_TAST_files %>%
 # filter(`TAST status`%in% c("ON", "OFF"))

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
  scale_fill_manual(values = c("ON_numeric" = "blue", "OFF_numeric" = "red")) +
  scale_x_date(breaks = unique(forplot_bytime_TAST$Date), date_labels = "%m-%d")+
                 theme_minimal()

