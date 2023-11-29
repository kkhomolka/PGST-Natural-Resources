# 1. Setup ---------------------------------------------------------------------

## Load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyverse, 
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
  filter((Date >= as.Date("2023-05-23") & Date <= as.Date("2023-05-26")) |
           Date >= as.Date("2023-06-05") & Date <= as.Date("2023-06-09"))

##create ON and OFF stratum
TAST_ON <- subset(bydate_TAST_files, bydate_TAST_files$`TAST status` == "ON")
TAST_OFF <- subset(bydate_TAST_files, bydate_TAST_files$`TAST status` == "OFF")


## let's try and graph the number of ON and OFF hours 
bytime_TAST_files <- bydate_TAST_files %>%
  filter(`TAST status`%in% c("ON", "OFF"))

## calculate the duration of each status 
bytime_TAST_files <- bydate_TAST_files %>%
  group_by(Date, `TAST status`) %>%
  summarise(Duration = sum(difftime(lead(`File timestamp`, default = last(`File timestamp`)), `File timestamp`, units = "secs"), na.rm = TRUE))

#pivot wider
wide_bytime_TAST_files <- bytime_TAST_files %>%
  pivot_wider(names_from = `TAST status`, values_from = Duration)

wide_bytime_TAST_files$ON <- as.difftime(wide_bytime_TAST_files$ON, units = "secs", origin = as.POSIXct("1970-01-01"))
wide_bytime_TAST_files$OFF <- as.difftime(wide_bytime_TAST_files$OFF, units = "secs", origin = as.POSIXct("1970-01-01"))
wide_bytime_TAST_files[is.na(wide_bytime_TAST_files)] <- 0

# Sum the durations for each day
wide_bytime_TAST_files <- wide_bytime_TAST_files %>%
  mutate(Total_ON = cumsum(ON), Total_OFF = cumsum(OFF))
