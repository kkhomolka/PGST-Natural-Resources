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

##removing rows by index number that we do not want to include 
rows_remove <- c(85:89,181,272:332)
Forage_TAST_ON <- Forage_TAST_ON[-rows_remove, ]


# 5. Save for Later ------------------------------------------------------------
## we have to create a dataframe for each date range to be able to filter the times 
## Define date ranges
date_ranges <- list(
  c("2023-05-30", "2023-06-01"),
  c("2023-06-05", "2023-06-07"),
  c("2023-06-12", "2023-06-14"),
  c("2023-06-14", "2023-06-16"),
  c("2023-06-20", "2023-06-22"))

## Create a list of data frames for each date range
dates_range_list <- lapply(date_ranges, function(dates) {
  bydate_TAST_files %>%
    filter(Date >= as.Date(dates[1]) & Date <= as.Date(dates[2]))})

## Assign names to the list elements
names(dates_range_list) <- paste("df", 1:length(dates_range_list), sep = "")

## Print the first few rows of each data frame
for (i in seq_along(dates_range_list)) {
  cat("Data Frame:", names(dates_range_list)[i], "\n")
  print(head(dates_range_list[[i]]))}

## Now we have to define the foraging time ranges 
time_ranges <- list(
  c("05:00:00", "10:00:00"),
  c("15:00:00", "21:00:00"))

# Convert time range values to POSIXct
time_ranges <- lapply(time_ranges, function(range) {
  as.POSIXct(strptime(range, format = "%H:%M:%S"), tz = "UTC")})

## Filter each data frame in the list for the time ranges
time_range_forage <- lapply(dates_range_list, function(df) {
  df %>%
    filter((as.POSIXct(`File timestamp`, format = "%H:%M:%S") >= time_ranges[[1]][1] &
              as.POSIXct(`File timestamp`, format = "%H:%M:%S") <= time_ranges[[1]][2]) |
             (as.POSIXct(`File timestamp`, format = "%H:%M:%S") >= time_ranges[[2]][1] &
                as.POSIXct(`File timestamp`, format = "%H:%M:%S") <= time_ranges[[2]][2]))})

# Assign names to the filtered list elements
names(time_range_forage) <- paste("filtered_df", 1:length(time_range_forage), sep = "")

# Print the first few rows of each filtered data frame
for (i in seq_along(time_range_forage)) {
  cat("Filtered Data Frame:", names(time_range_forage)[i], "\n")
  print(head(time_range_forage[[i]]))}

# Create separate data frames in the global environment
list2env(time_range_forage, envir = .GlobalEnv)





## filter by seal foraging time windows 
bydate_forage_TAST <- bydate_TAST_files %>%
  filter(`File timestamp`>= as.difftime("05:00:00") & `File timestamp`<= as.difftime("10:00:00")|
  `File timestamp`>= as.difftime("15:00:00") & `File timestamp`<= as.difftime("21:00:00"))
          




# 6. Graphing ------------------------------------------------------------------

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


