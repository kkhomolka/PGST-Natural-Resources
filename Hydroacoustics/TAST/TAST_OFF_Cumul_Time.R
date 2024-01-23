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
TAST_OFF_Times_clean$Date <- as.Date(TAST_OFF_Times_clean$Date, format = "%m/%d/%Y")

##remove NAs
#TAST_OFF_Times_clean <- TAST_OFF_Times_clean %>%
  #select_if(function(col) any(!is.na(col)))

# 3. Read in ON Files ----------------------------------------------------------

## read in file and remove empty rows
TAST_ON_Times <- read_csv("Seal_presence_time_TAST_ON_KH.csv")
TAST_ON_Times_clean <- head(TAST_ON_Times, 343)

##reformat the date 
TAST_ON_Times_clean$Date <- as.Date(TAST_ON_Times_clean$Date, format = "%m/%d/%Y")

##remove NAs
#TAST_ON_Times_clean <- TAST_ON_Times_clean %>%
  #select_if(function(col) any(!is.na(col)))

#4. Check for duplicates or redundancies ---------------------------------------

#check OFF
duplicates_OFF <- TAST_OFF_Times_clean[duplicated(TAST_OFF_Times_clean[, c('File')]), ]

#check ON
duplicates_ON <- TAST_ON_Times_clean[duplicated(TAST_ON_Times_clean[, c('File')]), ]

# 5. Combine dataframes and group by date --------------------------------------

combined_ON_OFF <- rbind(TAST_OFF_Times_clean, TAST_ON_Times_clean)

#convert date from a continuous variable to a categorical
combined_ON_OFF$Date <- as.factor(combined_ON_OFF$Date)

#removing 0 from the Cumulative_Time_s column
combined_ON_OFF <- combined_ON_OFF %>% 
  filter(Cumulative_Time_s != 0)

#summarize to calculate sum of cumulative time
summarized_df <- combined_ON_OFF %>%
  group_by(Date) %>%
  summarize(Cumulative_Time_sum = sum(Cumulative_Time_s))



# 6. Graphing ------------------------------------------------------------------

boxplot(Cumulative_Time_sum ~ Date, data = summarized_df,
        main = "Cumulative Seal Presence",
        xlab = "Date",
        ylab = "Non-normalized Cumulative Time (s)")

ggplot(summarized_df, aes(x = Date, y = Cumulative_Time_sum)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Cumulative Seal Presence",
       x = "Date",
       y = "Non-normalized Cumulative Time (s)") +
      theme_minimal()

combined_ON_OFF %>%
  ggplot(aes(Date, Cumulative_Time_s,))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~Tast_Status)+
  theme_bw()
  

                         