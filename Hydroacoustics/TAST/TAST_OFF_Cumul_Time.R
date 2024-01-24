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

#convert date from a continuous variable to a categorical
TAST_OFF_Times_clean$Date <- as.factor(TAST_OFF_Times_clean$Date)

# 3. Read in ON Files ----------------------------------------------------------

## read in file and remove empty rows
TAST_ON_Times <- read_csv("Seal_presence_time_TAST_ON_KH.csv")
TAST_ON_Times_clean <- head(TAST_ON_Times, 343)

##reformat the date 
TAST_ON_Times_clean$Date <- as.Date(TAST_ON_Times_clean$Date, format = "%m/%d/%Y")

##remove NAs
#TAST_ON_Times_clean <- TAST_ON_Times_clean %>%
  #select_if(function(col) any(!is.na(col)))

#convert date from a continuous variable to a categorical
TAST_ON_Times_clean$Date <- as.factor(TAST_ON_Times_clean$Date)

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

#brute force approach
box_summarized_df <- combined_ON_OFF %>% 
  group_by(Tast_Status) %>% 
  summarise(
    mean = mean(Cumulative_Time_s),
    sd = sd(Cumulative_Time_s),
    lower = mean(Cumulative_Time_s) - sd(Cumulative_Time_s),
    upper = mean(Cumulative_Time_s) + sd(Cumulative_Time_s))

# 6. Draft Graphing ------------------------------------------------------------

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
  ggplot(aes(Date, Cumulative_Time_s, fill = Tast_Status))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~Tast_Status)+
  ggtitle("Non-normalized Cumulative Seal Presence")+
  theme_minimal()

ggplot(combined_ON_OFF, aes(x = Tast_Status, y = Cumulative_Time_s)) +
  geom_violin(fill = "lightblue", color = "blue") +
  geom_boxplot(width = 0.2, fill = "white", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1, color = "blue", alpha = 0.4)+
  labs(title = "Non-Normalized Cumulative Time of Seal Presence", 
       x = "Tast Status", 
       y = "Cumulative Time (s)")+
  theme_minimal()

# 7. Good Graphing -------------------------------------------------------------

## Plot a violin plot with customized boxplot points using different colors for "ON" and "OFF"
ggplot(combined_ON_OFF, aes(x = Tast_Status, y = Cumulative_Time_s, fill = Tast_Status)) +
  geom_violin(width = 1, alpha = 0.5, color = "black") +
  geom_boxplot(width = 0.2, fill = "white", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 0.9, color = "black", alpha = 0.3) +
  scale_fill_manual(values = c("ON" = "mediumturquoise", "OFF" = "mediumpurple2")) +
  labs(
    title = "Non-Normalized Cumulative Time of Seal Presence",
    x = "TAST Status",
    y = "Cumulative Time (secs)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

combined_ON_OFF %>%
  ggplot(aes(Date, Cumulative_Time_s, fill = Tast_Status))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~Tast_Status)+
  ggtitle("Non-normalized Cumulative Seal Presence")+
  theme_minimal()

