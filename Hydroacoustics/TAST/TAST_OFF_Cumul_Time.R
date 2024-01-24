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

## Set working directory for WORK
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

##Set working directory for HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

# 2. Loading OFF Files --------------------------------------------------------

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

# 4. Check for duplicates or redundancies ---------------------------------------

#check OFF
duplicates_OFF <- TAST_OFF_Times_clean[duplicated(TAST_OFF_Times_clean[, c('File')]), ]

#check ON
duplicates_ON <- TAST_ON_Times_clean[duplicated(TAST_ON_Times_clean[, c('File')]), ]

# 5. Combine dataframes and group by date --------------------------------------

#bind both dataframes together into one 
combined_ON_OFF <- rbind(TAST_OFF_Times_clean, TAST_ON_Times_clean)

#removing 0 from the Cumulative_Time_s column
#combined_ON_OFF <- combined_ON_OFF %>% 
  #filter(Cumulative_Time_s != 0)

#summarize to calculate sum of cumulative time for ON
summarized_ON <- TAST_ON_Times_clean %>%
  group_by(Date) %>%
  summarize(Cumulative_Time_sum = sum(Cumulative_Time_s))

#summarize to calculate sum of cumulative time for OFF
summarized_OFF <- TAST_OFF_Times_clean %>%
  group_by(Date) %>%
  summarize(Cumulative_Time_sum = sum(Cumulative_Time_s))

#calculate stats for ON
box_stats_ON <- TAST_ON_Times_clean %>% 
  group_by(Tast_Status) %>% 
  summarise(
    mean = mean(Cumulative_Time_s),
    sd = sd(Cumulative_Time_s),
    se = sd(Cumulative_Time_s) / sqrt(n()),
    lower = mean(Cumulative_Time_s) - sd(Cumulative_Time_s),
    upper = mean(Cumulative_Time_s) + sd(Cumulative_Time_s))

#calculate stats for OFF
box_stats_OFF <- TAST_OFF_Times_clean %>% 
  group_by(Tast_Status) %>% 
  summarise(
    mean = mean(Cumulative_Time_s),
    sd = sd(Cumulative_Time_s),
    se = sd(Cumulative_Time_s) / sqrt(n()),
    lower = mean(Cumulative_Time_s) - sd(Cumulative_Time_s),
    upper = mean(Cumulative_Time_s) + sd(Cumulative_Time_s))

#binding the stats together for ON and OFF
box_stats_combined <- rbind(box_stats_OFF, box_stats_ON)

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
    y = "Cumulative Time (secs)") +
  theme_grey() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))
  
#boxplots 
combined_ON_OFF %>%
  ggplot(aes(Date, Cumulative_Time_s, fill = Tast_Status))+
  geom_boxplot(width = 0.2, fill = "mediumpurple2", color = "black", outlier.shape = NA)+
  geom_jitter(width = 0.2, size = 0.9, color = "black", alpha = 0.3)+
  facet_wrap(~Tast_Status)+
  ggtitle("Non-Normalized Cumulative Time of Seal Presence")+
  labs(x = "Date", y = "Cumulative Time (secs)")+
  theme_grey()+
    theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle=30, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

# Create a boxplot from the summarized statistics
ggplot(box_stats_combined, aes(x = Tast_Status, y = mean, fill = Tast_Status, group = Tast_Status)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("ON" = "mediumturquoise", "OFF" = "mediumpurple2")) +
  labs(
    title = "Bar Chart of Cumulative Time by TAST Status with Error Bars",
    x = "TAST Status",
    y = "Cumulative Time (s)")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))


combined_ON_OFF %>%
  mutate(Shortened_Date = format(Date, "%b %d")) %>%  # Create a new column with shortened dates
  ggplot(aes(x = Shortened_Date, y = Cumulative_Time_s, fill = Tast_Status)) +
  geom_boxplot(width = 0.2, fill = "mediumpurple2", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 0.9, color = "black", alpha = 0.3) +
  facet_wrap(~Tast_Status) +
  ggtitle("Non-Normalized Cumulative Time of Seal Presence") +
  labs(x = "Date", y = "Cumulative Time (secs)") +
  theme_grey() +
  theme(legend.position = "none")


