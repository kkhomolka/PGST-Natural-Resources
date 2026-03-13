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
  "#F58E27",  
  "#272EF5",  
  "#78b41f",  
  "#7487ff",  
  "#b41f78"   
)

clrblind_pal_fun <- function(n) {
  if (n > length(clrblind_pal)) stop("Palette only has ", length(clrblind_pal), " colors.")
  clrblind_pal[1:n]}


## Set working directory for KK WORK
setwd("Z:/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

## Set working directory for KK HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

# 2. Reading in & formatting BlueView Time in Beam Files -----------------------

# Reading in file
BV_fullday <- read_excel("TAST_full_day_and_foraging_window_seal_presence_20260126.xlsx")

# Selecting columns of interest
BV_fullday <- BV_fullday %>% 
  select(File,
         File_Timestamp,
         Date,
         Cumulative_Time_s,
         TAST_Status)

#Reformatting the dates
BV_fullday$Date <- as.Date(BV_fullday$Date)

# Extracting the time component and pasting it with the correct date because
# of Excel adding 1899-12-31 to each time entry...
time_part <- format(BV_fullday$File_Timestamp, format = "%H:%M:%S")
BV_fullday$DateTime <- as.POSIXct(paste(BV_fullday$Date, time_part), format = "%Y-%m-%d %H:%M:%S")


## 3. Sorting and Wrangling ----------------------------------------------------

# Making sure that data are ordered properly
BV_fullday <- BV_fullday %>% 
  arrange(DateTime)

# Calculate file end time and duration
BV_fullday <- BV_fullday %>%
  mutate(File_EndTime = lead(DateTime),
         File_Duration_s = as.numeric(difftime(File_EndTime, DateTime, units = "secs")))

#Removing bad time durations (if any)
BV_fullday <- BV_fullday %>%
  filter(!is.na(File_Duration_s),
         File_Duration_s > 0,
         File_Duration_s < 1000)


## 4. Normalizing using presence rate-------------------------------------------

# Creating normalized column for rate of seal presence
# Seal_Presence_Rate describes the percentage of time a seal was in that file
BV_fullday <- BV_fullday %>%
  mutate(
    Seal_Presence_Rate = Cumulative_Time_s / File_Duration_s)

# Creating binary for use in stats later on
BV_fullday <- BV_fullday %>%
  mutate(
    Seal_Present = Cumulative_Time_s > 0)

# Quick look at the data
summary(BV_fullday$File_Duration_s)
summary(BV_fullday$Seal_Presence_Rate)

# Sanity check, presence rate should never exceed 1
any(BV_fullday$Seal_Presence_Rate > 1)

# Shows why we needed to normalize the data
BV_fullday %>%
  group_by(TAST_Status) %>%
  summarise(
    n_files = n(),
    total_effort_hr = sum(File_Duration_s) / 3600,
    mean_file_min = mean(File_Duration_s) / 60)

# Quick plot
ggplot(BV_fullday, aes(TAST_Status, Seal_Presence_Rate, fill = TAST_Status)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  scale_fill_manual(values = clrblind_pal_fun(2)) +
  theme_classic()

# Sanity check for the file duration distribution
hist(BV_fullday$File_Duration_s, breaks = 50)


## 4. Normalizing using time analyzed-------------------------------------------

# Need to determine the cumulative time in beam for ON/OFF Status to normalize,
# and also find the average for each status (not counting the zero values)
BV_cumul <- BV_fullday %>% 
  group_by(TAST_Status) %>% 
  summarise(Total_Beam_Time_s = sum(Cumulative_Time_s),
            Avg_Beam_Time_s = mean(Cumulative_Time_s[Cumulative_Time_s !=0]))

# use same normalization variables as EV files
#ON_norm <- 1.12 #this was calculated by dividing total OFF time / total ON time
#OFF_norm <- 1 #this is 1 because it was total OFF time / total OFF time

#this was calculated by dividing total ON time analyzed / total OFF time analyzed
# should only be applied to OFF times to downsample since there are more OFF files
OFF_norm <- 0.89
ON_norm <- 1


# create normalization column for time in beam and multiplied by 10^5 to
# improve the readability when plotting 
BV_fullday <- BV_fullday %>% 
  mutate(BV_Normalized_time_in_beam = if_else(TAST_Status == "ON", 
                                              Cumulative_Time_s * ON_norm, 
                                              Cumulative_Time_s * OFF_norm))


## 5. Removing Outliers --------------------------------------------------------

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


## 6. Create non-zero values for BV normalized and non-normalized --------------

# Filter out non-zero values for boxplot
BV_non_zero_data <- BV_fullday[BV_fullday$Cumulative_Time_s != 0, ]


## 7. BV Plotting ---------------------------------------------------------------

install.packages("showtext")
library(showtext)

font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

# Violin plot
BV_fullday %>% 
  ggplot(aes(x = TAST_Status, y = BV_Normalized_time_in_beam, fill = TAST_Status))+
  geom_violin(width = 0.6)+
  geom_jitter(color = "black", alpha = 0.1)+
  labs(x = "TAST Status", 
       y = "Time in Beam (s)", 
       title = "Duration of Seal Presence")+
  theme_cowplot()+
  scale_fill_manual(values = clrblind_pal[3:4])+
  guides(fill = "none")+
  theme(text = element_text(size = 24, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"),
        plot.title = element_text(size = 24, family = "Times New Roman", vjust = 2.0))

# Create the boxplot for non-zero values
ggplot(BV_non_zero_data, aes(x = TAST_Status, y = BV_Normalized_time_in_beam)) +
  geom_boxplot(fill = clrblind_pal[3:4], width = 0.6)+
  labs(x = "TAST Status", y = "Time in Beam (s)", title = "Seal Presence Duration Per Sampling Period")+
  theme_cowplot()+
  guides(fill = "none")+
  theme(text = element_text(size = 18, family = "serif"),
        axis.text = element_text(size = 18, family = "serif"),
        axis.title = element_text(size = 20, family = "serif"),
        plot.title = element_text(size = 25, family = "serif", vjust = 2.0))

# Create the bar chart for # of zero values
BV_fullday %>%
  group_by(TAST_Status) %>%
  summarise(Count_Zero_Values = sum(Cumulative_Time_s == 0)) %>%
  ggplot(aes(x = TAST_Status, y = Count_Zero_Values)) +
  geom_bar(stat = "identity", fill = clrblind_pal[3:4], color = "black", width = 0.6) +
  labs(x = "TAST Status", y = "Number of Zero Values", title = "Number of Zero Values Between Treatments") +
  theme_cowplot() +
  guides(fill = "none") +
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, vjust = 2.0))

# normalize the bar chart and stack it 
BV_proportions <- BV_fullday %>% 
  group_by(TAST_Status) %>% 
  summarise(Count_Zero = sum(Cumulative_Time_s == 0),
            Count_Nonzero = sum(Cumulative_Time_s > 0)) %>% 
  mutate(Total_Count = Count_Zero + Count_Nonzero)

#Calculate proportions
BV_proportions <- BV_proportions %>%
  mutate(Count_Proportion_Zero = Count_Zero / Total_Count,
         Count_Proportion_Nonzero = Count_Nonzero / Total_Count)

# pivot longer for plotting
BV_proportions_long <- BV_proportions %>%
  pivot_longer(cols = c(Count_Proportion_Zero, Count_Proportion_Nonzero),
               names_to = "Value_Type",
               values_to = "Proportion")

# Stacked barplot
ggplot(BV_proportions_long, aes(x = TAST_Status, 
                                y = Proportion, 
                                fill = Value_Type)) +
  geom_bar(stat = "identity", width = 0.6)+
  labs(title = "Proportion of Seal Presence vs. Absence",
       x = "TAST Status",
       y= "Proportion")+
  theme_cowplot()+
  scale_fill_manual(values = clrblind_pal[1:2], 
                    name = NULL, 
                    labels = c("Seal Absence", "Seal Presence"))+
  theme(text = element_text(size = 24, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"),
        plot.title = element_text(size = 24, family = "Times New Roman", vjust = 2.0))

#Same color stacked barplot
ggplot(BV_proportions_long, aes(x = TAST_Status, 
                                y = Proportion, 
                                fill = Value_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Proportion of Seal Presence vs. Absence",
       x = "TAST Status",
       y = "Proportion") +
  theme_cowplot() +
  scale_fill_manual(values = clrblind_pal[1:2], 
                    labels = c("Seal Presence", "Seal Absence")) +
  theme(text = element_text(size = 24, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 0),
        legend.key.size = unit(1.5, "lines"),
        plot.title = element_text(size = 24, family = "Times New Roman", vjust = 2.0))

#Only plotting the count_nonzero data for HCB management meeting
BV_mini <- data.frame(
  TAST_Status = c("ON", "OFF"),
  Count_Nonzero = c(70,194))

ggplot(BV_mini, aes(x = TAST_Status, 
                    y = Count_Nonzero,
                    fill = TAST_Status)) +
  geom_bar(stat = "identity",width = 0.6) +
  labs(x = "TAST Status",
       y = "Number of Seal Observations",
       title = "~60% Reduction in Seal Observations When TAST was ON") +
  theme_cowplot() +
  scale_fill_manual(values = clrblind_pal[3:5], 
                    labels = c("TAST OFF", "TAST ON")) +
  theme(text = element_text(size = 18, family = "serif"),
        axis.text = element_text(size = 18, family = "serif"),
        axis.title = element_text(size = 20, family = "serif"),
        legend.title = element_text(size = 0),
        legend.key.size = unit(1.5, "lines"),
        plot.title = element_text(size = 25, family = "serif", vjust = 2.0))


## 8. Statistics-----------------------------------------------------------------

# checking the normality of the data distribution
# Visual check
ggplot(BV_non_zero_data, aes(x = BV_Normalized_time_in_beam)) +
  geom_histogram() +
  facet_wrap(~TAST_Status)

# Shapiro-Wilk test (if n < 5000)
shapiro.test(BV_fullday$BV_Normalized_time_in_beam[BV_fullday$TAST_Status == "ON"])
shapiro.test(BV_fullday$BV_Normalized_time_in_beam[BV_fullday$TAST_Status == "OFF"])

# Part 1: Did seals show up? (Binary: presence/absence)
BV_fullday <- BV_fullday %>%
  mutate(Seal_Presence = ifelse(TAST_Status == "ON", 1, 0))

# Chi-square or Fisher's exact test
table(BV_fullday$TAST_Status, BV_fullday$Seal_Presence)
chisq.test(BV_fullday$TAST_Status, BV_fullday$Seal_Presence)

# Part 2: When present, how long? (Non-zero values only)
# Mann-Whitney U test on non-zero durations
wilcox.test(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_non_zero_data)

# Compare medians/means when present
BV_non_zero_data %>%
  group_by(TAST_Status) %>%
  summarize(
    median_duration = median(BV_Normalized_time_in_beam),
    mean_duration = mean(BV_Normalized_time_in_beam),
    n = n()
  )

# Tests if ON vs OFF differ in overall distribution (including zeros)
wilcox.test(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_fullday)

## 9. Mixed-Effects Hurdle Model------------------------------------------------

install.packages("glmmTMB")
install.packages("DHARMa")   # diagnostics
install.packages("performance")

library(glmmTMB)
library(DHARMa)
library(performance)
library(dplyr)
library(reformulas)

# Ensure correct types
BV_fullday <- BV_fullday %>%
  mutate(
    Date         = as.factor(Date),
    TAST_Status  = factor(TAST_Status, levels = c("OFF", "ON")),  # OFF = reference
    Seal_Present = as.logical(Seal_Present),
    DateTime     = as.POSIXct(DateTime)
  )

#Check shape of non-zero data because this determines what family to use
#Right-skewed continuous -> truncated_gamma()
#Relatively normal -> truncated_gaussian()
BV_fullday %>%
  filter(BV_Normalized_time_in_beam > 0) %>%
  ggplot(aes(x = BV_Normalized_time_in_beam)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of non-zero residency times")


# Any negative or zero values in non-absent files?
BV_fullday %>%
  filter(Seal_Present == TRUE) %>%
  summarise(
    min_val  = min(BV_Normalized_time_in_beam),
    n_zeros  = sum(BV_Normalized_time_in_beam == 0),
    n_neg    = sum(BV_Normalized_time_in_beam < 0)
  )

detach("package:glmmTMB", unload = TRUE)
library(glmmTMB)

m_hurdle <- glmmTMB(
  BV_Normalized_time_in_beam ~ TAST_Status + offset(log(File_Duration_s)) + (1 | Date),
  ziformula = ~ TAST_Status + (1 | Date),
  family    = glmmTMB::truncated_gamma(link = "log"),
  data      = BV_fullday
)

getNamespaceExports("glmmTMB")

m_hurdle <- glmmTMB(
  BV_Normalized_time_in_beam ~ TAST_Status + offset(log(File_Duration_s)) + (1 | Date),
  ziformula = ~ TAST_Status + (1 | Date),
  family    = lognormal(link = "log"),
  data      = BV_fullday
)

summary(m_hurdle)
