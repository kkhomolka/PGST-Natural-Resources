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

## EXPERIMENT-------------------------------------------------------------------

# Find the minimum sample size
min_n <- BV_fullday %>%
  count(TAST_Status) %>%
  pull(n) %>%
  min()

# Subsample each group to that size
BV_balanced <- BV_fullday %>%
  group_by(TAST_Status) %>%
  slice_sample(n = min_n) %>%
  ungroup()

#Sanity check that they are the same
BV_balanced %>% count(TAST_Status)


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

# Violin plot
BV_fullday %>% 
  ggplot(aes(x = TAST_Status, y = BV_Normalized_time_in_beam, fill = TAST_Status))+
  geom_violin(width = 0.6)+
  geom_jitter(color = "black", alpha = 0.1)+
  labs(x = "TAST Status", 
       y = "Normalized Time in Beam (s)", 
       title = "Duration of Seal Presence")+
  theme_cowplot()+
  scale_fill_manual(values = clrblind_pal[3:4])+
  guides(fill = "none")+
  theme(text = element_text(size = 18, family = "serif"),
        axis.text = element_text(size = 18, family = "serif"),
        axis.title = element_text(size = 20, family = "serif"),
        plot.title = element_text(size = 25, family = "serif", vjust = 2.0))

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
  geom_bar(stat = "identity", fill = clrblind_pal[2:5], color = "black", width = 0.6)+
  labs(title = "Proportion of Seal Presence vs. Absence",
       x = "TAST Status",
       y= "Proportion")+
  theme_cowplot()+
  scale_fill_manual(values = wes_palette("AsteroidCity1")[1:4], 
                    name = NULL, 
                    labels = c("Seal Absence", "Seal Presence"))+
  theme(text = element_text(size = 18, family = "serif"),
        axis.text = element_text(size = 18, family = "serif"),
        axis.title = element_text(size = 20, family = "serif"),
        plot.title = element_text(size = 25, family = "serif", vjust = 2.0))

#Same color stacked barplot
ggplot(BV_proportions_long, aes(x = TAST_Status, 
                                y = Proportion, 
                                fill = Value_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Proportion of Seal Presence vs. Absence",
       x = "TAST Status",
       y = "Proportion") +
  theme_cowplot() +
  scale_fill_manual(values = clrblind_pal[3:4], 
                    labels = c("Seal Presence", "Seal Absence")) +
  theme(text = element_text(size = 18, family = "serif"),
        axis.text = element_text(size = 18, family = "serif"),
        axis.title = element_text(size = 20, family = "serif"),
        legend.title = element_text(size = 0),
        legend.key.size = unit(1.5, "lines"),
        plot.title = element_text(size = 25, family = "serif", vjust = 2.0))

#Only plotting the count_nonzero data for HCB management meeting
BV_mini <- data.frame(
  TAST_Status = c("ON", "OFF"),
  Count_Nonzero = c(75,192))

ggplot(BV_mini, aes(x = TAST_Status, 
                    y = Count_Nonzero,
                    fill = TAST_Status)) +
  geom_bar(stat = "identity",width = 0.6) +
  labs(x = "TAST Status",
       y = "Number of Seal Observations",
       title = "~60% Reduction in Seal Observations When TAST was ON") +
  theme_cowplot() +
  scale_fill_manual(values = clrblind_pal[3:4], 
                    labels = c("TAST OFF", "TAST ON")) +
  theme(text = element_text(size = 18, family = "serif"),
        axis.text = element_text(size = 18, family = "serif"),
        axis.title = element_text(size = 20, family = "serif"),
        legend.title = element_text(size = 0),
        legend.key.size = unit(1.5, "lines"),
        plot.title = element_text(size = 25, family = "serif", vjust = 2.0))


# Bin cumulative time in beam by hour of the day and then plot 
# Extract the hour from the DateTime_PST column and sum Time_in_beam for each hour
hourly_sum <- TAST_combined %>%
  mutate(hour = hour(DateTime_PST)) %>%
  group_by(TAST_Status, hour) %>%                     
  summarise(total_time_in_beam = sum(Time_in_beam, na.rm = TRUE)) %>% 
  ungroup()

ggplot(hourly_sum, aes(x = hour, y = total_time_in_beam, fill = TAST_Status)) +
  geom_col() +
  scale_x_continuous(breaks = 0:23) +  
  labs(
    x = "Hour of Day",
    y = "Cumulative Time in Beam (s)",
    title = "Cumulative Time in Beam for Each Hour of the Day",
    fill = "TAST Status")+
  theme_classic(base_size = 15)+
  theme(axis.text = element_text(color = "black"))+
  scale_fill_manual(values = clrblind_pal[3:4])

#Sum of BV cumulative time in beam, and then finding the average to plot
ggplot(BV_cumul, aes(x = TAST_Status, y = Avg_Beam_Time_s, fill = TAST_Status)) +
  geom_bar(stat = "identity") +
  labs(
    x = "TAST Status",
    y = "Average Time in Beam (s)",
    title = "~30% Reduction in Time Spent by Seaks in the Study Area",
    fill = "TAST Status")+
  theme_classic(base_size = 15)+
  theme(axis.text = element_text(color = "black"))+
  scale_fill_manual(values = clrblind_pal[3:4])

## 8. Statistics-----------------------------------------------------------------

#two sample t-test for BV time in beam 
t_test2 <- t.test(Cumulative_Time_s ~ TAST_Status, data = BV_fullday)
print(t_test2)

anova_result2 <- aov(Cumulative_Time_s ~ TAST_Status, data = BV_fullday)
summary(anova_result2)

t_test4 <- t.test(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_non_zero_data)
print(t_test4)

anova_result4 <- aov(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_non_zero_data)
summary(anova_result4)

