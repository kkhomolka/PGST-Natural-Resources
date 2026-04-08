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
               ggformula,
               showtext,
               ggeffects)

# Aesthetics color palette
clrblind_pal <- c(
  "navy",  
  "#0B6ED9",  
  "#78b41f",  
  "#7487ff",  
  "#b41f78"   
)

clrblind_pal_fun <- function(n) {
  if (n > length(clrblind_pal)) stop("Palette only has ", length(clrblind_pal), " colors.")
  clrblind_pal[1:n]}

font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

## Set working directory for KK WORK
setwd("Z:/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

## Set working directory for KK HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

# 2. Reading in & formatting BlueView Time in Beam Files -----------------------

# Reading in file
BV_fullday <- read_excel("spreadsheets/2025_BV_Final.xlsx")

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

# Any bad time durations to be removed?
removed_rows <- BV_fullday %>%
  filter(
    is.na(File_Duration_s) |
      File_Duration_s <= 0 |
      File_Duration_s >= 1000)

View(removed_rows)

#Removing the bad time durations
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


## 5. Normalizing using time analyzed-------------------------------------------

# Need to determine the cumulative time in beam for ON/OFF Status to normalize,
# and also find the average for each status (not counting the zero values)
#BV_cumul <- BV_fullday %>% 
#  group_by(TAST_Status) %>% 
#  summarise(Total_Beam_Time_s = sum(Cumulative_Time_s),
#            Avg_Beam_Time_s = mean(Cumulative_Time_s[Cumulative_Time_s !=0]))

#this was calculated by dividing total OFF time analyzed / total ON time analyzed
# should only be applied to ON times to downsample since there are more ON files
#OFF_norm <- 1
#ON_norm <- 0.598


# create normalization column for time in beam and multiplied by 10^5 to
# improve the readability when plotting 
#BV_fullday <- BV_fullday %>% 
#  mutate(BV_Normalized_time_in_beam = if_else(TAST_Status == "ON", 
#                                              Cumulative_Time_s * ON_norm, 
#                                              Cumulative_Time_s * OFF_norm))



## 6. Create non-zero values for BV normalized and non-normalized --------------

# Filter out non-zero values for boxplot
BV_non_zero_data <- BV_fullday[BV_fullday$Cumulative_Time_s != 0, ]


## 7. BV Plotting ---------------------------------------------------------------

# Violin plot
BV_fullday %>% 
  ggplot(aes(x = TAST_Status, y = Cumulative_Time_s, fill = TAST_Status))+
  geom_violin(width = 0.6)+
  geom_jitter(color = "black", alpha = 0.1)+
  labs(x = "TAST Status", 
       y = "Time in Beam (s)")+
  theme_cowplot()+
  scale_fill_manual(values = clrblind_pal[3:4])+
  guides(fill = "none")+
  theme(text = element_text(size = 48, family = "Times New Roman"),
        axis.text = element_text(size = 36, family = "Times New Roman"),
        axis.title.x = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(t = 25)),
        axis.title.y = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(r = 25)),
        plot.title = element_text(size = 48, family = "Times New Roman", vjust = 2.0))

ggsave("2025_violin.png")

# Create the boxplot for non-zero values
ggplot(BV_non_zero_data, aes(x = TAST_Status, y = Cumulative_Time_s)) +
  geom_boxplot(fill = clrblind_pal[3:4], width = 0.6)+
  labs(x = "TAST Status", y = "Time in Beam (s)")+
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
  labs(x = "TAST Status",
       y= "Proportion")+
  theme_cowplot()+
  scale_fill_manual(values = clrblind_pal[1:2], 
                    name = NULL, 
                    labels = c("Seal Absence", "Seal Presence"))+
  theme(text = element_text(size = 48, family = "Times New Roman"),
        axis.text = element_text(size = 36, family = "Times New Roman"),
        axis.title.x = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(t = 25)),
        axis.title.y = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(r = 25)),
        plot.title = element_text(size = 48, family = "Times New Roman", vjust = 2.0))

ggsave("2025_stacked_bar.png")

#Only plotting the count_nonzero data for HCB management meeting
BV_mini <- data.frame(
  TAST_Status = c("ON", "OFF"),
  Count_Nonzero = c(23,113))

ggplot(BV_mini, aes(x = TAST_Status, 
                    y = Count_Nonzero,
                    fill = TAST_Status)) +
  geom_bar(stat = "identity",width = 0.6) +
  labs(x = "TAST Status",
       y = "Number of Seal Observations") +
  theme_cowplot() +
  scale_fill_manual(values = clrblind_pal[1:2], 
                    labels = c("TAST OFF", "TAST ON")) +
  theme(text = element_text(size = 48, family = "Times New Roman"),
        axis.text = element_text(size = 36, family = "Times New Roman"),
        axis.title.x = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(t = 25)),
        axis.title.y = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(r = 25)),
        legend.position = "none",
        plot.title = element_text(size = 48, family = "Times New Roman", vjust = 2.0))

ggsave("2025_Obs_reduction_barplot.png")

# 7. Aggregating by monitoring period-------------------------------------------

# Aggregating seal time by monitoring period since status switches midday
# Aggregate by date and TAST status period
period_summary <- BV_fullday %>%
  group_by(Date, TAST_Status) %>%
  summarise(
    Cumulative_Time_s     = sum(Cumulative_Time_s),
    Total_Time_Analyzed_s = sum(File_Duration_s),
    .groups = "drop") %>%
  mutate(Date = as.Date(Date),
         Normalized_Seal_Time = Cumulative_Time_s / Total_Time_Analyzed_s)

# Line graph
period_summary %>%
  ggplot(aes(x = Date, y = Normalized_Seal_Time, color = TAST_Status, group = TAST_Status)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 days") +
  scale_color_manual(values = clrblind_pal[3:4]) +
  labs(x = "Date",
       y = "Proportion of Seal Presence",
       color = "TAST Status") +
  theme_cowplot() +
  theme(text = element_text(size = 48, family = "Times New Roman"),
        axis.text = element_text(size = 36, family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(r = 20)),
        legend.text = element_text(size = 36, family = "Times New Roman"),
        legend.title = element_text(size = 36, family = "Times New Roman"))

ggsave("2023_linegraph.png")

# 8. Statistics-----------------------------------------------------------------

# checking the normality of the data distribution
# Visual check
ggplot(BV_non_zero_data, aes(x = Cumulative_Time_s)) +
  geom_histogram() +
  facet_wrap(~TAST_Status)

# Shapiro-Wilk test (if n < 5000)
shapiro.test(BV_fullday$Cumulative_Time_s[BV_fullday$TAST_Status == "ON"])
shapiro.test(BV_fullday$Cumulative_Time_s[BV_fullday$TAST_Status == "OFF"])

# PART 1: Did seals show up? (Binary: presence/absence)
BV_fullday <- BV_fullday %>%
  mutate(Seal_Present = as.integer(Seal_Present))  # TRUE/FALSE to 1/0

# Contingency table of TAST status vs actual seal presence
seal_table <- table(BV_fullday$TAST_Status, BV_fullday$Seal_Present)
print(seal_table)

# Chi-square test
chisq.test(seal_table)

# PART 2: When present, how long? (Non-zero values only)
# Mann-Whitney U test on non-zero durations
wilcox.test(Cumulative_Time_s ~ TAST_Status, data = BV_non_zero_data)

# Compare medians/means when present
BV_non_zero_data %>%
  group_by(TAST_Status) %>%
  summarize(
    median_duration = median(Cumulative_Time_s),
    mean_duration = mean(Cumulative_Time_s),
    n = n())

# Tests if ON vs OFF differ in overall distribution (including zeros)
wilcox.test(Cumulative_Time_s ~ TAST_Status, data = BV_fullday)

# 9. Mixed-Effects Hurdle Model------------------------------------------------

#install.packages("glmmTMB")
#install.packages("DHARMa")   # diagnostics
#install.packages("performance")
#install.packages("tseries")
#install.packages("forecast")

library(glmmTMB)
library(DHARMa)
library(performance)
library(dplyr)
library(reformulas)
library(forecast)

# Ensure correct types
BV_fullday <- BV_fullday %>%
  mutate(
    Date         = as.factor(Date),
    TAST_Status  = factor(TAST_Status, levels = c("OFF", "ON")),  # OFF = reference
    Seal_Present = as.logical(Seal_Present),
    DateTime     = as.POSIXct(DateTime))

#Check shape of non-zero data because this determines what family to use
BV_fullday %>%
  filter(Cumulative_Time_s > 0) %>%
  ggplot(aes(x = Cumulative_Time_s)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of non-zero residency times")


# Any negative or zero values in non-absent files?
BV_fullday %>%
  filter(Seal_Present == TRUE) %>%
  summarise(
    min_val  = min(Cumulative_Time_s),
    n_zeros  = sum(Cumulative_Time_s == 0),
    n_neg    = sum(Cumulative_Time_s < 0))


# Creating AR(1) for Hurdle Model because of temporal residual decay
# Checking everything is in the right format and making period level ID
BV_fullday <- BV_fullday %>%
  arrange(Date, File_Timestamp) %>%
  mutate(
    Period_ID = factor(paste(Date, TAST_Status, sep = "_")),
    TAST_Status = factor(TAST_Status),
    time_index = as.integer(ave(seq_len(nrow(.)), Period_ID, FUN = seq_along)))

# Quick check - make sure it looks right
BV_fullday %>%
  select(Date, TAST_Status, Period_ID, time_index, File_Timestamp) %>%
  head(20)

#Gotta change to be a factor 
BV_fullday <- BV_fullday %>%
  mutate(time_index = factor(time_index))

# ACF within TAST OFF files
BV_fullday %>%
  filter(TAST_Status == "OFF") %>%
  arrange(Date, File_Timestamp) %>%
  pull(Cumulative_Time_s) %>%
  acf(main = "ACF - TAST OFF (file level, raw data)")

# First build period summary
period_summary_2025 <- BV_fullday %>%
  group_by(Date, TAST_Status) %>%
  summarise(
    Seal_Present_count = sum(Seal_Present, na.rm = TRUE),
    Cumulative_Time_s = sum(Cumulative_Time_s, na.rm = TRUE),
    Total_Time_Analyzed_s = sum(File_Duration_s, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    Date = as.Date(Date),
    Normalized_Seal_Time = Cumulative_Time_s / Total_Time_Analyzed_s)

# ACF on period-level normalized seal time for OFF periods
period_summary_2025 %>%
  filter(TAST_Status == "OFF") %>%
  arrange(Date) %>%
  pull(Normalized_Seal_Time) %>%
  acf(main = "ACF - TAST OFF periods (day level)")

# Convert time_index to factor (required for ar1() in glmmTMB)
BV_fullday <- BV_fullday %>%
  mutate(time_index = factor(time_index),
         Period_ID = factor(Period_ID),
         TAST_Status = factor(TAST_Status),
         Date = as.Date(Date))

# Fit the model
m_hurdle_2025_v3 <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status +
    ar1(time_index + 0 | Period_ID),
  ziformula = ~ TAST_Status,
  family = lognormal(link = "log"),
  data = BV_fullday)

summary(m_hurdle_2025_v3)

# Check if the AR(1) variance is the problem - 
# fit without it and compare
m_hurdle_2025_v4 <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status + (1 | Date),
  ziformula = ~ TAST_Status,
  family = lognormal(link = "log"),
  data = BV_fullday)

summary(m_hurdle_2025_v4)

# And a simple no-random-effects version
m_hurdle_2025_v5 <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status,
  ziformula = ~ TAST_Status,
  family = lognormal(link = "log"),
  data = BV_fullday)

summary(m_hurdle_2025_v5)

# Compare all converging models
AIC(m_hurdle_2025_v3, m_hurdle_2025_v4, m_hurdle_2025_v5)

# 10. Trying more models--------------------------------------------------------

m_hurdle <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status,
  ziformula = ~ TAST_Status,
  family = lognormal(link = "log"),
  data = BV_fullday)

summary(m_hurdle)

#DHARMa diagnositics
sim_res_2025 <- simulateResiduals(m_hurdle, n = 999)
plot(sim_res_2025)
testDispersion(sim_res_2025)
plotResiduals(sim_res_2025, form = BV_fullday$Date)
plotResiduals(sim_res_2025, form = BV_fullday$TAST_Status)
outliers(sim_res_2025)

# Add a numeric time index
BV_fullday <- BV_fullday %>%
  mutate(Time_index = as.numeric(as.Date(Date) - min(as.Date(Date))))

# Add temporal trend to the model
m_hurdle_2025_v6 <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status + Time_index,
  ziformula = ~ TAST_Status + Time_index,
  family = lognormal(link = "log"),
  data = BV_fullday
)

summary(m_hurdle_2025_v6)

# Check if it resolves the temporal pattern
sim_res_2025_v6 <- simulateResiduals(m_hurdle_2025_v6, n = 999)
plotResiduals(sim_res_2025_v6, form = as.factor(BV_fullday$Date))

# Compare AIC
AIC(m_hurdle_2025_v5, m_hurdle_2025_v6)

# 11. Final model---------------------------------------------------------------

m_hurdle_woop <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status + (1|Time_index),
  ziformula = ~ TAST_Status + (1|Time_index),
  family = lognormal(link = "log"),
  data = BV_fullday)

summary(m_hurdle_woop)

#DHARMa diagnostics
sim_res_woop <- simulateResiduals(m_hurdle_woop, n = 999)
plot(sim_res_woop)
testDispersion(sim_res_woop)
plotResiduals(sim_res_woop, form = BV_fullday$TAST_Status)
outliers(sim_res_woop)

# Back-transform zi predictions
library(ggeffects)
pred_zi_2025 <- ggpredict(m_hurdle_woop, 
                          terms = "TAST_Status", 
                          type = "zi_prob")
print(pred_zi_2025)

# 12. Figures-------------------------------------------------------------------

# Conditional component - predicted presence rate when seals are detected
pred_conditional_2025 <- ggpredict(m_hurdle_2025, 
                                   terms = "TAST_Status",
                                   type = "fixed")

# Zero-inflation component - predicted probability of seal absence
pred_zi_2025 <- ggpredict(m_hurdle_2025, 
                          terms = "TAST_Status", 
                          type = "zi_prob")

print(pred_conditional_2025)
print(pred_zi_2025)

#Figure 1: Raw data with model predictions overlaid (conditional component)
fig_conditional_2025 <- BV_fullday %>%
  filter(Seal_Presence_Rate > 0) %>%
  ggplot(aes(x = TAST_Status, y = Seal_Presence_Rate, fill = TAST_Status)) +
  geom_violin(width = 0.6, alpha = 0.6) +
  geom_jitter(color = "black", alpha = 0.1, width = 0.1) +
  geom_pointrange(data = data.frame(pred_conditional_2025) %>% 
                    rename(TAST_Status = x),
                  aes(x = TAST_Status,
                      y = predicted,
                      ymin = conf.low,
                      ymax = conf.high),
                  color = "black", size = 1.2, linewidth = 1.2,
                  inherit.aes = FALSE) +
  scale_fill_manual(values = clrblind_pal[1:2]) +
  guides(fill = "none") +
  labs(x = "TAST Status",
       y = "Seal Presence Rate") +
  theme_cowplot() +
  theme(text = element_text(size = 48, family = "Times New Roman"),
        axis.text = element_text(size = 36, family = "Times New Roman"),
        axis.title.x = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(r = 20)))

fig_conditional_2025
ggsave("2025_hurdle_model.png")

# Figure 2: Predicted probability of seal absence (zi component)
fig_zi_2025 <- data.frame(pred_zi_2025) %>%
  rename(TAST_Status = x) %>%
  ggplot(aes(x = TAST_Status, y = predicted, fill = TAST_Status)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, linewidth = 1) +
  scale_fill_manual(values = clrblind_pal[1:2]) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  guides(fill = "none") +
  labs(x = "TAST Status",
       y = "Predicted Probability of Seal Absence") +
  theme_cowplot() +
  theme(text = element_text(size = 48, family = "Times New Roman"),
        axis.text = element_text(size = 36, family = "Times New Roman"),
        axis.title.x = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(r = 20)))

fig_zi_2025
ggsave("2025_hurdle_probability.png")

# Figure 3: Temporal trend 
# Option 1: use Time_index (numeric) instead of time_index (factor)
pred_time_2025 <- ggpredict(m_hurdle_2025,
                            terms = "time_index [0:9 by=0.1]",
                            type = "zi_prob")

fig_temporal_2025 <- data.frame(pred_time_2025) %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "gray80", alpha = 0.6) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = 0:9,
                     labels = format(seq(as.Date("2025-05-31"),
                                         as.Date("2025-06-09"),
                                         by = "day"), "%b %d")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "Date",
       y = "Predicted Probability\nof Seal Absence") +
  theme_bw() +
  theme(text = element_text(size = 24, family = "Times New Roman"),
        axis.text = element_text(size = 18, family = "Times New Roman"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 24, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 24, family = "Times New Roman",
                                    margin = margin(r = 20)))


fig_temporal_2025
