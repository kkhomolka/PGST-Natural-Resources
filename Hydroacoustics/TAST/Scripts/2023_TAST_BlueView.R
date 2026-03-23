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
               showtext)

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

# Font aesthetics
font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

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


#this was calculated by dividing total ON time analyzed / total OFF time analyzed
# should only be applied to OFF times to downsample since there are more OFF files
OFF_norm <- 0.89
ON_norm <- 1


# create normalization column for time in beam
BV_fullday <- BV_fullday %>% 
  mutate(BV_Normalized_time_in_beam = if_else(TAST_Status == "ON", 
                                              Cumulative_Time_s * ON_norm, 
                                              Cumulative_Time_s * OFF_norm))


## 5. Create non-zero values for BV normalized and non-normalized --------------

# Filter out non-zero values for boxplot
BV_non_zero_data <- BV_fullday[BV_fullday$Cumulative_Time_s != 0, ]


## 6. BV Plotting ---------------------------------------------------------------

# Violin plot
BV_fullday %>% 
  ggplot(aes(x = TAST_Status, y = Cumulative_Time_s, fill = TAST_Status))+
  geom_violin(width = 0.6)+
  geom_jitter(color = "black", alpha = 0.1)+
  labs(x = "TAST Status", 
       y = "Residency Time (s)")+
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

ggsave("2023_violin.png")


# Create the boxplot for non-zero values
ggplot(BV_non_zero_data, aes(x = TAST_Status, y = Cumulative_Time_s)) +
  geom_boxplot(fill = clrblind_pal[3:4], width = 0.6)+
  labs(x = "TAST Status", y = "Time in Beam (s)", title = "Seal Presence Duration Per Sampling Period")+
  theme_cowplot()+
  guides(fill = "none")+
  theme(text = element_text(size = 48, family = "Times New Roman"),
        axis.text = element_text(size = 36, family = "Times New Roman"),
        axis.title = element_text(size = 48, family = "Times New Roman"),
        plot.title = element_text(size = 48, family = "Times New Roman", vjust = 2.0))

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
       y = "Seal Observations") +
  theme_cowplot() +
  scale_fill_manual(values = clrblind_pal[3:5], 
                    labels = c("TAST OFF", "TAST ON")) +
  theme(text = element_text(size = 48, family = "serif"),
        axis.text = element_text(size = 36, family = "serif"),
        axis.title.x = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 48, family = "Times New Roman",
                                    margin = margin(r = 20)),
        legend.title = element_text(size = 0),
        legend.key.size = unit(1.5, "lines"),
        plot.title = element_text(size = 48, family = "serif", vjust = 2.0))

ggsave("2023_Obs_reduction_barplot.png")

# Aggregating seal time by monitoring period since status switches midday
# Aggregate by date and TAST status period
period_summary <- BV_fullday %>%
  group_by(Date, TAST_Status) %>%
  summarise(
    Cumulative_Time_s     = sum(Cumulative_Time_s),
    Total_Time_Analyzed_s = sum(File_Duration_s),
    .groups = "drop"
  ) %>%
  mutate(
    Date = as.Date(Date),
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

## 8. Statistics-----------------------------------------------------------------

# checking the normality of the data distribution
# Visual check
ggplot(BV_non_zero_data, aes(x = Cumulative_Time_s)) +
  geom_histogram() +
  facet_wrap(~TAST_Status)

# Shapiro-Wilk test (if n < 5000)
shapiro.test(BV_fullday$Cumulative_Time_s[BV_fullday$TAST_Status == "ON"])
shapiro.test(BV_fullday$Cumulative_Time_s[BV_fullday$TAST_Status == "OFF"])

# Part 1: Did seals show up? (Binary: presence/absence)
BV_fullday <- BV_fullday %>%
  mutate(Seal_Presence = ifelse(TAST_Status == "ON", 1, 0))

# Chi-square or Fisher's exact test
table(BV_fullday$TAST_Status, BV_fullday$Seal_Presence)
chisq.test(BV_fullday$TAST_Status, BV_fullday$Seal_Presence)

# Part 2: When present, how long? (Non-zero values only)
# Mann-Whitney U test on non-zero durations
wilcox.test(Cumulative_Time_s ~ TAST_Status, data = BV_non_zero_data)

# Compare medians/means when present
BV_non_zero_data %>%
  group_by(TAST_Status) %>%
  summarize(
    median_duration = median(Cumulative_Time_s),
    mean_duration = mean(Cumulative_Time_s),
    n = n()
  )

# Tests if ON vs OFF differ in overall distribution (including zeros)
wilcox.test(Cumulative_Time_s ~ TAST_Status, data = BV_fullday)

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


# Run the model!
m_hurdle <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status + (1 | Date),
  ziformula = ~ TAST_Status + (1 | Date),
  family    = lognormal(link = "log"),
  data      = BV_fullday)

summary(m_hurdle)

# Sanity Check the Stats--------------------------------------------------------

#Check ACF for residual decay
BV_fullday %>%
  filter(TAST_Status == "OFF") %>%
  arrange(Date) %>%
  pull(Seal_Presence_Rate) %>%
  acf(main = "ACF - TAST OFF periods")

#Going to run DHARMa diagnostics to check if the family used was the right fit
#Simulate residuals (999 simulations is standard)
sim_res <- simulateResiduals(m_hurdle, n = 999)

#Main diagnostic plot (2 panels)
plot(sim_res)

#Test for overdispersion specifically
testDispersion(sim_res)

#Test for zero inflation
testZeroInflation(sim_res)

#Now lets check is another family would have a better fit
#Let's try ziGamma instead of lognormal
m_hurdle_gamma <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status + (1 | Date),
  ziformula = ~ TAST_Status + (1 | Date),
  family    = ziGamma(link = "log"),
  data      = BV_fullday)

summary(m_hurdle_gamma)

#Akaike Information Criterion(AIC) measures how well a model fits your data while
#penalizing for complexity (number of parameters)
AIC(m_hurdle, m_hurdle_gamma)

## Playing with the model-------------------------------------------------------

# Ok, to tackle the date issue we need to group by status instead
BV_fullday <- BV_fullday %>%
  mutate(Period_ID = paste(Date, TAST_Status, sep = "_"))

m_hurdle_period <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status + (1 | Date / Period_ID),
  ziformula = ~ TAST_Status + (1 | Date / Period_ID),
  family = lognormal(link = "log"),
  data = BV_fullday)

summary(m_hurdle_period)

AIC(m_hurdle, m_hurdle_period)

# Looking at model residuals over time
library(forecast)

#Check ACF on your response variable aggregated by day
period_summary %>%
  filter(TAST_Status == "OFF") %>%
  arrange(Date) %>%
  pull(Normalized_Seal_Time) %>%
  acf(main = "ACF - TAST OFF periods")

# NEW SIMPLER MODEL APPROACH
m_period <- glmmTMB(
  Normalized_Seal_Time ~ TAST_Status + (1 | Date),
  family = lognormal(link = "log"),
  data = period_summary)

summary(m_period)

#Simulate residuals (999 simulations is standard)
sim_res_period <- simulateResiduals(m_period, n = 999)

#Main diagnostic plot (2 panels)
plot(sim_res_period)

#Test for overdispersion specifically
testDispersion(sim_res_period)

#Test for zero inflation
testZeroInflation(sim_res_period)

# Check for influential observations - 
# are June 6/7 driving everything?
plotResiduals(sim_res_period, form = period_summary$Date)

# Check that TAST effect looks clean
plotResiduals(sim_res_period, form = period_summary$TAST_Status)

# Simple outlier check
outliers(sim_res_period)

# Add a numeric time index to period_summary
period_summary <- period_summary %>%
  arrange(Date) %>%
  mutate(Time_index = as.numeric(Date - min(Date)))  # days since first sampling day

# Model with temporal trend
m_period3 <- glmmTMB(
  Cumulative_Time_s ~ TAST_Status + Time_index + 
    offset(log(Total_Time_Analyzed_s)) + (1 | Date),
  family = lognormal(link = "log"),
  data = period_summary)

summary(m_period3)

sim_res3 <- simulateResiduals(m_period3, n = 999)
plotResiduals(sim_res3, form = period_summary$Date)

## GLMM MODEL------------------------------------------------------------------

m_GLMM <- glmmTMB(
  Normalized_Seal_Time ~ TAST_Status + (1 | Date),
  family = lognormal(link = "log"),
  data = period_summary)

summary(m_GLMM)


# Run the diagnostics
sim_res_GLMM <- simulateResiduals(m_GLMM, n = 999)
plot(sim_res_GLMM)
testDispersion(sim_res_GLMM)
plotResiduals(sim_res_GLMM, form = period_summary$Date)
plotResiduals(sim_res_GLMM, form = period_summary$TAST_Status)

## Creating AR(1) for Hurdle Model----------------------------------------------

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

m_hurdle_ar1_v2 <- glmmTMB(
  Seal_Presence_Rate ~ TAST_Status +
    (1 | Date) +
    ar1(time_index + 0 | Period_ID),
  ziformula = ~ TAST_Status + (1 | Date),
  family = lognormal(link = "log"),
  data = BV_fullday)

summary(m_hurdle_ar1_v2)

sim_res_ar1_v2 <- simulateResiduals(m_hurdle_ar1_v2, n = 999)
plot(sim_res_ar1_v2)
testDispersion(sim_res_ar1_v2)
plotResiduals(sim_res_ar1_v2, form = BV_fullday$Date)
plotResiduals(sim_res_ar1_v2, form = BV_fullday$TAST_Status)
outliers(sim_res_ar1_v2)

## Plot model outputs-----------------------------------------------------------

install.packages("ggeffects")
library(ggeffects)

# Conditional component - predicted presence rate when seals are detected
pred_conditional <- ggpredict(m_hurdle_ar1_v2, terms = "TAST_Status")

# Zero-inflation component - predicted probability of seal absence
pred_zi <- ggpredict(m_hurdle_ar1_v2, terms = "TAST_Status", type = "zi_prob")

# Quick check
print(pred_conditional)
print(pred_zi)

#Figure 1: Raw data with model predictions overlaid (conditional components)
#Non-zero files only
fig_conditional <- BV_fullday %>%
  filter(Seal_Presence_Rate > 0) %>%  # conditional component = non-zero files only
  ggplot(aes(x = TAST_Status, y = Seal_Presence_Rate, fill = TAST_Status)) +
  geom_violin(width = 0.6, alpha = 0.6) +
  geom_jitter(color = "black", alpha = 0.1, width = 0.1) +
  geom_pointrange(data = data.frame(pred_conditional) %>% rename(TAST_Status = x),
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
  theme(text = element_text(size = 16, family = "Times New Roman"),
        axis.text = element_text(size = 12, family = "Times New Roman"),
        axis.title.x = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(r = 20)))
fig_conditional

ggsave("hurdle_model_violin.png")

#Figure 2: Predicted Probability of seal absence (zero-inflation component)
fig_zi <- data.frame(pred_zi) %>%
  rename(TAST_Status = x) %>%
  ggplot(aes(x = TAST_Status, y = predicted, fill = TAST_Status)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, linewidth = 1) +
  scale_fill_manual(values = clrblind_pal[1:2]) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  guides(fill = "none") +
  labs(x = "TAST Status",
       y = "Predicted Probability\nof Seal Absence") +
  theme_cowplot() +
  theme(text = element_text(size = 16, family = "Times New Roman"),
        axis.text = element_text(size = 12, family = "Times New Roman"),
        axis.title.x = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(r = 20)))

fig_zi

ggsave("hurdle_model_prob.png")

#Outputs for manuscript
# Run these so we have the exact numbers to report
print(pred_conditional)   # predicted presence rates + 95% CI
print(pred_zi)            # predicted absence probabilities + 95% CI

# Back-transform the zi intercept for TAST OFF probability
plogis(1.4519)            # P(absence) TAST OFF
plogis(1.4519 + 1.0262)   # P(absence) TAST ON

# Outlier check
BV_fullday %>%
  slice(620, 621, 741) %>%
  select(Date, TAST_Status, File_Timestamp,
         Seal_Presence_Rate, Cumulative_Time_s,
         File_Duration_s, Period_ID)

# Extract residuals from the model
resid_ar1_v2 <- residuals(m_hurdle_ar1_v2, type = "pearson")

# ACF on full residual series
acf(resid_ar1_v2, main = "ACF - Model Residuals (AR1 Hurdle)")

