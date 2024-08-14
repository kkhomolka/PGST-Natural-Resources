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
               ggrepel)

# Aesthetics color palette
pal <- wes_palette("AsteroidCity1", 2, type = "continuous")

## Set working directory for KK WORK
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

## Set working directory for KK HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

# 2. Reading in & formatting BlueView Time in Beam Files -----------------------

# Reading in file
BV_fullday <- read_excel("TAST_full_day_and_foraging_window_seal_presence.xlsx")

BV_fullday <- BV_fullday %>% 
  select(File,
         File_Timestamp,
         Date,
         Cumulative_Time_s,
         TAST_Status)

#Need to fix the date
BV_fullday$Date <- as.Date(BV_fullday$Date)

time_part <- format(BV_fullday$File_Timestamp, format = "%H:%M:%S")

BV_fullday$DateTime <- as.POSIXct(paste(BV_fullday$Date, time_part), format = "%Y-%m-%d %H:%M:%S")

#Need to determine the cumulative time in beam for ON/OFF Status to determine
#the normalization factors for both statuses

BV_cumul <- BV_fullday %>% 
  group_by(TAST_Status) %>% 
  summarise(Total_Beam_Time_s = sum(Cumulative_Time_s))

#Need to determine the total time of each treatment duration
BV_fullday <- BV_fullday %>% 
  arrange(DateTime)

times <- as.difftime(BV_fullday$File_Timestamp, format = "%H:%M:%S")

time_diffs <- BV_fullday %>%
  arrange(DateTime) %>%
  group_by(TAST_Status) %>%
  mutate(Time_Diff = lead(DateTime) - DateTime) %>%
  filter(!is.na(Time_Diff)) %>%
  summarize(Total_Hours = sum(as.numeric(Time_Diff, units = "hours")))

# use same normalization variables as EV files
ON_norm <- 154200
OFF_norm <- 147060

# create normalization column for time in beam and multiplied by 10^5 to
# improve the readability when plotting 
BV_fullday <- BV_fullday %>% 
  mutate(BV_Normalized_time_in_beam = if_else(TAST_Status == "ON", 
                                              Cumulative_Time_s / ON_norm * 10^5, 
                                              Cumulative_Time_s / OFF_norm * 10^5))

# remove na's
BV_fullday <- na.omit(BV_fullday)

# Idenitfying and removing outliers 
# Calculate the first quartile (Q1) and third quartile (Q3)
Q1 <- quantile(BV_fullday$BV_Normalized_time_in_beam, 0.25)
Q3 <- quantile(BV_fullday$BV_Normalized_time_in_beam, 0.75)

# Calculate the interquartile range (IQR)
IQR <- Q3 - Q1

# Define the lower and upper bounds for identifying outliers
lower_bound <- Q1 - 3.0 * IQR
upper_bound <- Q3 + 3.0 * IQR

# Identify outliers
outliers <- BV_fullday$BV_Normalized_time_in_beam < lower_bound | 
  BV_fullday$BV_Normalized_time_in_beam > upper_bound

# Display the identified outliers
outlier_values <- BV_fullday[outliers, "BV_Normalized_time_in_beam"]
print(outlier_values)

# Identify the top 3 outliers
top_outliers <- head(sort(BV_fullday$BV_Normalized_time_in_beam[outliers], 
                          decreasing = TRUE), 3)

# Remove the top 3 outliers from the dataframe
BV_fullday <- BV_fullday[!BV_fullday$BV_Normalized_time_in_beam %in% top_outliers, ]

# Filter out non-zero values for boxplot
BV_non_zero_data <- BV_fullday[BV_fullday $BV_Normalized_time_in_beam != 0, ]

# 3. Read in Files -------------------------------------------------------------

TAST_ON <- read.csv("TAST_ON_EV_Export_fullday.csv")
TAST_OFF <- read.csv("TAST_OFF_EV_Export_fullday.csv")

# 4. Combine dfs and filter ----------------------------------------------------

TAST_combined <- rbind(TAST_OFF, TAST_ON)

TAST_combined <- TAST_combined %>% 
  select(Process_ID,
         Date_M,
         Time_M,
         TAST_Status,
         Target_depth_mean, 
         Target_range_mean, 
         Fish_track_change_in_range,
         Speed_4D_mean_unsmoothed,
         Tortuosity_3D,
         Time_in_beam)

# create normalization variables
ON_norm <- 154200
OFF_norm <- 147060

# create normalization column for time in beam and multiplied by 10^5 to
# improve the readability when plotting 
TAST_combined <- TAST_combined %>% 
  mutate(Normalized_time_in_beam = if_else(TAST_Status == "ON", 
                                           Time_in_beam / ON_norm * 10^5, 
                                           Time_in_beam / OFF_norm * 10^5))

# assign numeric factors for TAST status
TAST_combined <- TAST_combined %>% 
  mutate(TAST_Status_numeric = case_when(
    TAST_Status == "ON" ~ 1,
    TAST_Status == "OFF" ~ 0,
    TRUE ~ NA_integer_))


# 5. Time Conversions ----------------------------------------------------------

# Combine "Date_M" and "Time_M" columns into a single datetime string
datetime_string <- paste(TAST_combined$Date_M, TAST_combined$Time_M)

# Convert datetime string to POSIXct format in UTC
TAST_combined$DateTime_POSIXct <- as.POSIXct(datetime_string, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")

# Convert to PST time zone
TAST_combined$DateTime_PST <- with_tz(TAST_combined$DateTime_POSIXct, "America/Los_Angeles")

# Print to check to make sure that the object class is "POSIXct"
#print(class(TAST_combined$DateTime_PST))

# remove date and just have Time_PST, FYI this will make the object a character class
TAST_combined$Time_PST <- format(TAST_combined$DateTime_PST, format = "%H:%M:%S")

# adding an arbitary date to Time_PST so I can plot everything on the same day
date <- "2024-04-15"
TAST_combined$DateTime_PST_Plotting <- paste(date, TAST_combined$Time_PST, sep = " ")
TAST_combined$DateTime_PST_Plotting <- as.POSIXct(TAST_combined$DateTime_PST_Plotting, format = "%Y-%m-%d %H:%M:%S")

# 6. BV Plotting ---------------------------------------------------------------

# Violin plot
BV_combined %>% 
  ggplot(aes(x = TAST_Status, y = BV_Normalized_time_in_beam, fill = TAST_Status))+
  geom_violin(width = 0.6)+
  geom_jitter(color = "black", alpha = 0.1)+
  labs(x = "TAST Status", y = "Normalized Time in Beam (s)", title = "Duration of Seal Presence")+
  theme_cowplot()+
  scale_fill_manual(values = wes_palette("AsteroidCity1")[3:4])+
  guides(fill = "none")+
  theme(text = element_text(size = 18, family = "Calibri"),
        axis.text = element_text(size = 18, family = "Calibri"),
        axis.title = element_text(size = 20, family = "Calibri"),
        plot.title = element_text(size = 25, family = "Calibri", vjust = 2.0))

# messing with aesthetics for violin plot 
BV_combined %>% 
  ggplot(aes(x = TAST_Status, y = BV_Normalized_time_in_beam, fill = TAST_Status)) +
  geom_violin() +
  geom_jitter(aes(color = TAST_Status), alpha = 0.2) +  # Use the same fill aesthetic mapping and set transparency to 10%
  geom_jitter(aes(color = TAST_Status), shape = 1, alpha = 0.5) +  # Outline points with shape = 1 (circle) and set transparency to 10%
  labs(x = "TAST Status", y = "Normalized Time in Beam (s)", title = "Duration of Seal Presence") +
  theme_cowplot() +
  scale_fill_manual(values = wes_palette("AsteroidCity1", 2)) +
  scale_color_manual(values = wes_palette("AsteroidCity1", 2)) +  # Match jitter point colors with violin plot colors
  guides(fill = "none", color = "none") +  # Remove legends for fill and color aesthetics
  ggplot(BV_non_zero_data, aes(x = TAST_Status, y = BV_Normalized_time_in_beam)) +
  geom_boxplot(fill = wes_palette("AsteroidCity1", 2), width = 0.6)+
  labs(x = "TAST Status", y = "Normalized Time in Beam (s)", title = "Seal Presence Duration Per Sampling Period")+
  theme_cowplot()+
  guides(fill = "none")+
  theme(text = element_text(size = 18, family = "Calibri"),
        axis.text = element_text(size = 18, family = "Calibri"),
        axis.title = element_text(size = 20, family = "Calibri"),
        plot.title = element_text(size = 25, family = "Calibri", vjust = 2.0))


# Create the boxplot for non-zero values
ggplot(BV_non_zero_data, aes(x = TAST_Status, y = BV_Normalized_time_in_beam)) +
  geom_boxplot(fill = wes_palette("AsteroidCity1")[3:4], width = 0.6)+
  labs(x = "TAST Status", y = "Normalized Time in Beam (s)", title = "Seal Presence Duration Per Sampling Period")+
  theme_cowplot()+
  guides(fill = "none")+
  theme(text = element_text(size = 18, family = "Calibri"),
        axis.text = element_text(size = 18, family = "Calibri"),
        axis.title = element_text(size = 20, family = "Calibri"),
        plot.title = element_text(size = 25, family = "Calibri", vjust = 2.0))

# Create the bar chart for # of zero values
BV_combined %>%
  group_by(TAST_Status) %>%
  summarise(Count_Zero_Values = sum(BV_Normalized_time_in_beam == 0)) %>%
  ggplot(aes(x = TAST_Status, y = Count_Zero_Values)) +
  geom_bar(stat = "identity", fill = wes_palette("AsteroidCity1", 2), color = "black", width = 0.6) +
  labs(x = "TAST Status", y = "Number of Zero Values", title = "Number of Zero Values Between Treatments") +
  theme_cowplot() +
  guides(fill = "none") +
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, vjust = 2.0))

# normalize the bar chart and stack it 
BV_proportions <- BV_combined %>% 
  group_by(TAST_Status) %>% 
  summarise(Count_Zero = sum(BV_Normalized_time_in_beam == 0),
            Count_Nonzero = sum(BV_Normalized_time_in_beam > 0)) %>% 
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
  geom_bar(stat = "identity", fill = wes_palette("AsteroidCity1", 4), color = "black", width = 0.6)+
  labs(title = "Proportion of Seal Presence vs. Absence",
       x = "TAST Status",
       y= "Proportion")+
  theme_cowplot()+
  scale_fill_manual(values = wes_palette("AsteroidCity1")[1:4], 
                    name = NULL, 
                    labels = c("Seal Absence", "Seal Presence"))+
  theme(text = element_text(size = 18, family = "Calibri"),
        axis.text = element_text(size = 18, family = "Calibri"),
        axis.title = element_text(size = 20, family = "Calibri"),
        plot.title = element_text(size = 25, family = "Calibri", vjust = 2.0))

ggplot(BV_proportions_long, aes(x = TAST_Status, 
                                y = Proportion, 
                                fill = Value_Type)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  labs(title = "Proportion of Seal Presence vs. Absence",
       x = "TAST Status",
       y = "Proportion") +
  theme_cowplot() +
  scale_fill_manual(values = wes_palette("AsteroidCity1")[1:4], 
                    labels = c("Seal Presence", "Seal Absence")) +
  theme(text = element_text(size = 18, family = "Calibri"),
        axis.text = element_text(size = 18, family = "Calibri"),
        axis.title = element_text(size = 20, family = "Calibri"),
        legend.title = element_text(size = 0),
        legend.key.size = unit(1.5, "lines"),
        plot.title = element_text(size = 25, family = "Calibri", vjust = 2.0))


# 7. EV Plotting ---------------------------------------------------------------

# Time_in_beam by Hour of Day
TAST_combined %>%
  mutate(Time_of_day = hour(DateTime_PST_Plotting)) %>%  # Extract hour component
  ggplot(aes(Time_of_day, Normalized_time_in_beam, color = TAST_Status)) +
  geom_point(size = 2.5, alpha = 0.4) +
  ggtitle("Normalized Seal Time in Beam by Peak Foraging Time Window") +
  scale_color_manual(values = c("ON" = "navy", "OFF" = "tan")) +
  labs(x = "Hour of Day", y = "Normalized Seal Time in Beam (s)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


# Time_in_beam by Hour of Day
TAST_combined %>%
  mutate(Time_of_day = hour(DateTime_PST_Plotting)) %>%  # Extract hour component
  ggplot(aes(Time_of_day, Tortuosity_3D, color = TAST_Status)) +
  geom_point(size = 2.5, alpha = 0.4) +
  ggtitle("Tortuosity by Peak Foraging Time Window") +
  scale_color_manual(values = c("ON" = "navy", "OFF" = "tan")) +
  labs(x = "Hour of Day", y = "3-Dimensonal Tortuosity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# Time_in_beam density plot
TAST_combined %>% 
  ggplot(aes(Time_in_beam, color = TAST_Status))+
  geom_density()+
  ggtitle("Normalized Seal Time in Beam")

# Box plots - Tortuosity
TAST_combined %>% ggplot(aes(TAST_Status, Tortuosity_3D, fill = TAST_Status))+
  geom_boxplot()+
  ggtitle("3-Dimensional Tortuosity")+
  labs(x = "TAST Status", y = "3-Dimensional Tortuosity")+
  scale_fill_manual(values = wes_palette("AsteroidCity1", 2))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Box plots - Time in Beam 
BV_combined %>% ggplot(aes(TAST_Status, BV_Normalized_time_in_beam, fill = TAST_Status))+
  geom_boxplot()+
  ggtitle("Normalized Time in Beam from BlueView Analysis")+
  labs(x = "TAST Status", y = "Normalized Time in Beam (s)")+
  scale_fill_manual(values = wes_palette("AsteroidCity1", 2))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Linear relationships - Tortuosity
ggplot(TAST_combined, aes(DateTime_PST_Plotting, Tortuosity_3D))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  facet_wrap(~TAST_Status)

# 8. NMDS ----------------------------------------------------------------------
numeric_data <- TAST_combined[sapply(TAST_combined, is.numeric)]
nmds_result <- metaMDS(numeric_data)
nmds_plot <- ordiplot(nmds_result, type = "p")
nmds_plot <- ordiplot(nmds_plot, display = "TAST_Status_numeric")


# 9. Correlation Plots ---------------------------------------------------------

# assigning factors and isolating numeric values only 
TAST_cor <- TAST_combined %>% 
  mutate(Status_numeric = case_when(TAST_Status == "ON" ~ 1,
                                    TAST_Status == "OFF" ~ 0))

TAST_cor <- TAST_combined %>% 
  select(where(is.numeric)) %>% 
  drop_na() %>% 
  select(-Process_ID, -Target_depth_mean, -Time_in_beam)

# basic correlation function, using spearman method for categorical variables with assigned factors 
cor(TAST_cor, method = "spearman")

matrix <- cor(TAST_cor) %>% 
  corrplot(addCoef.col = "black", col = COL2("BrBG"), tl.srt = 20, tl.col = "black",
           type = "lower", shade.col = wes_palette("AsteroidCity1"))


# 9. Statistics-----------------------------------------------------------------

# Principal Component Analysis (make sure 'stats' package is loaded)
pca_result <- prcomp(TAST_combined[,c("Target_range_mean",
                                      "Normalized_time_in_beam",
                                      #"TAST_Status_numeric",
                                      "Tortuosity_3D",
                                      "Fish_track_change_in_range",
                                      "Speed_4D_mean_unsmoothed")],
                     scale. = TRUE)
# extract principal components
pc_scores <- pca_result$x

# trying ggplot method
autoplot(pca_result, 
         data = TAST_combined, 
         color = "TAST_Status",
         loadings = TRUE,
         loadings.colour = "black",
         loadings.label = TRUE,
         loadings.label.colour = "black",
         loadings.label.size = 5,
         loadings.label.vjust = -0.2,
         loadings.label.hjust = -0.01,
         main = "Principal Component Analysis")+
  scale_color_manual(values = pal, guide = "none")+ #remove guide = "none" if you want to have a legend
  theme_cowplot()+
  theme(text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, vjust = 2.0))




# One-way ANOVA
anova_result <- aov(Normalized_time_in_beam ~ TAST_Status, data = TAST_combined)
summary(anova_result)

# two sample t-test
t_test <- t.test(Normalized_time_in_beam ~ TAST_Status, data = TAST_combined)
print(t_test)

#two sample t-test for BV time in beam 
t_test2 <- t.test(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_combined)
print(t_test2)

anova_result2 <- aov(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_combined)
summary(anova_result2)

t_test3 <- t.test(Target_range_mean ~ TAST_Status, data = TAST_combined)
print(t_test3)

anova_result3 <- aov(Tortuosity_3D ~ TAST_Status, data = TAST_combined)
summary(anova_result3)

t_test4 <- t.test(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_non_zero_data)
print(t_test4)

anova_result4 <- aov(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_non_zero_data)
summary(anova_result4)