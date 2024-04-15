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
               vegan)

## Set working directory for KK WORK
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

## Set working directory for KK HOME 
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

# 2. Read in Files -------------------------------------------------------------

TAST_ON <- read.csv("TAST_ON_EV_Export_combined.csv")
TAST_OFF <- read.csv("TAST_OFF_EV_Export_combined.csv")

# reformat the dates 
TAST_ON$Date_M <- as.Date(TAST_ON$Date_M, format = "%m/%d/%Y")
TAST_OFF$Date_M <- as.Date(TAST_OFF$Date_M, format = "%m/%d/%Y")

# 3. Combine dfs and filter ----------------------------------------------------

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


# 4. Time Conversions ----------------------------------------------------------

# need to convert the time from UTC to PST
# Combine Date and Time_M columns into a single datetime column in UTC
TAST_combined$DateTime_UTC <- as.POSIXct(paste(TAST_combined$Date_M, TAST_combined$Time_M), tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")

# Convert UTC to PST
TAST_combined$DateTime_PST <- as.POSIXct(TAST_combined$DateTime_UTC, tz = "America/Los_Angeles")

# remove date and just have Time_PST
#TAST_combined$Time_PST <- format(TAST_combined$DateTime_PST, format = "%H:%M:%S")

# round time to the nearest hour
TAST_combined$DateTime_PST_rounded <- hour(TAST_combined$DateTime_PST)
#TAST_combined$DateTime_PST_rounded <- paste0(TAST_combined$DateTime_PST_rounded, ":00")
#TAST_combined$DateTime_PST_rounded <- as.numeric(TAST_combined$DateTime_PST_rounded)

# 5. Plotting ------------------------------------------------------------------

# Time_in_beam by Hour of Day
TAST_combined %>% 
  ggplot(aes(DateTime_PST_rounded, Normalized_time_in_beam, color = TAST_Status))+
  geom_point(aes(color = TAST_Status), size = 2.5, alpha = 0.4)+
  #geom_jitter(aes(color = TAST_Status), size = 2.5, alpha = 0.4)+
  ggtitle("Normalized Seal Time in Beam by Peak Foraging Time Window")+
  scale_color_manual(values = c("ON" = "navy", "OFF" = "tan")) +
  labs(x = "Hour of Day", y = "Normalized Seal Time in Beam (s)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(min(TAST_combined$DateTime_PST_rounded), 
                                  max(TAST_combined$DateTime_PST_rounded)))

# Time_in_beam by DateTime
TAST_combined %>% 
  ggplot(aes(DateTime_PST, Normalized_time_in_beam, color = TAST_Status))+
  geom_point(aes(color = TAST_Status), size = 2.5, alpha = 0.4)+
  ggtitle("Normalized Seal Time in Beam by Peak Foraging Time Window")+
  scale_color_manual(values = c("ON" = "darkgreen", "OFF" = "darkred")) +
  labs(x = "Hour of Day", y = "Normalized Seal Time in Beam (s)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(min(TAST_combined$DateTime_PST_rounded), 
                                  max(TAST_combined$DateTime_PST_rounded)))

#  TEST trying to plot the time axis differently 
TAST_combined %>% 
  ggplot(aes(DateTime_PST, Normalized_time_in_beam, color = TAST_Status))+
  geom_jitter(aes(color = TAST_Status), size = 2.5, alpha = 0.4)+
  ggtitle("Normalized Seal Time in Beam by Peak Foraging Time Window")+
  scale_color_manual(values = c("ON" = "darkgreen", "OFF" = "darkred")) +
  labs(x = "Hour of Day", y = "Normalized Seal Time in Beam (s)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_datetime(date_labels = "%H", date_breaks = "1 hour")
  
time <- hour(TAST_combined$DateTime_PST)
plot(time, TAST_combined$Normalized_time_in_beam)

TAST_combined %>% 
  ggplot(aes(time, Normalized_time_in_beam))+
  geom_point()

# Time_in_beam density plot
TAST_combined %>% 
  ggplot(aes(Time_in_beam, color = TAST_Status))+
  geom_density()+
  ggtitle("Normalized Seal Time in Beam")

# 6. NMDS ----------------------------------------------------------------------
numeric_data <- TAST_combined[sapply(TAST_combined, is.numeric)]
nmds_result <- metaMDS(numeric_data)
nmds_plot <- ordiplot(nmds_result, type = "p")
nmds_plot <- ordiplot(nmds_plot, display = "TAST_Status_numeric")

  
# 7. Correlation Plots ---------------------------------------------------------

# assigning factors and isolating numeric values only 
TAST_cor <- TAST_combined %>% 
  mutate(Status_numeric = case_when(TAST_Status == "ON" ~ 1,
                                    TAST_Status == "OFF" ~ 2))

TAST_cor <- TAST_combined %>% 
  select(where(is.numeric)) %>% 
  drop_na() %>% 
  select(-Process_ID, -Target_depth_mean)

# basic correlation function, using spearman method for categorical variables with assigned factors 
cor(TAST_cor, method = "spearman")

library(wesanderson)  
pal <- wes_palette("Darjeeling2", 21, type = "continuous")

matrix <- cor(TAST_cor) %>% 
  corrplot(addCoef.col = "black", col = COL2("BrBG"), tl.srt = 45, tl.col = "black",
           type = "lower", shade.col = c("blue", "tan"))

# 8. Statistics-----------------------------------------------------------------

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
         loadings.label.size = 3,
         main = "Principal Component Analysis")


# One-way ANOVA
anova_result <- aov(Tortuosity_3D ~ DateTime_PST, data = TAST_combined)
summary(anova_result)

# two sample t-test
t_test <- t.test(Normalized_time_in_beam ~ TAST_Status, data = TAST_combined)
print(t_test)




