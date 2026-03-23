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


## 2. Read in EV Files ---------------------------------------------------------

TAST_ON <- read.csv("spreadsheets/TAST_ON_EV_Export_fullday.csv")
TAST_OFF <- read.csv("spreadsheets/TAST_OFF_EV_Export_fullday.csv")

## 3. Combine dfs and filter ----------------------------------------------------

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

# create normalization factor
#ON_norm <- 1
#OFF_norm <- 0.89

# create normalization column for time in beam
#TAST_combined <- TAST_combined %>% 
#  mutate(Normalized_time_in_beam = if_else(TAST_Status == "ON", 
#                                           Time_in_beam * ON_norm, 
#                                           Time_in_beam * OFF_norm))

# assign numeric factors for TAST status
TAST_combined <- TAST_combined %>% 
  mutate(TAST_Status_numeric = case_when(
    TAST_Status == "ON" ~ 1,
    TAST_Status == "OFF" ~ 0,
    TRUE ~ NA_integer_))


## 4. Time Conversions ----------------------------------------------------------

# Combine "Date_M" and "Time_M" columns into a single datetime string
datetime_string <- paste(TAST_combined$Date_M, TAST_combined$Time_M)

# Convert datetime string to POSIXct format in UTC
TAST_combined$DateTime_POSIXct <- as.POSIXct(datetime_string, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")

# Convert to PST time zone
TAST_combined$DateTime_PST <- with_tz(TAST_combined$DateTime_POSIXct, "America/Los_Angeles")

# remove date and just have Time_PST, FYI this will make the object a character class
TAST_combined$Time_PST <- format(TAST_combined$DateTime_PST, format = "%H:%M:%S")

## 11. EV Plotting ---------------------------------------------------------------

# Time_in_beam by Hour of Day
TAST_combined %>%
  mutate(Time_of_day = hour(DateTime_PST)) %>%  # Extract hour component
  ggplot(aes(Time_of_day, Time_in_beam, color = TAST_Status)) +
  geom_point(size = 2.5, alpha = 0.4) +
  scale_color_manual(values = clrblind_pal[4:5]) +
  labs(x = "Hour of Day", y = "Residency Time (s)") +
  theme_cowplot() +
  theme(text = element_text(size = 16, family = "Times New Roman"),
             axis.text = element_text(size = 12, family = "Times New Roman"),
             axis.title.x = element_text(size = 16, family = "Times New Roman",
                                         margin = margin(t = 20)),
             axis.title.y = element_text(size = 16, family = "Times New Roman",
                                         margin = margin(r = 20)),
             legend.text = element_text(size = 12, family = "Times New Roman"),
             legend.title = element_text(size = 12, family = "Times New Roman"))

ggsave("2023_EV_scatter.png")

# Tortuosity by Hour of Day
TAST_combined %>%
  mutate(Time_of_day = hour(DateTime_PST)) %>%  # Extract hour component
  ggplot(aes(Time_of_day, Tortuosity_3D, color = TAST_Status)) +
  geom_point(size = 2.5, alpha = 0.4) +
  scale_color_manual(values = clrblind_pal[4:5]) +
  labs(x = "Hour of Day", y = "3-Dimensonal Tortuosity") +
  theme_cowplot() +
  theme(text = element_text(size = 16, family = "Times New Roman"),
        axis.text = element_text(size = 12, family = "Times New Roman"),
        axis.title.x = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(r = 20)),
        legend.text = element_text(size = 12, family = "Times New Roman"),
        legend.title = element_text(size = 12, family = "Times New Roman"))

ggsave("2023_EV_tortuosity.png")

# Box plots - Tortuosity
TAST_combined %>% ggplot(aes(TAST_Status, Tortuosity_3D, fill = TAST_Status))+
  geom_boxplot()+
  ggtitle("3-Dimensional Tortuosity")+
  labs(x = "TAST Status", y = "3-Dimensional Tortuosity")+
  scale_fill_manual(values = wes_palette("AsteroidCity1", 2))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Box plots - Time in Beam 
BV_fullday %>% ggplot(aes(TAST_Status, BV_Normalized_time_in_beam, fill = TAST_Status))+
  geom_boxplot()+
  ggtitle("Normalized Time in Beam from BlueView Analysis")+
  labs(x = "TAST Status", y = "Normalized Time in Beam (s)")+
  scale_fill_manual(values = wes_palette("AsteroidCity1", 2))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Linear relationships - Tortuosity
ggplot(TAST_combined, aes(Time_PST, Tortuosity_3D))+
  geom_point()+
  facet_wrap(~TAST_Status)

## 12. NMDS ----------------------------------------------------------------------
numeric_data <- TAST_combined[sapply(TAST_combined, is.numeric)]
nmds_result <- metaMDS(numeric_data)
nmds_plot <- ordiplot(nmds_result, type = "p")
nmds_plot <- ordiplot(nmds_plot, display = "TAST_Status_numeric")


## 13. Correlation Plots ---------------------------------------------------------

#creating gradient for matrix
clrblind_gradient <- colorRampPalette(clrblind_pal)(200)

# assigning factors and isolating numeric values only 
TAST_cor <- TAST_combined %>% 
  mutate(Status_numeric = case_when(TAST_Status == "ON" ~ 1,
                                    TAST_Status == "OFF" ~ 0))

TAST_cor <- TAST_combined %>% 
  select(where(is.numeric)) %>% 
  drop_na() %>% 
  select(-Process_ID, -Target_depth_mean, -Time_in_beam)

# basic correlation function, using spearman method for categorical variables with assigned factors 
corrplot(cor(TAST_cor, method = "spearman"), 
         method = "circle",                   # << this is the key part
         col = clrblind_gradient,             # gradient color fill
         addCoef.col = "black",               # add correlation values
         tl.srt = 20,                         # text label angle
         tl.col = "black",                    # text color
         type = "lower",                      # only show lower triangle
         shade.col = NA)      


## 14. Statistics-----------------------------------------------------------------

# Principal Component Analysis (make sure 'stats' package is loaded)
pca_result <- prcomp(TAST_combined[,c("Target_range_mean",
                                      "Time_in_beam",
                                      "TAST_Status_numeric",
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
         loadings.label.size = 4,
         loadings.label.vjust = -1,
         loadings.label.hjust = 0.45)+
  scale_color_manual(values = clrblind_pal[4:5])+ #remove guide = "none" if you want to have a legend
  labs(color = "TAST Status")+
  theme_cowplot()+
  theme(text = element_text(size = 16, family = "Times New Roman"),
        axis.text = element_text(size = 12, family = "Times New Roman"),
        axis.title.x = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, family = "Times New Roman",
                                    margin = margin(r = 20)),
        legend.text = element_text(size = 12, family = "Times New Roman"),
        legend.title = element_text(size = 12, family = "Times New Roman"))

ggsave("2023_EV_pca.png")


# One-way ANOVA
anova_result <- aov(Time_in_beam ~ TAST_Status, data = TAST_combined)
summary(anova_result)

# two sample t-test
t_test <- t.test(Time_in_beam ~ TAST_Status, data = TAST_combined)
print(t_test)

#two sample t-test for BV time in beam 
t_test2 <- t.test(Cumulative_Time_s ~ TAST_Status, data = BV_fullday)
print(t_test2)

anova_result2 <- aov(Cumulative_Time_s ~ TAST_Status, data = BV_fullday)
summary(anova_result2)

t_test3 <- t.test(Target_range_mean ~ TAST_Status, data = TAST_combined)
print(t_test3)

anova_result3 <- aov(Tortuosity_3D ~ TAST_Status, data = TAST_combined)
summary(anova_result3)

t_test4 <- t.test(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_non_zero_data)
print(t_test4)

anova_result4 <- aov(BV_Normalized_time_in_beam ~ TAST_Status, data = BV_non_zero_data)
summary(anova_result4)

t_test5 <- t.test(Tortuosity_3D ~ TAST_Status, data = TAST_combined)
print(t_test5)
