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
               showtext,
               ggeffects,
               glmmTMB,
               DHARMa,
               emmeans,
               permute,
               gt)

font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#Use for windows operating systems
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

# 2. Read in files--------------------------------------------------------------
df_0329 <- read_csv("dtx_exports/20240329.csv")
df_0405 <- read_csv("dtx_exports/20240405.csv")
df_0412 <- read_csv("dtx_exports/20240412.csv")
df_0419 <- read_csv("dtx_exports/20240419.csv")
df_0426 <- read_csv("dtx_exports/20240426.csv")
#df_0524 <- read_csv("dtx_exports/20240524.csv")

# Select the filenames I want to keep
keep_files <- c("20240329_140635.dt4", "20240405_122218.dt4", "20240412_103611.dt4",
                "20240419_111531.dt4", "20240426_111537.dt4") #"20240524_193424.dt4"

# Bind the rows together into one df
df<- bind_rows(df_0329, df_0405, df_0412, df_0419, df_0426) %>% 
  filter(FileName %in% keep_files) %>%
  mutate(survey_date = ymd(str_sub(FileName, 1, 8)))

#Reformatting the dates
time_part <- format(df$Time, format = "%H:%M:%S")
df <- df %>% mutate(DateTime = as.POSIXct(paste(df$Time, time_part), format = "%Y-%m-%d %H:%M:%S"))

# 3. Simple Stats---------------------------------------------------------------
height_summary <- df %>%
  group_by(survey_date) %>%
  summarise(
    n_pings     = n(),
    n_veg       = sum(PlantHeight_m > 0, na.rm = TRUE),
    pct_veg     = 100 * n_veg / n_pings,
    mean_height = mean(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    sd_height   = sd(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    min_height  = min(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    max_height  = max(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE))

# Creating a df with the zero pings removed, only vegetated pings remain
df_veg <- df %>% filter(PlantHeight_m > 0)

# boxplot of plant height over time
height_box <- ggplot(df_veg, aes(x = factor(survey_date), y = PlantHeight_m)) +
  geom_boxplot(fill = "#8fb996", outlier.alpha = 0.3, outlier.size = 1) +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %d")) +   # "Mar 29"
  labs(x = "Survey date (2024)",
       y = "Plant height (m)") +
  theme_bw(base_size = 12)

height_box

ggsave("plant_height_boxplot.png", height_box, width = 6.5, height = 4, dpi = 300)

## can confirm it does not look better as a line graph...

# Percent plant coverage 
height_summary <- df %>%
  group_by(survey_date) %>%
  summarise(
    n_reports    = n(),
    mean_cover   = mean(PercentCoverage, na.rm = TRUE),   # includes the zeros, which is what we want here
    sd_cover     = sd(PercentCoverage, na.rm = TRUE),
    n_veg        = sum(PlantHeight_m > 0, na.rm = TRUE),
    mean_height  = mean(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    sd_height    = sd(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    min_height   = min(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE),
    max_height   = max(PlantHeight_m[PlantHeight_m > 0], na.rm = TRUE))

height_summary

# Percent vegetation coverage over time line graph
cover_line <- ggplot(height_summary, aes(x = survey_date, y = mean_cover)) +
  geom_line(color = "#2f6b4f", linewidth = 0.8) +
  geom_point(color = "#2f6b4f", size = 2.5) +
  scale_x_date(date_labels = "%b %d", breaks = height_summary$survey_date) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(x = "Survey date (2024)",
       y = "Mean vegetation coverage") +
  theme_classic(base_size = 12)

ggsave("veg_coverage_line.png", cover_line, width = 6.5, height = 4, dpi = 300)

# 4. Incorporating current and tide point measurements
survey_cond <- tribble(
  ~survey_date,        ~current_dir, ~tide_stage, ~current_kts,
  ymd("2024-03-29"),   "North",      "Flood",     0.08,
  ymd("2024-04-05"),   "South",      "Flood",     2.13,
  ymd("2024-04-12"),   "North",      "Ebb",       3.61,
  ymd("2024-04-19"),   "South",      "Flood",     2.69,
  ymd("2024-04-26"),   "North",      "Ebb",       3.01)

# Label each survey with its date and speed, in chronological order
survey_cond <- survey_cond %>%
  mutate(label = paste0(format(survey_date, "%b %d"), "\n", current_kts, " kts"),
         label = fct_reorder(label, as.numeric(survey_date)))

df_cond  <- df %>% filter(survey_date != ymd("2024-05-24")) %>% left_join(survey_cond, by = "survey_date")
veg_cond <- df_cond %>% filter(PlantHeight_m > 0)

tide_cols <- c(Flood = "olivedrab4", Ebb = "cornflowerblue")

# summarise and group
cond_summary <- veg_cond %>%
  group_by(survey_date, tide_stage, current_dir, current_kts) %>%
  summarise(mean_height = mean(PlantHeight_m), sd_height = sd(PlantHeight_m), .groups = "drop")

# plotting mean plant height and current velocity by tidal stage/direction
p_speed <- ggplot(cond_summary, aes(x = current_kts, y = mean_height,
                                    color = tide_stage, shape = current_dir)) +
  geom_pointrange(aes(ymin = mean_height - sd_height, ymax = mean_height + sd_height),
                  size = 0.6) +
  geom_text(aes(label = format(survey_date, "%b %d")), nudge_x = 0.25, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = tide_cols) +
  labs(x = "Current velocity at time of survey (kts)",
       y = "Mean plant height ± SD (m)",
       color = "Tidal stage", shape = "Current direction") +
  theme_classic(base_size = 12)

p_speed

# boxplot of plant heights by current speed
p_dist <- ggplot(veg_cond, aes(x = label, y = PlantHeight_m, fill = tide_stage)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = tide_stage), width = 0.15, alpha = 0.25, size = 2,
              show.legend = FALSE) +
  scale_fill_manual(values = tide_cols) +
  scale_color_manual(values = tide_cols) +
  labs(x = "Survey date and predicted current velocity (kts)",
       y = "Plant height (m)", fill = "Tidal stage") +
  theme_classic(base_size = 18, base_family = "Times New Roman")
  

p_dist

ggsave("plant_height_box.png", p_dist, width = 6.5, length = 4, dpi = 300)