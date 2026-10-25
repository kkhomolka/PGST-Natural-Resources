# 1. Setup ---------------------------------------------------------------------
pacman::p_load(tidyverse)

sonde_dir <- "data/raw/water_quality/upcasts/JDFB"                       

# 2. Read every cast ---------------------------------------------------------------
sonde_files <- list.files(sonde_dir, pattern = "\\.csv$", full.names = TRUE)
length(sonde_files)    # should be 16

# read_csv() can read many files at once and stack them.
# id = "file" adds a column saying which file each row came from.
sonde_raw <- read_csv(sonde_files, id = "file", show_col_types = FALSE)

# 3. Tidy ------------------------------------------------------------------------
sonde <- sonde_raw %>%
  mutate(file    = basename(file),                               # drop the folder path
         station = str_match(file, "^JDFB_([A-Z0-9]+)_")[, 2],   # station from file name
         date    = mdy(date_mm_dd_yyyy)) %>%                     # "6/26/2025" -> a real date
  select(file, station, date,
         depth_m, temp_c, sal_psu,
         do_mg_l = odo_mg_l, do_pct = odo_percent_sat)          # rename while selecting

# One row per cast: every file should have a station and a date
count(sonde, station, date) %>% print(n = Inf)
stopifnot(!any(is.na(sonde$station)), !any(is.na(sonde$date)))

# Remove out-of-water / surface-noise rows
n_before <- nrow(sonde)
sonde <- sonde %>% filter(sal_psu > 20, depth_m > 0.5)
n_before - nrow(sonde)      # rows removed

summary(select(sonde, depth_m, temp_c, sal_psu, do_mg_l))

# 4. Average into 1 m depth bins -------------------------------------------------
sonde_bin <- sonde %>%
  mutate(depth_bin = round(depth_m)) %>%      # 0.5-1.49 m -> 1, 1.5-2.49 -> 2, ...
  group_by(file, station, date, depth_bin) %>%
  summarise(temp_c  = mean(temp_c),
            sal_psu = mean(sal_psu),
            do_mg_l = mean(do_mg_l),
            n_obs   = n(),                     # how many readings went into each bin
            .groups = "drop")

nrow(sonde_bin)          
summary(sonde_bin$n_obs)

# 5. Profile plots from the raw cast (not aggregated into bins yet)-------------
# Same station colours as the richness profiles, so the figures match
station_cols <- c(PBKN = "#E8A302FF", PBKS = "#00EBF0FF", PBSH = "#CD2990",
                  PBN  = "#6495ED", PBS  = "#E2DD4DFF", PBW  = "#EE6A50",
                  DW1  = "#006400", DW2  = "#191970")

sonde_plot <- sonde %>%
  mutate(month   = factor(month(date, label = TRUE, abbr = TRUE),
                          levels = c("Jun", "Jul", "Aug", "Sep")),
         station = factor(station, levels = names(station_cols))) %>%
  arrange(file, depth_m)          # sort each cast by depth so the line doesn't scribble

p_temp <- ggplot(sonde_plot, aes(x = temp_c, y = depth_m, colour = station)) +
  geom_path(aes(group = file), linewidth = 0.7) +       # one line per cast
  facet_wrap(~ month, nrow = 1, drop = FALSE) +         # keep the empty Aug panel
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_colour_manual(values = station_cols, drop = FALSE) +
  labs(x = "Temperature (\u00b0C)", y = "Depth (m)", colour = "Station") +
  theme_poster

p_temp

# salinity
p_sal <- ggplot(sonde_plot, aes(x = sal_psu, y = depth_m, colour = station)) +
  geom_path(aes(group = file), linewidth = 0.7) +       # one line per cast
  facet_wrap(~ month, nrow = 1, drop = FALSE) +         # keep the empty Aug panel
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_colour_manual(values = station_cols, drop = FALSE) +
  labs(x = "Salinity (PSU)", y = "Depth (m)", colour = "Station") +
  theme_poster

p_sal  

# dissolved oxygen
  p_do <- ggplot(sonde_plot, aes(x = do_mg_l, y = depth_m, colour = station)) +
  geom_path(aes(group = file), linewidth = 0.7) +       # one line per cast
  facet_wrap(~ month, nrow = 1, drop = FALSE) +         # keep the empty Aug panel
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_colour_manual(values = station_cols, drop = FALSE) +
  labs(x = expression(DO~(mg~L^{-1})), y = "Depth (m)", colour = "Station") +
  theme_poster
  
  p_do

# 5. Aggregated into 1m depth bins to smooth plots and for RDA------------------
  
sonde_bin_plot <- sonde_bin %>%
    mutate(month   = factor(month(date, label = TRUE, abbr = TRUE),
                            levels = c("Jun", "Jul", "Aug", "Sep")),
           station = factor(station, levels = names(station_cols))) %>%
          arrange(file, depth_bin)          # sort each cast by depth so the line doesn't scribble

# binned temperature   
p_bin_temp <- ggplot(sonde_bin_plot, aes(x = temp_c, y = depth_bin, colour = station)) +
    geom_path(aes(group = file), linewidth = 0.7) +       # one line per cast
    facet_wrap(~ month, nrow = 1, drop = FALSE) +         # keep the empty Aug panel
    scale_x_continuous(position = "top") +
    scale_y_reverse() +
    scale_colour_manual(values = station_cols, drop = FALSE) +
    labs(x = "Temperature (\u00b0C)", y = "Depth (m)", colour = "Station") +
    theme_poster

p_bin_temp

#binned salinity
p_bin_sal <- ggplot(sonde_bin_plot, aes(x = sal_psu, y = depth_bin, colour = station)) +
  geom_path(aes(group = file), linewidth = 0.7) +       # one line per cast
  facet_wrap(~ month, nrow = 1, drop = FALSE) +         # keep the empty Aug panel
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_colour_manual(values = station_cols, drop = FALSE) +
  labs(x = "Salinity (PSU)", y = "Depth (m)", colour = "Station") +
  theme_poster

p_bin_sal  

# binned dissolved oxygen
p_bin_do <- ggplot(sonde_bin_plot, aes(x = do_mg_l, y = depth_bin, colour = station)) +
  geom_path(aes(group = file), linewidth = 0.7) +       # one line per cast
  facet_wrap(~ month, nrow = 1, drop = FALSE) +         # keep the empty Aug panel
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_colour_manual(values = station_cols, drop = FALSE) +
  labs(x = expression(DO~(mg~L^{-1})), y = "Depth (m)", colour = "Station") +
  theme_poster

p_bin_do

#NOTE!! To use WQ for an RDA, you need to go back in and save your sonde_bin df
write_csv(sonde_bin, file = "data/processed/2025_sonde_depth_binned.csv")
