# 1. Setup ---------------------------------------------------------------------
pacman::p_load(tidyverse, 
               sf,
               rnaturalearth,
               rnaturalearthdata,
               showtext)

# add my fav font
font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()
showtext_opts(dpi = 300)

#Use for windows operating systems
#font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
#showtext_auto()

#create file objects
meta_raw_file   <- "data/raw/eDNA_explorer/Metadata/2025_eDNA_Sample_Metadata_SQL.csv"
meta_clean_file <- "data/processed/2025_sample_metadata_clean.csv"
stopifnot(file.exists(meta_raw_file))

coords_file <- "data/raw/eDNA_explorer/Metadata/2025_eDNA_sampling_coordinates.csv"

# setup consistent theme for nice figures
theme_poster <- theme_bw(base_size = 24) +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.minor = element_blank())

station_cols <- c(PBKN = "#E8A302FF", PBKS = "#00EBF0FF", PBSH = "#CD2990",
                  PBN  = "#6495ED", PBS  = "#E2DD4DFF", PBW  = "#EE6A50",
                  DW1  = "#006400", DW2  = "#191970")

# 2. Sample map-----------------------------------------------------------------

# read in coordinates file
coords <- read_csv(coords_file, show_col_types = FALSE)          

# need to convert to sf object for plotting, but it will override the normal coords
# so need to keep them separate
coords_sf <- st_as_sf(coords, coords = c("longitude", "latitude"),
                      crs = 4326, remove = FALSE) 

# removing SQU, OAK, SJI stations for my poster. Comment out if you want all the stations
# Partridge Bank stations only
#coords_sf <- coords_sf %>%
#  filter(!station_id %in% c("SQU", "OAK", "SJI", "PTBAY_TOW_8m"))

# create the basemap that I want
detailed_land <- ne_countries(scale = "large", returnclass = "sf")

# plot the stations
map <- ggplot()+
  geom_sf(data = detailed_land, color = "black", fill = "grey")+
  geom_sf(data = coords_sf, aes(color = station_id), size = 3)+
  scale_colour_manual(values = station_cols) +
  theme_bw()+
  theme_poster +
  coord_sf(xlim = c(st_bbox(coords_sf)$xmin - 0.08, st_bbox(coords_sf)$xmax + 0.08),
           ylim = c(st_bbox(coords_sf)$ymin - 0.08, st_bbox(coords_sf)$ymax + 0.08)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(axis.title.x = element_text(margin = margin(t = 15)),   
        axis.title.y = element_text(margin = margin(r = 15))) +  
  labs(x = "Longitude", 
       y = "Latitude",
       color = "Sampling Stations")

map

ggsave("output/figures/station_map.png", map,
       width = 8, height = 7, dpi = 300, bg = "white")

# 3. Read in sample metadata ---------------------------------------------------

# lots of NAs to remove
meta_raw <- read_csv(meta_raw_file, na = c("", "NA", "-"),
                     show_col_types = FALSE)

# drop the empty rows 
meta <- meta_raw %>% filter(!is.na(`Sample ID`))

# check for duplicate sample ID's (should come back zero)
meta %>% count(`Sample ID`) %>% filter(n > 1)   
stopifnot(!any(duplicated(meta$`Sample ID`)))


# 4. Rename columns to R-friendly names ----------------------------------------
meta <- meta %>%
  rename(sample       = `Sample ID`,    # MUST match the sequencing column names
         station      = `Station ID`,
         site_text    = Site,
         sample_type  = `Sample Type`,
         lat          = Latitude,
         lon          = Longitude,
         date         = `Sample Date`,
         volume_L     = `Liters Filtered`,
         depth_m      = `Depth (m)`,
         habitat      = `Environmental Feature`,
         notes        = `Additional environmental metadata....`,
         filter_um    = `Filter Pore Size (uM)`,
         preservation = `Preservation Method`) %>%
  select(-`Running No.`, -Substrate)    # not needed (substrate is always "water")

# 5. Clean spelling errors and find missing station id's if there are any
meta %>% count(site_text, station) %>% print(n = Inf)   

# Every sample should now have a station
meta %>% filter(is.na(station))   

# Which samples have a station that isn't in the official station list? 
# should just be the blanks
anti_join(meta, coords, by = c("station" = "station_id")) %>%
  count(station)

# 6. Standardise categories and pull flags out of the notes --------------------

# sanity check number of rows before we join
nrow(meta)

# join coords with meta df
meta <- meta %>%
  left_join(coords, by = c("station" = "station_id"))

# sanity check nrow after the join (should still be 146)
nrow(meta)

# what did the join add?
names(meta)

# Do the field sheet and the station list agree on site names?
meta %>% filter(site_text != site) %>% select(sample, station, site_text, site)

# 7. Check the coordinates -----------------------------------------------------

# Coordinates agree? (0.0001 degrees ≈ 10 m). Should be 0 rows.
meta %>%
  filter(abs(lat - latitude) > 0.0001 | abs(lon - longitude) > 0.0001) %>%
  select(sample, station, lat, latitude, lon, longitude)

# Keep one copy of each (field-sheet versions also cover the blanks)
meta <- meta %>%
  select(-site, -latitude, -longitude) %>%
  rename(site = site_text)

# 8. Add analysis columns ------------------------------------------------------
# also need to change 50um to 20um because I noticed that was wrong in the metadata
meta <- meta %>%
  mutate(
    # broad area, for grouping in figures
    area = case_when(
      str_starts(sample, "JDFB|PBN_TOW") ~ "Partridge Bank",
      str_starts(sample, "PTBAY")        ~ "Port Townsend Bay",
      str_starts(sample, "OAK")          ~ "Oak Bay",
      str_starts(sample, "SJI")          ~ "San Juan Islands",
      str_starts(sample, "SQU")          ~ "Squamish Harbor"),
    
    # sampling method, from the notes ("Rosette Cast" vs "Rosette cast" -> one)
    method = case_when(
      str_detect(notes, regex("sequench", ignore_case = TRUE)) ~ "Sequench tow",
      str_detect(notes, regex("blank",    ignore_case = TRUE)) ~ "Field blank",
      str_detect(notes, regex("rosette",  ignore_case = TRUE)) ~ "Rosette cast",
      str_detect(notes, regex("grab",     ignore_case = TRUE)) ~ "3 m grab"),
    
    # Correction: tows were recorded as 50 µm in the field sheet but used 20 µm filters
    filter_um = if_else(method == "Sequench tow", 20, filter_um),
    
    # filter system ("\u00b5" = the micro sign, written safely)
    filter_type = factor(case_when(
      filter_um == 1.2 ~ "1.2 \u00b5m",
      filter_um == 5   ~ "5 \u00b5m",
      filter_um == 20  ~ "20 \u00b5m tow"),
      levels = c("1.2 \u00b5m", "5 \u00b5m", "20 \u00b5m tow")),
    
    # TRUE/FALSE flags from the notes
    flag_delaminated = str_detect(notes, regex("delaminat", ignore_case = TRUE)),
    flag_valve_open  = str_detect(notes, regex("valve left open", ignore_case = TRUE)),
    tech_rep_of      = str_extract(notes, "[A-Z]+_\\d{2}_\\d{6}"))

# Nothing should be NA in these
stopifnot(!any(is.na(meta$area)), !any(is.na(meta$method)),
          !any(is.na(meta$filter_type)))
count(meta, area, method, filter_type)

# 9. Save -----------------------------------------------------------------------
write_csv(meta, meta_clean_file)
