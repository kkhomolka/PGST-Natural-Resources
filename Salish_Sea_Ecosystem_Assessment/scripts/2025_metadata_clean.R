# 1. Setup ---------------------------------------------------------------------
pacman::p_load(tidyverse, 
               sf,
               ggplot2,
               rnaturalearth,
               rnaturalearthdata,
               showtext)

# add my fav font
font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#Use for windows operating systems
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

#create file objects
meta_raw_file   <- "data/raw/eDNA_explorer/Metadata/2025_eDNA_Sample_Metadata_SQL.csv"
meta_clean_file <- "data/raw/eDNA_explorer/Metadata/2025_sample_metadata_clean.csv"
stopifnot(file.exists(meta_raw_file))

coords_file <- "data/raw/eDNA_explorer/Metadata/2025_eDNA_sampling_coordinates.csv"

# 2. Sample map-----------------------------------------------------------------

# read in coordinates and station_ids
coords <- read_csv(coords_file)

# need to convert to an sf object adn extract the map I want
coords <- st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)
detailed_land <- ne_countries(scale = "large", returnclass = "sf")

# plot the stations
ggplot()+
  geom_sf(data = detailed_land, color = "black", fill = "grey")+
  geom_sf(data = coords, aes(color = station_id), size = 3)+
  theme_bw()+
  theme(text = element_text(size = 16, family = "Times New Roman"))+
  coord_sf(xlim = c(st_bbox(coords)$xmin - 0.08, st_bbox(coords)$xmax + 0.08),
           ylim = c(st_bbox(coords)$ymin - 0.08, st_bbox(coords)$ymax + 0.08)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Longitude", 
       y = "Latitude",
       color = "Sampling Stations")

# 3. Read in sample metadata ---------------------------------------------------

# lots of NAs to remove
meta_raw <- read_csv(meta_raw_file, na = c("", "NA", "-"),
                     show_col_types = FALSE)

# drop the empty rows 
meta <- meta_raw %>% filter(!is.na(`Sample ID`))
nrow(meta)

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

# 3. Clean spelling errors and find missing station id's if there are any
meta %>% count(site_text, station) %>% print(n = Inf)   

# The SeQuench tows have no station id in the metadata, so I must assign
meta <- meta %>%
  mutate(station = case_when(
    !is.na(station)                           ~ station,   # keep existing IDs
    str_detect(sample, "^PBN_TOW_.*_3m_")     ~ "PBN_TOW_3m",
    str_detect(sample, "^PBN_TOW_.*_15m_")    ~ "PBN_TOW_15m",
    str_detect(sample, "^PTBAY_TOW_.*_8m_")   ~ "PTBAY_TOW_8m"))

# Every sample should now have a station
meta %>% filter(is.na(station))   

# Which samples have a station that isn't in the official station list?
anti_join(meta, coords, by = c("station" = "station_id")) %>%
  count(station)

#just the blanks don't have stations so we're good to move on!

# 4. Standardise categories and pull flags out of the notes --------------------

# sanity check number of rows before we join
nrow(meta)

#join coords with meta df
meta <- meta %>%
  left_join(coords, by = c("station" = "station_id"))

# sanity check nrow after the join 
nrow(meta)




# 6. Save ----------------------------------------------------------------------
meta_clean <- meta %>%
  select(sample, area, station, station_name, sample_type_meta, date,
         lat, lon, depth_m, habitat, method, volume_L, filter_um,
         preservation, filter_type, flag_delaminated, flag_valve_open,
         tech_rep_of, notes)

write_csv(meta_clean, meta_clean_file)
message("Saved ", nrow(meta_clean), " samples to ", meta_clean_file)