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
         sample_type_meta = `Sample Type`,
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

# 3. Clean spelling errors and convert to station codes-------------------------
meta %>% count(site_text, station) %>% print(n = Inf)   # see the problem

# The SeQuench tows have no Station ID. Fill it in from the sample name.
meta <- meta %>%
  mutate(station = case_when(
    !is.na(station)                ~ station,
    str_detect(sample, "^PBN_TOW")   ~ "PBN_tow",
    str_detect(sample, "^PTBAY_TOW") ~ "PTBAY_tow"))

# The SeQuench tows have no station id in the metadata, so I ust assign
meta <- meta %>%
  mutate(station = case_when(
    !is.na(station)                           ~ station,   # keep existing IDs
    str_detect(sample, "^PBN_TOW_.*_3m_")     ~ "PBN_TOW_3m",
    str_detect(sample, "^PBN_TOW_.*_15m_")    ~ "PBN_TOW_15m",
    str_detect(sample, "^PTBAY_TOW_.*_8m_")   ~ "PTBAY_TOW_8m"))

# Every sample should now have a station
meta %>% filter(is.na(station))          # should print 0 rows

# Which samples have a station that isn't in the official station list?
anti_join(meta, stations, by = c("station" = "station_id")) %>%
  count(station)


# ---- 4. Standardise categories and pull flags out of the notes -------------------
meta <- meta %>%
  mutate(
    # "Rosette Cast" vs "Rosette cast" -> one spelling
    method = case_when(
      str_detect(notes, regex("sequench", ignore_case = TRUE)) ~ "SeQuench tow",
      str_detect(notes, regex("blank",    ignore_case = TRUE)) ~ "Field blank",
      str_detect(notes, regex("rosette",  ignore_case = TRUE)) ~ "Rosette cast",
      str_detect(notes, regex("grab",     ignore_case = TRUE)) ~ "3 m grab"),
    
    # filter system: pore size + preservation together describe it
    # ("\u00b5" is the code for the micro sign; it avoids encoding problems)
    filter_type = case_when(
      filter_um == 1.2 ~ "1.2 \u00b5m PES, silica gel",
      filter_um == 5   ~ "5 \u00b5m, self-preserving",
      filter_um == 50  ~ "50 \u00b5m SeQuench tow"),
    # factor() fixes the order things appear in legends and facets
    filter_type = factor(filter_type, levels = c("1.2 \u00b5m PES, silica gel",
                                                 "5 \u00b5m, self-preserving",
                                                 "50 \u00b5m SeQuench tow")),
    
    # TRUE/FALSE flags from the free-text notes, so we can filter/colour by them
    flag_delaminated = str_detect(notes, regex("delaminat", ignore_case = TRUE)),
    flag_valve_open  = str_detect(notes, regex("valve left open", ignore_case = TRUE)),
    tech_rep_of      = str_extract(notes, "[A-Z]+_\\d{2}_\\d{6}"),
    
    habitat = str_to_sentence(habitat),
    date    = as.Date(date))

# Check the new columns. Every sample should have a method and a filter type.
count(meta, method, filter_type, preservation)
stopifnot(!any(is.na(meta$method)), !any(is.na(meta$filter_type)))

meta %>% filter(flag_delaminated | flag_valve_open) %>%
  select(sample, station, depth_m, notes)


# ---- 6. Save ---------------------------------------------------------------------
meta_clean <- meta %>%
  select(sample, area, station, station_name, sample_type_meta, date,
         lat, lon, depth_m, habitat, method, volume_L, filter_um,
         preservation, filter_type, flag_delaminated, flag_valve_open,
         tech_rep_of, notes)

write_csv(meta_clean, meta_clean_file)
message("Saved ", nrow(meta_clean), " samples to ", meta_clean_file)