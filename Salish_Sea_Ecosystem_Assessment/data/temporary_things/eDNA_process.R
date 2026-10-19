# Required packages
library(tidyverse)
library(readr)
library(readxl)
library(janitor)

# Read in metadata for July
metadat <- read_excel('../data/eDNA_metadata_notfromGUIpull/2025_eDNA_Explorer_Sample_Metadata.xlsx',
                      sheet = 1, skip = 4) %>%
  clean_names() %>%
  filter(sample_date >= as.Date("2025-07-01 UTC") & sample_date <= as.Date("2025-07-31 UTC")) %>%
  rename(filter_size = x16,
         filter_type = x17)

table(metadat$sample_type, useNA = "ifany")

metadat <- metadat %>%
  filter(sample_type == "Sample")

# Read in data for July
dats <- list()
# HAVE TO DO ONE GRP AT A TIME BC SOME TAXPATH/COMMONNAMES show up in multiple groups
grps <- c("12S_MiFish_u") # "18S_euk", "CO1_Metazoa", "vert12S"
samps_used_vec <- c(123) # 123, 123, 121# hard-coded from eDNA Explorer GUI
names(samps_used_vec) <- grps

for (grp in grps) { 
  
  print(grp)
  
  samps_used <- samps_used_vec[grp]
  
  dat_pp_bycat <- read_tsv(paste0("../data/eDNA/eDNA Results/", grp, "/decontaminated/", grp, "_basic_list_decontaminated.tsv"))
  dat_po_bysamp <- read_tsv(paste0("../data/eDNA/eDNA Results/", grp, "/decontaminated/", grp, "_read_summary_decontaminated.tsv"))
  dat_expanded <- read_tsv(paste0("../data/eDNA/eDNA Results/", grp, "/decontaminated/", grp, "_total_reads_decontaminated.tsv"))

  # Check sample read totals match
  po_bysamp <- dat_po_bysamp %>%
  filter(is.na(notes)) %>%  # exclude samples with notes
  select(reads)

  po_bysamp_check <- dat_expanded %>% 
  select(where(is.numeric)) %>% # numeric cols represent the samps
  colSums() # total reads per sample

  print(all(po_bysamp$reads == po_bysamp_check))

  # Check percentages positive match (e.g., across the 121 samples used for verts!)
  perc_check <- dat_expanded %>%
  filter(!taxonomic_path %in% c('unassigned because reads did not pass filters', 
                                'unassigned by tronko', 
                                'unassigned because of decontamination')) %>%
  rowwise() %>%
  mutate(tot = sum(c_across(where(is.numeric)) > 0, na.rm = TRUE)) %>%
  ungroup() %>%
  select(taxonomic_path, common_name, tot) %>%
  mutate(perc = tot/samps_used)

  t <- dat_pp_bycat %>% 
  left_join(perc_check, by = c('taxonomic_path', 'common_name'))
  
  print(t[1:5,])

  # Proceed with full data (dat_expanded)
  dat <- dat_expanded

  rm(dat_pp_bycat, dat_po_bysamp, dat_expanded, perc_check, po_bysamp_check, po_bysamp, t)

  dat <- dat %>%
    filter(!taxonomic_path %in% c('unassigned because reads did not pass filters', 
                                  'unassigned by tronko', 
                                  'unassigned because of decontamination')) 
  dat <- dat %>%
    select(c("taxonomic_path", "common_name", matches("07[0-9]{2}25$")))
  
  dats[[grp]] <- dat
}
rm(dat, grp, samps_used)

dats_long <- lapply(dats, function(x){ # make row ids sample/category
  x %>% pivot_longer(cols = c(where(is.numeric)), names_to = 'sample_id', values_to = 'reads')
})

dats_long <- lapply(dats_long, function(x){ # add in sample metadata
  x %>%
    left_join(metadat %>%
              select(station_id, sample_id, sample_date, depth_m, filter_size),
            by = 'sample_id')
  
})

# Reorganize by site!
all_dat_long <- bind_rows(dats_long) # rows are still sample/tax. cat.
table(all_dat_long$station_id, useNA = 'ifany') 
# keep finer station categories but rename SJI to SJI_WI
all_dat_long <- all_dat_long %>%
  mutate(station_id = if_else(station_id == 'SJI', 'SJI_WI', station_id))


write.csv(all_dat_long, '../data/eDNA_processed/MiFish.csv', row.names = FALSE)



