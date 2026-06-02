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
               # eDNA-specific additions
               here,         # clean relative paths within the project
               janitor,      # clean_names() and other tidying helpers
               dada2,        # amplicon sequence variant inference
               phyloseq,     # community data objects + downstream analysis
               decontam,     # identify contaminants using blanks/negatives
               Biostrings)   # DNA sequence manipulation

font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

# 2. Read in files--------------------------------------------------------------
co1     <- read_tsv(here("data/raw/eDNA_data/eDNA Results/CO1_Metazoa/decontaminated/CO1_Metazoa_total_reads_decontaminated.tsv"))
euk18s <- read_tsv(here("data/raw/eDNA_data/eDNA Results/18S_Euk/decontaminated/18S_Euk_total_reads_decontaminated.tsv"))         
mifish  <- read_tsv(here("data/raw/eDNA_data/eDNA Results/12S_MiFish_U/decontaminated/12S_MiFish_U_total_reads_decontaminated.tsv"))    
vert12s <- read_tsv(here("data/raw/eDNA_data/eDNA Results/vert12S/decontaminated/vert12S_total_reads_decontaminated.tsv"))         

# 3. Data transformations-------------------------------------------------------

#binding all dfs into one 
all_markers <- bind_rows(
  co1     %>% mutate(marker = "CO1_Metazoa"),
  euk18s  %>% mutate(marker = "18S_Euk"),
  mifish  %>% mutate(marker = "12S_MiFish_U"),
  vert12s %>% mutate(marker = "vert12S"))

# 4. Species Overlap------------------------------------------------------------

#18S vs CO1
co1_species    <- co1    %>% filter(!is.na(common_name)) %>% pull(common_name) %>% unique()
euk18s_species <- euk18s %>% filter(!is.na(common_name)) %>% pull(common_name) %>% unique()

shared      <- intersect(co1_species, euk18s_species)
co1_only    <- setdiff(co1_species, euk18s_species)
euk18s_only <- setdiff(euk18s_species, co1_species)

length(shared); length(co1_only); length(euk18s_only)


