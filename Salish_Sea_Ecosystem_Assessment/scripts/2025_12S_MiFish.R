# 1. Setup ---------------------------------------------------------------------
## Load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyr, 
               dplyr,
               cowsay,
               lubridate,
               tidyverse,
               corrplot,
               cowplot,
               changepoint,
               strucchange,
               ggpubr,
               stats,
               ggfortify,
               vegan,
               ggrepel,
               showtext,
               ggeffects,
               glmmTMB,
               DHARMa,
               emmeans,
               permute,
               gt,
               scales,
               stringr,
               readr)

font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#Use for windows operating systems
#font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
#showtext_auto()

marker <- "12S_MiFish"
infile <- "12S_MiFish_U_total_reads_decontaminated.tsv"
out_dir <- file.path("output", marker)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Read in data --------------------------------------------------------------
mifish <- read_tsv("data/raw/eDNA_explorer/eDNA Results/12S_MiFish_U/decontaminated/12S_MiFish_U_total_reads_decontaminated.tsv")
metadata <- read_csv("data/raw/eDNA_explorer/Metadata/2025_eDNA_Sample_Metadata_SQL.csv")


