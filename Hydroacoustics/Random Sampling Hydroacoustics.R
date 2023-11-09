#script for weighted and random sampling of BlueView files for the TAST 

#Remember to set your working directory!
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics")

#Loading packages
require(pacman)
p_load(tidyverse, ggplot2)

## 1. Loading and subsetting data------------------------------------------------------------

#read in files and remove empty rows
TAST_files <- read_csv("BlueView footage datasheet (OI).csv")
TAST_files_clean <- head(TAST_files, 6684)

#create ON and OFF dataframes 
TAST_ON <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "ON")
TAST_OFF <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "OFF")

## 2. Random sampling without replacement------------------------------------------------------------------------
sample(TAST_ON$Filename, size = 100)


