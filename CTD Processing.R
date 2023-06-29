## Loading packages
require(pacman)
p_load(tidyverse, oce, leaflet, sf)
require(ocedata)

## Importing data to work with 
read_xlsx("/Users/khadijahhomolka/Documents/R/exo_cast_022223.xlsx")

data <- "exo_cast_022223"

colnames(exo_cast_022223)[6] <- "cond_us_cm"
colnames(exo_cast_022223)[7] <- "depth_m"




       