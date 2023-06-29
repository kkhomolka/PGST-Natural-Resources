## Loading packages
require(pacman)
p_load(tidyverse, oce, leaflet, sf)

## Importing data to work with 
read.csv("/Users/khadijahhomolka/Documents/GitHub/PGST-Natural-Resources/CTD Data/KorEXO Measurement File Export - 022223 115854 (1).csv")

data <- "exo_cast_022223"

colnames(exo_cast_022223)[6] <- "cond_us_cm"
colnames(exo_cast_022223)[7] <- "depth_m"

ctd_data <- dr_read("./EXO_SD_13D101118_010820_183642.xlsx", "EXO", defineVar = TRUE, cleanVar = TRUE)
?dr_read


       