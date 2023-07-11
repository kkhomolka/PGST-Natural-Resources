## Loading packages
require(pacman)
p_load(tidyverse, oce, leaflet, sf, devtools, driftR, scales)

# install.packages("devtools")
#devtools::install_github("shaughnessyar/driftR")

## Importing data to work with 
ctd_data <- dr_read("./EXO_SD_13D101118_010820_183642.xlsx", "EXO", defineVar = TRUE, cleanVar = TRUE)

ctd_clean <- subset(ctd_data, depth_m > 0.5)

ggplot(ctd_clean, aes(x=time_hh_mm_ss, y=depth_m)) +
  geom_point() +
  scale_y_reverse() +
  xlab("Time") + 
  ylab("Depth (m)") + 
  theme_classic(base_size = 10) +
  theme(legend.position = "none")





       