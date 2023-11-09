#script for weighted and random sampling of BlueView files for the TAST 

#Remember to set your working directory!
setwd("~/Documents/GitHub/PGST-Natural-Resources/Hydroacoustics")

#Loading packages
require(pacman)
p_load(tidyverse, ggplot2, dplyr)

## 1. Loading and subsetting data------------------------------------------------------------

#read in files and remove empty rows
TAST_files <- read_csv("BlueView footage datasheet (OI).csv")
TAST_files_clean <- head(TAST_files, 6684)

#create ON and OFF stratum
TAST_ON <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "ON")
TAST_OFF <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "OFF")

#sample(TAST_ON$Filename, size = 100)

#strata_sampling_plan <- TAST_ON %>% 
  #initial_split(strata = "Filename", prop = 0.22)

#stratified_training_data <- training(strata_sampling_plan)

## 2. Random sampling without replacement------------------------------------------------------------------------

#calculate proportions of ON and OFF files in combined dataset
##ON <- 1147/6684 = 0.22
##OFF <- 5237/6684 = 0.78

#determine sample size -> 335 files total seems reasonable to start with (5% of 6684)
#calculate proportions with new sample size
##ON <- 335*0.22 = 74
##OFF <- 335*0.78 = 261

#random sampling within TAST_ON stratum
set.seed(1994) #setting seed will ensure reproducibility of random generating (same numbers will be generated every time)
sample_size <- 74 #based off a total sample size of 335 (5% of 6684), 74 is 22% of 335
random_TAST_ON <- TAST_ON[sample(nrow(TAST_ON), sample_size), , drop = FALSE]

#random sampling within TAST_OFF stratum
set.seed(1994)
sample_size <- 261 #based off a total sample size of 335 (5% of 6684), 261 is 78% of 335
random_TAST_OFF <- TAST_OFF[sample(nrow(TAST_ON), sample_size), , drop = FALSE]
