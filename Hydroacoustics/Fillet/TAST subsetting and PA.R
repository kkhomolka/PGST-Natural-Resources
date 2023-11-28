# 1. Setup ---------------------------------------------------------------------

## Load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyverse, 
               dplyr,
               cowsay)

## Welcome
say("Welcome To All Things TAST!", 
    by = "shark", 
    what_color = "green", 
    by_color = "blue")

## Set working directory
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")


# 2. Power analysis using 'pwr' package ----------------------------------------

## Two-sample t-test analysis
pwr.t.test(n =, #leaving n blank because that is the desired outcome
           d = 0.3, #assuming a medium-ish effect size 
           sig.level = 0.05, 
           power = 0.9, 
           type = c("two.sample"), 
           alternative = "two.sided")


## 1. Loading and subsetting data-----------------------------------------------

#read in files and remove empty rows
TAST_files <- read_csv("BlueView footage datasheet (OI).csv")
TAST_files_clean <- head(TAST_files, 6684)

#create ON and OFF stratum
TAST_ON <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "ON")
TAST_OFF <- subset(TAST_files_clean, TAST_files_clean$`TAST status` == "OFF")






