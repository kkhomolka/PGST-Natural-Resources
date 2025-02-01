# 1. Setup ---------------------------------------------------------------------

## load packages
pacman::p_load(pwr, 
               ggplot2, 
               tidyverse, 
               dplyr,
               cowsay)

## Set working directory
setwd("~/GitHub/PGST-Natural-Resources/Hydroacoustics/TAST")

## Welcome
say("Welcome To All Things Fillet!", by = "random")


# 2. POWER ANALYSES FOR FILLET -------------------------------------------------

## Two-sample t-test analysis
pwr.t.test(n =600 , d = , sig.level = 0.05, power = 0.8, type = c("two.sample"), alternative = "two.sided")

