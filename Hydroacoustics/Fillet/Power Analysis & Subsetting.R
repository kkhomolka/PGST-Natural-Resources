#load in packages
pacman::p_load(pwr, ggplot2, tidyverse, dplyr)

#Determining the sample size (n) using a power analysis and the 'pwr' package 

pwr.anova.test(k = 2, n = , f = 0.1, sig.level = 0.05, power = 0.9) #leaving n blank because that is the desired outcome 

#trying the same thing with a two-sample t-test instead
pwr.t.test(n =, d = 0.1, sig.level = 0.05, power = 0.9, type = c("two.sample"), alternative = "less")
