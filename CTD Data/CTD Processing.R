#Remember to set your working directory!
setwd("~/GitHub/PGST-Natural-Resources/CTD Data")

##If you need to install the {driftR} package 
install.packages("devtools")
devtools::install_github("shaughnessyar/driftR")
remotes::install_github("dankelley/oce", ref = "develop")
remotes::install_github("dankelley/ocedata", ref = "main")

#Loading packages
require(pacman)
p_load(tidyverse, oce, leaflet, sf, gsw, magrittr, devtools, driftR, scales, lubridate)

## 1. Importing data--------------------------------------------------------------------------------------------------- 

#Using the {driftR} package to import 
ctd_data <- dr_read("./EXO_SD_13D101118_010820_183642.xlsx", "EXO", defineVar = TRUE, cleanVar = TRUE)

ctd_clean <- subset(ctd_data, depth_m > 0.5)

## 1.1 Exploring and cleaning the data------------------------------------------------------------------------------------------------ 

#Finding the max pressure of the cast 
max_press <- max(ctd_clean$press_psi_a, na.rm = TRUE)
max(ctd_clean$depth_m, na.rm = TRUE)

#Removing unwanted columns and combining date and time into a new col
ctd_clean2 <- ctd_clean %>% select(-c(time_fract_sec, fault_code, battery_v, cable_pwr_v))

ctd_clean3 <- ctd_clean2 %>% 
  mutate(datetime = ymd(date_mm_dd_yyyy) + hms(paste(time_hh_mm_ss, sep = ".")))

#Isolating the downcast by finding the index number where pressure starts to decrease
downcast_start_index <- which(diff(ctd_clean3$press_psi_a) < 0)[1]
downcast <- ctd_clean3[1:downcast_start_index, ]
print(downcast) #print to check 

#subtracting the downcast from the larger df
upcast <- ctd_clean3[-(1:downcast_start_index-1), ]

## 2.1 Plotting----------------------------------------------------------------------------------------------------------

#Preliminary plotting of the upcast + downcast depth profile, note that you can see the 1min soak periods 
ggplot(ctd_clean, aes(x=time_hh_mm_ss, y=depth_m)) +
  geom_point() +
  scale_y_reverse() +
  xlab("Time") + 
  ylab("Depth (m)") + 
  theme_classic(base_size = 10) +
  theme(legend.position = "none")

#Upcast + downcast profile using DO vs. Pressure
ggplot(ctd_clean, aes(x=odo_mg_l, y=press_psi_a))+
  geom_path(col = "blue")+
  scale_y_reverse()+
  scale_x_continuous(position = "top")+
  theme_bw()+
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1))+
  labs(x = expression(~Dissolved~Oxygen~(mg/L)),
       y = expression(~Pressure~(psi)))

#Now let's plot the DO from just the downcast to check if the downcast truly separated  
ggplot(downcast, aes(x=odo_mg_l, y=press_psi_a)) +
  geom_point() +
  ylim(max("depth_m"))+
  scale_y_reverse() +
  theme_classic(base_size = 10) +
  theme(legend.position = "none")+
  labs(x = expression(~Dissolved~Oxygen~(mg/L)),
       y = expression(~Presure~(psi)))

## 2.2 Data exploration plots and making the aesthetics better--------------------------------------------------------

#Plotting the upcast  
ggplot(upcast, aes(x=odo_mg_l, y=depth_m))+
  geom_path(col = "blue")+
  scale_y_reverse()+
  scale_x_continuous(position = "top")+
  theme_bw()+
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1))+
  labs(x = expression(~Dissolved~Oxygen~(mg/L)),
       y = expression(~Depth~(m)))

#Plotting the downcast 
ggplot(downcast, aes(x=odo_mg_l, y=depth_m))+
  geom_path(col = "blue")+
  scale_y_reverse()+
  scale_x_continuous(position = "top")+
  theme_bw()+
  theme(axis.text = element_text(size = 12, colour = 1),
        axis.title = element_text(size = 14, colour = 1))+
  labs(x = expression(~Dissolved~Oxygen~(mg/L)),
       y = expression(~Depth~(m)))

#Let's make some facet plots!
downcast_pivot <- pivot_longer(downcast, cols= c(4:25), names_to = "variable", values_to = "values")



#Adding a new column that contains the numeric representation of the POSIXct datetime
ctd_clean3 <- ctd_clean3 %>% mutate(numeric_datetime = as.numeric(datetime))

#Doing the same thing for the downcast
downcast <- downcast %>% mutate(numeric_datetime = as.numeric(datetime))

ggplot(data = downcast, mapping = aes(y = depth_m))+
  geom_path(aes(x = temp_c, color = "Temperature (C)"))+
  geom_path(aes(x = sal_psu, color = "Salinity (psu)"))+
  geom_path(aes(x = odo_mg_l, color = "Dissolved Oxygen (mg/L)"))+
  labs(title = "Water Quality Variables vs. Depth", 
       x = "Variables",
       y = "Depth (m)")+
  scale_color_manual(name = "Variables",
                     values = c("Temperature (C)" = "red","Salinity (psu)" = "black", "Dissolved Oxygen (mg/L)" = "blue"))

