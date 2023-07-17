## Loading packages
require(pacman)
p_load(tidyverse, oce, leaflet, sf, gsw, magrittr, devtools, driftR, scales, lubridate)

install.packages("devtools")
devtools::install_github("shaughnessyar/driftR")

remotes::install_github("dankelley/oce", ref = "develop")
remotes::install_github("dankelley/ocedata", ref = "main")

## Remember to set your working directory!

#Importing data to work with 
ctd_data <- dr_read("./EXO_SD_13D101118_010820_183642.xlsx", "EXO", defineVar = TRUE, cleanVar = TRUE)
ctd_clean <- subset(ctd_data, depth_m > 0.5)

#How to find the max value of a column
max_press <- max(ctd_clean$press_psi_a, na.rm = TRUE)
max(ctd_clean$depth_m, na.rm = TRUE)



#Preliminary plotting of the depth profile 
ggplot(ctd_clean, aes(x=time_hh_mm_ss, y=depth_m)) +
  geom_point() +
  scale_y_reverse() +
  xlab("Time") + 
  ylab("Depth (m)") + 
  theme_classic(base_size = 10) +
  theme(legend.position = "none")

#Plotting dissolved oxygen vs. depth 
ggplot(ctd_clean, aes(x=odo_mg_l, y=depth_m)) +
  geom_point() +
  ylim(max("depth_m"))+
  scale_y_reverse() +
  xlab("Dissolved Oxygen (mg/L") + 
  theme_classic(base_size = 10) +
  theme(legend.position = "none")

##trying to isolate the upcast 
#looks like I need to make a ctd object using {oce} before I can do anything 
#ctd_clean_upcast <- ctdTrim("depth_m", 
                            #method = "upcast", 
                            #removeDepthInversions = FALSE,
                            #parameters = NULL,
                            #indices = FALSE,
                            #debug = getOption("oceDebug"))


ggplot(ctd_clean, aes(x=odo_mg_l, y=press_psi_a))+
   geom_path(col = "blue")+
   scale_y_reverse()+
   scale_x_continuous(position = "top")+
   theme_bw()+
   theme(axis.text = element_text(size = 12, colour = 1),
         axis.title = element_text(size = 14, colour = 1))+
   labs(x = expression(~Dissolved~Oxygen~(mg/L)),
        y = expression(~Pressure~(psi)))

##Removing unwanted columns and combining date and time into a new col
ctd_clean2 <- ctd_clean %>% select(-c(time_fract_sec, fault_code, battery_v, cable_pwr_v))

ctd_clean3 <- ctd_clean2 %>% 
  mutate(datetime = ymd(date_mm_dd_yyyy) + hms(paste(time_hh_mm_ss, sep = ".")))

#isolating the downcast by finding the index number where pressure starts to decrease
downcast_start_index <- which(diff(ctd_clean3$press_psi_a) < 0)[1]
downcast <- ctd_clean3[1:downcast_start_index, ]
print(downcast)

#subtracting the downcast from the larger df
upcast <- ctd_clean3[-(1:downcast_start_index-1), ]

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