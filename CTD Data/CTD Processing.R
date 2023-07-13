## Loading packages
require(pacman)
pacman::p_load(tidyverse, oce, leaflet, sf, gsw, magrittr, devtools, driftR, scales, lubridate)

install.packages("devtools")
devtools::install_github("shaughnessyar/driftR")

remotes::install_github("dankelley/oce", ref = "develop")
remotes::install_github("dankelley/ocedata", ref = "main")

## Remember to set your working directory!

#Importing data to work with 
ctd_data <- dr_read("./EXO_SD_13D101118_010820_183642.xlsx", "EXO", defineVar = TRUE, cleanVar = TRUE)
ctd_clean <- subset(ctd_data, depth_m > 0.5)

#How to find the max value of a column
max(ctd_clean$press_psi_a, na.rm = TRUE)
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

ctd_clean2 <- ctd_clean %>% select(-c(time_fract_sec, fault_code, battery_v, cable_pwr_v))

ctd_clean3 <- ctd_clean2 %>% 
  mutate(datetime = ymd(date_mm_dd_yyyy) + hms(paste(time_hh_mm_ss, time_fract_sec, sep = ".")))


ctd_long <- pivot_longer(ctd_clean3, cols = everything(),
               names_to = "variable",
               values_to = "value")


