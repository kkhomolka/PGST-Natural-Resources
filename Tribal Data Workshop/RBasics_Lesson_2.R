#Loading libraries
require(pacman)
p_load(knitr, plotly, tidyverse, dataRetrieval, lubridate)

#Read in data 
water_qual <- read_csv("/Users/khadijahhomolka/Documents/Tribal Data Workshop/water_quality_data.csv")
# can also use the import button in the environment pane, then from Text (readr) and then you can navigate to the file to open
# You can tell it to skip rows, change the headers and it will also generate code that you can copy int your console to check the syntax you need

#Checking dataframe structure 
head(water_qual)
str(water_qual)
summary(water_qual)

#### Note: shift + cmd + c allows you to comment out multiple lines of code that are selected 
# to remove of variable or dataframe from the environment
      ### use rm() to remove it from the environment 

#How to add new columns and populate them
water_qual2 <- mutate(water_qual, Temperature_F = (Temperature_C * 9/5) +32)

#can use ifelse to replace existing values in a column
  ### can use a ' mark to name a column with spaces in the name 
water_qual2 <- mutate(water_qual, DO_status = ifelse('Optical DO_mg/l' < 3, "DO too low", "DO meets standard"))

#can use na_if to assign NAs to values that we want to relist as NAs 
water_qual <- mutate(water_qual, across(where(is.numeric), ~na_if(., -1e+06)))
summary(water_qual)

#loading in USGS data so that we can combine streamflow data with the water quality data 
siteNo <- "01018035"  #Lowery bridge

# For a full list of pCodes see
# #https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%
# 00060 is discharge 00010 is stream temperature in Celcius
pCodes <- c("00060", "00010")

start.date <- "2022-01-01"
end.date <- "2022-12-31"

#Now we can use pipelines to streamline, have to rerun the initial code to get the data in 
streamflow <- readNWISuv(siteNumbers = siteNo, parameterCd = pCodes, startDate = start.date,
                         endDate = end.date) #Can also read the data in like this below

streamflow <- streamflow %>%
  renameNWISColumns() %>%
  rename(DateTime = dateTime) %>%
  select(DateTime, Flow_Inst)

# For water quality stats 
wq.stats <- water_qual %>%
  mutate(Month = month(DateTime)) %>%
  group_by(Month) %>%
  mutate(TempAbove22.2 = (Temperature_C > 22.2), TempAbove25.6 = (Temperature_C >25.6), 
         pHBelow7 = (pH_pH < 7), 
         pHAbove8.5 = (pH_pH > 8.5), 
         DOBelow7 = (`Optical DO_mg/l` < 7)) %>%
  summarize(TempAbove22.2 = sum(TempAbove22.2, na.rm = TRUE), 
            TempAbove25.6 = sum(TempAbove25.6,na.rm = TRUE), 
            pHBelow7 = sum(pHBelow7, na.rm = TRUE), 
            pHAbove8.5 = sum(pHAbove8.5,na.rm = TRUE), 
            DOBelow7 = sum(DOBelow7, na.rm = TRUE))
kable(wq.stats)

#Making plots!!
ggplot(water_qual, aes(x=DateTime, y=`Optical DO_%`)) +
  geom_line() +
  xlab("Time") + ylab("Optical DO (%)")

#Joining datasets 
combined <- full_join(water_qual, streamflow, by="DateTime")

#left_join is good for combining two datasets when you want to see what data is paired between two datasets
combined <- left_join(water_qual, streamflow, by="DateTime")

ggplot(combined, aes(x=DateTime)) +
  geom_line(aes(y=Flow_Inst)) +
  geom_line(aes(y=Temperature_C)) +
  geom_line(aes(y=`Specific Cond_mS/cm`)) +
  geom_line(aes(y=pH_pH)) +
  geom_line(aes(y=`Optical DO_%`)) +
  xlab("Time")

ggplot(combined, aes(x=DateTime)) +
  geom_line(aes(y=Flow_Inst, color = "green")) +
  geom_line(aes(y=Temperature_C, color="blue")) +
  geom_line(aes(y=`Specific Cond_mS/cm`, color="red")) +
  geom_line(aes(y=pH_pH, color="orange")) +
  geom_line(aes(y=`Optical DO_%`)) +
  xlab("Time")

#Pivoting 
combined_long <- pivot_longer(combined, cols = -c("DateTime"), 
                              names_to = "Parameter",
                              values_to = "Values")
#Plotting the combined data 
ggplot(combined_long, aes(x=DateTime, y=Values, color=Parameter)) +
  geom_line() +
  xlab("Time")

#making a facet 
ggplot(combined_long, aes(x=DateTime, y=Values)) +
  geom_line() +
  xlab("Time") +
  facet_wrap(.~Parameter, scales="free") +
  theme(legend.position = "none")

#making a facet grid
ggplot(combined_long, aes(x=DateTime, y=Values, color=Parameter)) +
  geom_line() +
  xlab("Time") +
  facet_grid(Parameter~., scales="free") +
  theme(legend.position = "none")

param.labs <- c("Stream flow (cf/s)", "Optical DO (%)", "Optical DO (mg/l)",
                "pH", "Spec. Conductivity (mS/cm)", "Temperature (Celcius)")
names(param.labs) <- levels(combined_long$Parameter)

final.plot = ggplot(combined_long, aes(x=DateTime, y=Values, color=Parameter)) +
  geom_line() +
  xlab("Time") +
  facet_grid(Parameter~., scales="free",
             labeller = labeller(Parameter = param.labs, # rename facet labels
                                 group = label_wrap_gen(width=50))) +
  theme_minimal() + # change theme
  theme(legend.position = "none")
print(final.plot)
