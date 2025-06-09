######################################
#
# analyze Etosha Heights micromet data
#
# MML 09/09/2024
######################################

# clear workspace
rm(list = ls())

# load required packages
library(tidyverse)
library(lubridate)

#set working directory depending on which computer being used
ifelse(Sys.info()['sysname'] == "Darwin",
       setwd("/Users/mloranty/Library/CloudStorage/GoogleDrive-mloranty@colgate.edu/My Drive/Documents/research/giraffe"),
       setwd("G:/My Drive/Documents/research/giraffe"))


# READ DATA
# list data files
df <- list.files(path = "eh_met_data/raw_data/csv", pattern = ".csv", full.names = T)

#list logger id for each file
lgr <- substr(list.files(path = "eh_met_data/raw_data/csv", pattern = ".csv"),1,8)

# test some code
met <- read.csv(df[1], skip = 2, header = T)

# add logger id column/var  
met$loggerID <- lgr[1]

# read the rest of files and append them to the data frame
for(i in 2:length(df))
{
  t <- read.csv(df[i], skip = 2, header = T, na.strings = "#N/A")
  t$loggerID <- lgr[i]
  
  # append to data frame
  met <- bind_rows(met,t)
}

rm(t)

# create timestamp 
met$ts <- strptime(met$Timestamp, format = "%m/%d/%y %H:%M")

# create date vars - omitting this for now
# met$year <- year(met$tmstmp)
# met$jday <- yday(met$tmstmp)
# met$hour <- hour(met$tmstmp)
# met$min <- minute(met$tmstmp)

# create vector to cleanup column names 
lookup <- c(solar.rad = "W.m..Solar.Radiation", 
            precip = "mm.Precipitation", 
            ltn = "Lightning.Activity", 
            ltn.dist = "km.Lightning.Distance", 
            wind.dir = "X..Wind.Direction", 
            wind.spd = "m.s.Wind.Speed", 
            gust.spd = "m.s.Gust.Speed", 
            t.air = "X.C.Air.Temperature", 
            rh = "RH.Relative.Humidity", 
            atm = "kPa.Atmospheric.Pressure", 
            lev.x = "X..X.axis.Level", 
            lev.y = "X..Y.axis.Level", 
            precip.max = "mm.h.Max.Precip.Rate", 
            rh.t = "X.C.RH.Sensor.Temp", 
            batt.pct = "X..Battery.Percent", 
            batt = "mV.Battery.Voltage", 
            ref.atm = "kPa.Reference.Pressure", 
            t.logger = "X.C.Logger.Temperature")

# cleanup the data frame
edi <- met %>%
  distinct() %>% # remove duplicate rows (from downloads that include all stored data)
  select(-c("kPa.Vapor.Pressure", "Timestamp")) %>% # omit erroneous vapor pressure and redundant time stamp vars
  rename(all_of(lookup)) %>%
  arrange(loggerID, ts) %>% 
  relocate(ts) %>%
  relocate(loggerID)


# write to csv file
write.csv(edi, file = paste("eh_met_data/eh_met_data_",Sys.Date(),".csv", sep = ""), row.names = F)


