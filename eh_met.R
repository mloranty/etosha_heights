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

#set working directory depending on which computer being used
ifelse(Sys.info()['sysname'] == "Darwin",
       setwd("/Users/mloranty/Library/CloudStorage/GoogleDrive-mloranty@colgate.edu/My Drive/Documents/research/giraffe"),
       setwd("G:/My Drive/Documents/research/giraffe"))

#setwd("L:/projects/etosha_heights/")

# list data files
df <- list.files(path = "eh_met_data/raw_data/csv", pattern = ".csv", full.names = T)

#list logger id for each file
lgr <- substr(list.files(path = "eh_met_data/raw_data/csv", pattern = ".csv"),4,8)

# test some code
met <- read.csv(df[1], skip = 2, header = T)

# add logger id column/var  
met$logger <- lgr[1]

# read the rest of files and append them to the data frame
for(i in 2:length(df))
{
  t <- read.csv(df[i], skip = 2, header = T, na.strings = "#N/A")
  t$logger <- lgr[i]
  
  # append to data frame
  met <- bind_rows(met,t)
}

rm(t)

# create timestamp 
met$tmstmp <- strptime(met$Timestamp, format = "%m/%d/%y %H:%M")

# create date vars
met$year <- year(met$tmstmp)
met$yday <- yday(met$tmstmp)

# add site name
c("Vlakwater", "Witgat", "Serengeti", "Swartposdam", "Oupos", "Lion Lodge")

# make data frames for key met vars to start
prcp <- met %>%
        select(Timestamp, tmstmp, year, jday, logger, mm.Precipitation) %>%
        distinct()

prcp.day <- prcp %>%
  group_by(logger,year,jday) %>%
  summarise(precip.mm = sum(mm.Precipitation, na.rm = T))
