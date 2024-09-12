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
site <- data.frame(site =c("Vlakwater", "Witgat", "Serengeti", "Swartposdam", "Oupos", "Lion Lodge"),
                   logger = c("22569", "22572", "22575", "22576", "22583", "22584"))

# make data frames for key met vars to start
prcp <- met %>%
        select(Timestamp, tmstmp, year, jday, logger, mm.Precipitation) %>%
        distinct()

ta <- met %>%
  select(Timestamp, tmstmp, year, jday, logger, X.C.Air.Temperature) %>%
  distinct()

# aggregate to daily data
prcp.day <- prcp %>%
  group_by(logger,year,jday) %>%
  summarise(precip.mm = sum(mm.Precipitation, na.rm = T))

ta.day <- ta %>%
  group_by(logger,year,jday) %>%
  summarise(airTemp = mean(X.C.Air.Temperature, na.rm = T))

# add timestamp
prcp.day$tmstmp <- strptime(paste(prcp.day$year,prcp.day$jday,sep=""),
                            format = "%Y%j")

ta.day$tmstmp <- strptime(paste(ta.day$year,ta.day$jday,sep=""),
                            format = "%Y%j")

# add site as a variable, for convenience
prcp.day <- full_join(prcp.day, site)
ta.day <- full_join(ta.day, site)


# make a plot of temp and precip
