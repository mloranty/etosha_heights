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

# DEFINE FUNCTIONS TO CALCULATE WATER DAY/YEAR
# note water year begins on July 1 in southern hemisphere
# determine water year from timestamp
wy <- function(x)
{
  #ifelse(is.POSIXct(x),,"Input is not Date/Time class")
  ifelse(month(x)<7,year(x),year(x)+1)
}

# calculate water day from timestamp
wd <- function(x)
{
  ifelse(leap_year(x),
         ifelse(month(x)<7,yday(x)+184,yday(x)-182),
         ifelse(month(x)<7,yday(x)+184,yday(x)-181))
}

# READ DATA
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
met$jday <- yday(met$tmstmp)

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

# add site as a variable, for convenience
prcp.day <- full_join(prcp.day, site)
ta.day <- full_join(ta.day, site)

# add timestamp. water year & water day
prcp.day$tmstmp <- as.POSIXct(strptime(paste(prcp.day$year,prcp.day$jday,sep=""),
                            format = "%Y%j"))
prcp.day$wy <- wy(prcp.day$tmstmp)
prcp.day$wday <- wd(prcp.day$tmstmp)


prcp.day <- prcp.day %>% group_by(site, wy) %>% arrange(tmstmp) %>% mutate(precip.cum = cumsum(precip.mm))
  
write.csv(prcp.day, file = "eh_met_data/daily_precip.csv", row.names = F)

ta.day$tmstmp <- as.POSIXct(strptime(paste(ta.day$year,ta.day$jday,sep=""),
                            format = "%Y%j"))




# make plots of temp and precip

ggplot(data = prcp.day,
       aes(x = tmstmp, y = precip.mm, group = site, fill = site)) +
geom_bar(stat = "identity", position = "dodge")

ggplot(data = ta.day,
       aes(x = tmstmp, y = airTemp, group = site, color = site)) +
geom_line()

ggplot(data = prcp.day,
       aes(x = tmstmp, y = precip.cum, group = site, color = site)) +
  geom_line()

# ordered data frame to identify at high precip days
pd2 <- prcp.day %>% arrange(desc(precip.mm))