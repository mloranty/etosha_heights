######################################
#
# analyze phenology for Etosha Heights 
# using Planet Imagery
#
# MML 08/10/2023
######################################

rm(list = ls())

#load required packages
library(terra)
library(tidyverse)

#set working directory
#setwd("G:/My Drive/Documents/research/giraffe")
setwd("L:/projects/etosha_heights/")

# define evi function for 8-band planet images
# note the reflectance values are multiplied by 10000
evi <- function(x){
  b <- x[[2]]/10000
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  2.5*((n-r)/(n+6*r-7.5*b+1))
}

# define soil adjusted vi (savi) function for 8-band planet images
# L = 0.5 after Huete et al 1988
# note the reflectance values are multiplied by 10000
savi <- function(x){
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  ((1.5*(n-r))/(n+r+0.5))
}

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

# list all composite files for the temporal stack
mos <- list.files(pattern = "composite.tif", recursive = T, full.names = T)

#extract the directory name to use for output filenaming and to get the date
 f <- sapply(strsplit(mos,"/"), FUN = "[", 3)

# extract timestamp from directory name 
ts <- strptime(as.numeric(substr(f,4,11)), 
               format = "%Y%m%d", tz = "")


# evi output filenames
eo <- paste("eh_planet/evi/",f,"_evi.tif", sep = "")

# evi output filenames
so <- paste("eh_planet/savi/",f,"_savi.tif", sep = "")


# calculate evi and savi and write to file

for(i in 1:length(mos))
{
  x <- rast(mos[i])
  e <- evi(x)
  s <- savi(x)
  writeRaster(e,eo[i], overwrite = T)
  writeRaster(s,so[i], overwrite = T)
  rm(x,s,e)
}





wd(ts)
wy(ts)

t <- rast(mos[1])
et <- evi(t)
st <- savi(t)

u <- rast(mos[2])
compa