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
library(patchwork)
library(tidyterra)
library(caret)
library(randomForest)
#set working directory
#setwd("G:/My Drive/Documents/research/giraffe")
setwd("L:/projects/etosha_heights/")

# define evi function for 8-band planet images
# note the reflectance values are divided by 10000
evi <- function(x){
  b <- x[[2]]/10000
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  2.5*((n-r)/(n+6*r-7.5*b+1))
}

# define soil adjusted vi (savi) function for 8-band planet images
# L = 0.5 after Huete et al 1988
# note the reflectance values are divided by 10000
savi <- function(x){
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  ((1.5*(n-r))/(n+r+0.5))
}

# define near infrared reflectance of vegetation for 8-band planet images
#  Badgley et al 2017
# note the reflectance values are divided by 10000
nirv <- function(x){
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  ((((n-r)/(n+r))-0.08)*r)
}

# define ndvi for 8-band planet images
ndvi <- function(x){
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  ((n-r)/(n+r))
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


# See Collapsed code for planet vi calcs, otherwise skip this section 
#------------------------------------------------------------------------------
# list all composite files for the temporal stack
mos <- list.files(path = "eh_planet", 
                  pattern = glob2rx("composite.tif"), 
                  recursive = T, 
                  full.names = T)

#extract the directory name to use for output filenaming and to get the date
f <- sapply(strsplit(mos,"/"), FUN = "[", 2)

# extract timestamp from directory name 
ts <- strptime(as.numeric(substr(f,4,11)), 
               format = "%Y%m%d", tz = "")


# evi output filenames
eo <- paste("eh_planet/evi/",f,"_evi.tif", sep = "")

# savi output filenames
so <- paste("eh_planet/savi/",f,"_savi.tif", sep = "")

# nirv output filenames
no <- paste("eh_planet/nirv/",f,"_nirv.tif", sep = "")

# ndvi output filenames
nd <- paste("eh_planet/ndvi/",f,"_ndvi.tif", sep = "")

# check to see which files exist to avoid unnecessary reprocessing
#p <- which(nd %in% list.files(path = "eh_planet/ndvi/", full.names = T)==F)

#reference raster to align everything to the same extent
ref <- rast(mos[1])

# calculate evi and savi and write to file
# note we ran into some memory issues here
for(i in 1:length(mos))
{
  x <- rast(mos[i])
  e <- evi(x)
  s <- savi(x)
  n <- nirv(x)
  d <- ndvi(x)
  e <- resample(e,ref, filename = eo[i], overwrite = T)
  s <- resample(s,ref, filename = so[i], overwrite = T)
  n <- resample(n,ref, filename = no[i], overwrite = T)
  d <- resample(d,ref, filename = nd[i], overwrite = T)
 # writeRaster(e,eo[i], overwrite = T)
 # writeRaster(s,so[i], overwrite = T)
  rm(x,s,e,n)
}

# remove hidden temporary files
tmpFiles(remove = T)

# read eh veg data for validation
vg <-read.csv("eh_veg_data/DB_EtoshaHeights_VegTransects.csv", 
              header = T)

# convert to spatial vector
veg <- vect(vg, geom = c("lon_dd", "lat_dd"), crs = "epsg:4326")

# buffer to 10m diameter based on Marufu thesis
veg.p <- buffer(veg, 5)
veg.p <- project(veg.p, ref)
writeVector(veg.p, filename = "eh_veg_data/DB_EtoshaHeights_VegTransects_5m_buffer.shp")

# calculate pre/post pulse vi change
# this didn't work, and also need to rethink these things a bit. 
# subset(eh.evi, "EH-20240410") - subset(eh.evi, "EH-20240315") %>%
#   writeRaster("eh_planet/evi/delta_evi_20240410_20240315.tif")
# 
# subset(eh.savi, "EH-20240410") - subset(eh.savi, "EH-20240315") %>%
#   writeRaster("eh_planet/savi/delta_savi_20240410_20240315.tif")
# 
# subset(eh.nirv, "EH-20240410") - subset(eh.nirv, "EH-20240315") %>%
#   writeRaster("eh_planet/nirv/delta_nirv_20240410_20240315.tif")

# copy vi files to Google Drive for analysis
system("cp -r eh_planet/evi \"G:/My Drive/Documents/research/giraffe/data/eh_planet\"")
system("cp -r eh_planet/savi \"G:/My Drive/Documents/research/giraffe/data/eh_planet\"")
system("cp -r eh_planet/nirv \"G:/My Drive/Documents/research/giraffe/data/eh_planet\"")
system("cp -r eh_planet/ndvi \"G:/My Drive/Documents/research/giraffe/data/eh_planet\"")


#-------------------------------------------------------------------------------------------

