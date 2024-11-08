######################################
#
# analyze phenology for Etosha Heights 
# using Planet Imagery
#
# see eh_planet
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

#set working directory 
setwd("L:/projects/etosha_heights/eh_planet_mosaic_tests/")
# define evi function for 8-band planet images
# note the reflectance values are divided by 10000
evi <- function(x){
  b <- x[[2]]/10000
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  2.5*((n-r)/(n+6*r-7.5*b+1))
}

# define near infrared reflectance of vegetation for 8-band planet images
#  Badgley et al 2017
# note the reflectance values are divided by 10000

ndvi <- function(x){
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  ((n-r)/(n+r))
}
# get list of files
f <- list.files(path = "EH_20240519-nh_psscene_analytic_8b_sr_udm2/",
                patter = glob2rx("*Analytic*.tif"), 
                full.names = T,
                recursive = T)

#list(paste("r", 1:9, sep = ""))
ehr <- vector("list", length(f))

# read in raster image
for(i in 1:length(f)) {
  ehr[[i]] <- rast(f[i])
  #assign(paste("r",i, sep = ""),x)
 # rm(x)
}

# make a spatial raster collection from the list
ehi <- sprc(ehr)

# masaic using the spatial raster collection
ehm <- mosaic(ehi, fun = "mean",
              filename = "EH_20240519_mosaic_test.tif")


ehn <- ndvi(ehm)
writeRaster(ehn, filename = "EH_20240519_mosaic_test_ndvi.tif" )

ehe <- evi(ehm)
writeRaster(ehe, filename = "EH_20240519_mosaic_test_evi.tif" )

