######################################
#
# mosaic strips of planet data for  
# Etosha Heights veg analysis
#
# see eh_planet
# MML 11/25/2024
######################################

rm(list = ls())

#load required packages
library(terra)
library(tidyverse)
library(patchwork)
library(tidyterra)

#set working directory

#set working directory 
setwd("L:/projects/etosha_heights/eh_planet")

# list all composite strips
# f <- list.files(pattern = glob2rx("*strip*composite.tif"), 
#                 full.names = T,
#                 recursive = T)

# list all clipped files
f <- list.files(pattern = glob2rx("*SR*.tif"), 
                full.names = T,
                recursive = T)
  
# get list of unique directories
d <- unique(sapply(strsplit(f,"/"), FUN = "[", 2))


for(i in 1:length(d))
{
  # change directory
  setwd(d[i])
  
  # list strips to be mosaiced
  f <- as.list(list.files(pattern = glob2rx("*SR*.tif"), 
                  full.names = T,
                  recursive = T))
  
  # read as rast files and create spat raster collection
  l <- sprc(lapply(f,rast))
  
  # mosaic these files
  m <- mosaic(l, fun = "mean",
              filename = "composite.tif")
  
  # move back to parent directory
  setwd("../")
  
  # remove unneeded objects
  rm(f,l,m)
}


# ehn <- ndvi(ehm)
# writeRaster(ehn, filename = "EH_20240519_mosaic_test_ndvi.tif" )
# 
# ehe <- evi(ehm)
# writeRaster(ehe, filename = "EH_20240519_mosaic_test_evi.tif" )

