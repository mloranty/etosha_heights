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







