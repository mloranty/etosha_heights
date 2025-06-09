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

# create output filename
of <- paste(substr(d,1,11),
            "_8b_sr_composite.tif",
            sep = "")

for(i in 1:length(d))
{
  # change directory
  setwd(paste("../",d[i], sep = ""))
  
  
  if(file.exists("composite.tif") == T)
  {
    file.rename("composite.tif", of[i])
  }
  
  # see if the file does not exist create a mosaic
  # otherwise skip
  if(file.exists(of[i]) ==F)
  {
    # list strips to be mosaiced
    f <- as.list(list.files(pattern = glob2rx("*SR*.tif"), 
                            full.names = T,
                            recursive = T))
    
    # read as rast files and create spat raster collection
    l <- sprc(lapply(f,rast))
    
    # mosaic these files
    m <- mosaic(l, fun = "mean",
                filename = of[i])
    
    # remove unneeded objects
    rm(f,l,m)
  }      
}  



# ehn <- ndvi(ehm)
# writeRaster(ehn, filename = "EH_20240519_mosaic_test_ndvi.tif" )
# 
# ehe <- evi(ehm)
# writeRaster(ehe, filename = "EH_20240519_mosaic_test_evi.tif" )

