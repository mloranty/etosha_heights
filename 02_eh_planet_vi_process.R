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
mos <- list.files(path = "L:/projects/etosha_heights/eh_planet", 
                  pattern = glob2rx("EH*8b*sr*composite.tif"), 
                  recursive = T, 
                  full.names = T)

#extract the directory name to use for output filenaming and to get the date
f <- sapply(strsplit(mos,"/"), FUN = "[", 6)

# extract timestamp from directory name 
ts <- strptime(as.numeric(substr(f,4,11)), 
               format = "%Y%m%d", tz = "")


# evi output filenames
eo <- paste("eh_planet/evi/",str_replace(f,".tif","_evi.tif"), sep = "")

# savi output filenames
so <- paste("eh_planet/savi/",str_replace(f,".tif","_savi.tif"), sep = "")

# nirv output filenames
no <- paste("eh_planet/nirv/",str_replace(f,".tif","_nirv.tif"), sep = "")

# ndvi output filenames
nd <- paste("eh_planet/ndvi/",str_replace(f,".tif","_ndvi.tif"), sep = "")

# check to see which files exist to avoid unnecessary reprocessing
#p <- which(nd %in% list.files(path = "eh_planet/ndvi/", full.names = T)==F)

#reference raster to align everything to the same extent
ref <- rast("L:/projects/etosha_heights/eh_planet/EH-20230818-nh_psscene_analytic_8b_sr_udm2/EH-20230818_8b_sr_composite.tif")

# calculate evi and savi and write to file
# note we ran into some memory issues here
for(i in 1:length(mos))
{
  x <- rast(mos[i])
  
  #calculate and resample evi only if the file does not already exist
  if(file.exists(eo[i])==F)
  {
    e <- evi(x)
    e <- resample(e,ref, filename = eo[i], overwrite = T)
    rm(e)
  }
  
  if(file.exists(so[i])==F)
  {
    s <- savi(x)
    s <- resample(s,ref, filename = so[i], overwrite = T)
    rm(s)
  }
  
  if(file.exists(so[i])==F)
  {
    n <- nirv(x)
    n <- resample(n,ref, filename = no[i], overwrite = T)
    rm(n)
  }  
  
  if(file.exists(so[i])==F)
  {
    d <- ndvi(x)
    d <- resample(d,ref, filename = nd[i], overwrite = T)
    rm(d)
  }  
  
  tmpFiles(remove = T)
  gc()
}

# remove hidden temporary files
tmpFiles(remove = T)


#------------------------------------------------------#
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


#---------------------------------------------------------------------------------#
# resample surface reflectance mosaics 
#---------------------------------------------------------------------------------#
ref <- rast("L:/projects/etosha_heights/eh_planet/EH-20230818-nh_psscene_analytic_8b_sr_udm2/EH-20230818_8b_sr_composite.tif")

srf <- list.files(path = "L:/projects/etosha_heights/eh_planet", 
                  pattern = glob2rx("EH*8b*sr*composite.tif"), 
                  recursive = T, 
                  full.names = T)

f <- sapply(strsplit(srf,"/"), FUN = "[", 6)

f <- str_replace(f,"composite", "composite_resample")

sro <- paste("L:/projects/etosha_heights/eh_planet/sr_resample/",f, sep = "")

for(i in 1:length(srf))
{
  if(file.exists(sro[i])==F)
  {
    x <- rast(srf[i])
    resample(x,ref, filename = sro[i], overwrite = F, progress = T, method = "bilinear", threads = T)
    rm(x)
  }
  
}
#---------------------------------------------------------------------------------#


# rename the resampled surface reflectance files to avoid confusion
# srrs <- list.files(path = "L:/projects/etosha_heights/eh_planet/sr_resample/", 
#                    pattern = glob2rx("EH*8b*sr*composite.tif"), 
#                    recursive = T, 
#                    full.names = T)
# 
# o <- sapply(strsplit(srrs,"/"), FUN = "[", 6)
# 
# o <- str_replace(o,"composite", "composite_resample")
# 
# res_out <- paste("L:/projects/etosha_heights/eh_planet/sr_resample/",o, sep = "")
# 
# for (i in 1:length(res_out))
# {
#   file.rename(srrs[i],res_out[i])
# }

# rename the resampled surface reflectance files to avoid confusion
# vi <- list.files(path = "L:/projects/etosha_heights/eh_planet/",
#                    pattern = glob2rx("*_*v*.tif"),
#                    recursive = T,
#                    full.names = T)
# 
# 
# vio <- str_replace(vi,"-nh_psscene_analytic_8b_sr_udm2", "_8b_sr_composite")
# 
# 
# for (i in 1:length(vi))
# {
#   file.rename(vi[i],vio[i])
# }
