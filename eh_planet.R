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
setwd("G:/My Drive/Documents/research/giraffe")

# define evi function for 8-band planet images
# note the reflectance values are multiplied by 10000
evi <- function(x){
  b <- x[[2]]/10000
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  2.5*((n-r)/(n+6*r-7.5*b+1))
}

# note water year begins on july 1 in southern hemisphere
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

t <- rast(mos[1])

u <- rast(mos[2])
compa