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


mos <- list.files(pattern = "composite.tif", recursive = T, full.names = T)

t <- rast(mos[1])

u <- rast(mos[2])
compa