######################################
#
# analyze Maxar imagery for Etosha Heights 
# acquired from Jared Stabach
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

#set working directory depending on which computer being used
ifelse(Sys.info()['sysname'] == "Darwin",
       setwd("/Users/mloranty/Library/CloudStorage/GoogleDrive-mloranty@colgate.edu/My Drive/Documents/research/giraffe"),
       setwd("G:/My Drive/Documents/research/giraffe"))

# note we have two large swaths of worldvies 2 & 3 images, each with panchromatic and multispectral imagery
list.files(path = "L:/data_repo/gis_data/etosha_heights_maxar/Maxar", 
           pattern = glob2rx("*.TIF"),
           recursive = T, 
           full.names = T)