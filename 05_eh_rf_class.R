######################################
#
# map Etosha Heights veg classes
# using Planet Imagery
#
# see eh_planet
# MML 12/05/2023
######################################

rm(list = ls())
# need to run the following to get GDAL on the supercomputer
#  . /local/gdal/bin/activate
# and then conda deactivate 

#load required packages
library(terra)
library(tidyverse)
library(patchwork)
#library(tidyterra)
library(caret)
library(randomForest)
#set working directory

#set working directory depending on which computer being used
ifelse(Sys.info()['sysname'] == "Darwin",
       setwd("/Users/mloranty/Library/CloudStorage/GoogleDrive-mloranty@colgate.edu/My Drive/Documents/research/giraffe"),
       setwd("L:/projects/etosha_heights/"))
 #     setwd("G:/My Drive/Documents/research/giraffe"))

#------------------------------------------------------------------------------------#
# read in the stacks of surface reflectance data along with sample plot polygons
#------------------------------------------------------------------------------------#
srf <- list.files(path = "L:/projects/etosha_heights/eh_planet/sr_resample/", 
                  pattern = glob2rx("EH*8b*sr*composite_resample.tif"), 
                  recursive = T, 
                  full.names = T)

# surface reflectance files have different extents, so just reading individually for now
sr <- rast(srf)

#------------------------------------------------------------------------------------#
# read in the stacks of vi data along with sample plot polygons
#------------------------------------------------------------------------------------#
eh.evi <- rast(list.files(path = "eh_planet/evi/", pattern = glob2rx("*.tif"), full.names = T))
eh.savi <- rast(list.files(path = "eh_planet/savi/", pattern = glob2rx("*.tif"), full.names = T))
eh.nirv <- rast(list.files(path = "eh_planet/nirv/", pattern = glob2rx("*.tif"), full.names = T))
eh.ndvi <- rast(list.files(path = "eh_planet/ndvi/", pattern = glob2rx("*.tif"), full.names = T))

# set layer names 
names(eh.evi) <- c(substr(list.files(path = "eh_planet/evi/",pattern = glob2rx("*.tif")),1,11))
names(eh.savi) <- c(substr(list.files(path = "eh_planet/savi/",pattern = glob2rx("*.tif")),1,11))
names(eh.nirv) <- c(substr(list.files(path = "eh_planet/nirv/",pattern = glob2rx("*.tif")),1,11))
names(eh.ndvi) <- c(substr(list.files(path = "eh_planet/ndvi/",pattern = glob2rx("*.tif")),1,11))

#------------------------------------------------------------------------------------#
# cleanup veg data
#------------------------------------------------------------------------------------#
veg.p <- vect("eh_veg_data/DB_EtoshaHeights_VegTransects_5m_buffer.shp")
veg.p <- project(veg.p, eh.evi) # not sure why this didn't carry through from the processing step...
veg.p <- veg.p[,1:3]  # retain only necessary rows
names(veg.p) <- c("assoc", "class", "releve")
veg.p$ID <- 1:nrow(veg.p) # add an ID column

veg.p$hab <- case_when(        # create variable for habitat type
  veg.p$assoc %in% c(1:4) ~ 1, # Mountain
  veg.p$assoc %in% c(5:7) ~ 2, # Wetland 
  veg.p$assoc > 7 ~ 3)         # Savanna

#------------------------------------------------------------------------------------#
# read in extracted vi values and append veg classes
#------------------------------------------------------------------------------------#
#p.evi <- read.csv("eh_vi_extracts/eh_plot_pix_evi.csv", header = T)
p.ndvi <- read.csv("eh_vi_extracts/eh_plot_pix_ndvi.csv", header = T)
#p.nir <- read.csv("eh_vi_extracts/eh_plot_pix_nirv.csv", header = T)
#p.savi <- read.csv("eh_vi_extracts/eh_plot_pix_savi.csv", header = T)

#------------------------------------------------------------------------------------#
# RANDOM FOREST CLASSIFICATION
#------------------------------------------------------------------------------------#
rfd <- psr
rfd <- p.evi
rfd <- p.ndvi

# select which layers to use as predictors (column IDs in extracted data)
lyr <- c(2:6,8:16,18:28)

rec <- nrow(rfd)
# IS ths necessary, or just to help keep track of things? 
#landClass <- data.frame(lcID = 1:3,
#                      landCover = c ("ground", "tall", "water"))

# specify training and validation data sets
set.seed(11213)

# randomly select half of the records
sampleSamp <- sample(seq(1,rec),rec/2)

rfd$sampleType <- "train"

rfd$sampleType[sampleSamp] <- "valid"


# create training and validation subsets
trainD <- rfd[rfd$sampleType=="train",]
validD<- rfd[rfd$sampleType=="valid",]


#------------Random Forest Classification of RGB data------------#
# run the Random Forest model
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:5) # number of variables available for splitting at each tree node

rf_model <- caret::train(x = trainD[,c(lyr)], #digital number data
                         y = as.factor(trainD$hab), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid

rf_model_cl <- caret::train(x = trainD[,c(lyr)], #digital number data
                         y = as.factor(trainD$assoc), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model
rf_model_cl


saveRDS(rf_model_cl, "eh_rf_models/ndvi_rf.rds")

mod_test <- readRDS("eh_rf_models/ndvi_rf_test.rds")
# evaluate validation data
confusionMatrix(predict(rf_model,validD[,lyr]),as.factor(validD$hab))
confusionMatrix(predict(rf_model_cl,validD[,lyr]),as.factor(validD$assoc))

# apply RF model to study site data
sv <- eh.ndvi
rf_prediction_cl <- predict(eh.ndvi[[-c(6,16)]], rf_model_cl, na.rm = T,
                         filename = "eh_rf_predictions/eh_rf_hab_ndvi.tif",
                         overwrite = T, progress = T)

 
#plot the data
plot(rf_prediction_cl)


map.cl <- ggplot() +
  geom_spatraster(data = rf_prediction_cl)
ggsave("sawma_figures/agu_rf_class_prediction_map_ndvi.png", width = 18, heigh = 6, units = "in")
