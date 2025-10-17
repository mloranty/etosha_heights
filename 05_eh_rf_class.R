######################################
#
# map Etosha Heights veg classes
# using Planet Imagery
#
# see eh_planet
# MML 10/15/2025
######################################

rm(list = ls())
# need to run the following to get GDAL on the supercomputer
#  . /local/gdal/bin/activate
# and then conda deactivate 

#load required packages
library(terra)
library(tidyverse)
library(patchwork)
library(caret)
library(randomForest)
#set working directory

#set working directory depending on which computer being used
ifelse(Sys.info()['sysname'] == "Darwin",
       setwd("/Users/mloranty/Library/CloudStorage/GoogleDrive-mloranty@colgate.edu/My Drive/Documents/research/giraffe"),
       setwd("L:/projects/etosha_heights/"))
#     setwd("G:/My Drive/Documents/research/giraffe"))




# list all surface reflectance mosaics
srf <- list.files(path = "L:/projects/etosha_heights/eh_planet/sr_resample/", 
                  pattern = glob2rx("EH*8b*sr*composite_resample.tif"), 
                  recursive = T, 
                  full.names = T)

# read in the first as a reference
d <- rast(srf[1])

# read and cleanup veg data
veg.p <- project(vect("L:/projects/etosha_heights/eh_veg_data/DB_EtoshaHeights_VegTransects_5m_buffer.shp"),d)[,1:3]
names(veg.p) <- c("assoc", "class", "releve")
veg.p$ID <- 1:nrow(veg.p) # add an ID column

# read in NDVI data for plots - see 03_eh_planet_vi_extract.R
plt_nd <- read.csv("eh_vi_extracts/eh_plot_pix_ndvi.csv", header = T)

#-----------------------------------------------------------------------------------------------------------------------------#
#------------------------ RF1 - fit a random forest model to differing numbers of seasonal NDVI mosaics ----------------------#
#-----------------------------------------------------------------------------------------------------------------------------#


# create an empty lists for rf model outputs
rf1_mod <- list()
rf1_con <- list()

# loop through each file and fit a model
for(i in 1:12)
{
  set.seed(11213)
  
  # select which dates to use
  var <- seq(2,26,i)
  
  # specify training and validation data sets
  rec <- nrow(plt_nd)
  
  # randomly select half of the records
  sampleSamp <- sample(seq(1,rec),rec/2)
  
  plt_nd$sampleType <- "train"
  
  plt_nd$sampleType[sampleSamp] <- "valid"
  
  
  # create training and validation subsets
  trainD <- plt_nd[plt_nd$sampleType=="train",]
  validD<- plt_nd[plt_nd$sampleType=="valid",]
  
  #------------Random Forest Classification of reflectance data------------#
  # run the Random Forest model
  tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                     number = 10, # number 10 fold
                     repeats = 10) # number of repeats
  ###random forests
  #Typically square root of number of variables
  rf.grid <- expand.grid(mtry=1:round(sqrt(length(var)))) # number of variables available for splitting at each tree node
  
  rf_model <- caret::train(x = trainD[,var], #digital number data
                           y = as.factor(trainD$assoc), #land class we want to predict
                           method = "rf", #use random forest
                           metric="Accuracy", #assess by accuracy
                           importance = TRUE, # calculate variable importance
                           trControl = tc, #use parameter tuning method
                           tuneGrid = rf.grid) #parameter tuning grid
  
  rf1_mod[[i]] <- rf_model
  rf1_con[[i]] <- confusionMatrix(predict(rf_model,validD[,var]),as.factor(validD$assoc))
}

rf1_sum <- rfv_con[[1]]$overall

for(i in 2:length(rfv_con))
{
  rf1_sum <- rbind(rf1_sum,rf1_con[[i]]$overall )
}

#---------------------------------------------------------------------------------------------------------------------------------------#
#------------------------ RF2 - fit a random forest model to best seasonal NDVI mosaics without atmos corr issues ----------------------#
#---------------------------------------------------------------------------------------------------------------------------------------#
var <- c(6:16,18:25)

# specify training and validation data sets
rec <- nrow(plt_nd)

# randomly select half of the records
sampleSamp <- sample(seq(1,rec),rec/2)

plt_nd$sampleType <- "train"

plt_nd$sampleType[sampleSamp] <- "valid"


# create training and validation subsets
trainD <- plt_nd[plt_nd$sampleType=="train",]
validD<- plt_nd[plt_nd$sampleType=="valid",]

#------------Random Forest Classification of reflectance data------------#
# run the Random Forest model
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:round(sqrt(length(var)))) # number of variables available for splitting at each tree node

rf2_mod <- caret::train(x = trainD[,var], #digital number data
                         y = as.factor(trainD$assoc), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         importance = TRUE, # calculate variable importance
                         trControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid

#------------------------------------------------------------------------------------------------------------------#
#------------------------ RF3 - fit a random forest model to each surface reflectance mosaic ----------------------#
#------------------------------------------------------------------------------------------------------------------#

# create an empty list for rf model outputs
rf3_mod <- list()
rf3_con <- list()

# loop through each file and fit a model
for(i in 1:length(srf))
{
  # read in the reflectance file
  d <- rast(srf[i])
  
  # simplify band names
  names(d) <- paste("B", 1:8, sep = "")
  
  # extract pixel values
  sr <- terra::extract(d, veg.p, FUN = NULL)
  
  # join with other variables from veg.p
  psr <- full_join(sr, as.data.frame(veg.p)) %>%
    na.omit()
  rm(sr)
  
  rec <- nrow(psr)
  # specify training and validation data sets
  set.seed(11213)
  
  # randomly select half of the records
  sampleSamp <- sample(seq(1,rec),rec/2)
  
  psr$sampleType <- "train"
  
  psr$sampleType[sampleSamp] <- "valid"
  
  
  # create training and validation subsets
  trainD <- psr[psr$sampleType=="train",]
  validD<- psr[psr$sampleType=="valid",]
  
  #------------Random Forest Classification of reflectance data------------#
  # run the Random Forest model
  tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                     number = 10, # number 10 fold
                     repeats = 10) # number of repeats
  ###random forests
  #Typically square root of number of variables
  rf.grid <- expand.grid(mtry=1:3) # number of variables available for splitting at each tree node
  
  rf_model <- caret::train(x = trainD[,2:9], #digital number data
                           y = as.factor(trainD$assoc), #land class we want to predict
                           method = "rf", #use random forest
                           metric="Accuracy", #assess by accuracy
                           importance = TRUE, # calculate variable importance
                           trControl = tc, #use parameter tuning method
                           tuneGrid = rf.grid) #parameter tuning grid
  
  rf3_mod[[i]] <- rf_model
  rf3_con[[i]] <- confusionMatrix(predict(rf_model,validD[,2:9]),as.factor(validD$assoc))
}

rf3_sum <- rf3_con[[1]]$overall

for(i in 2:length(rf3_con))
{
  rf3_sum <- rbind(rf3_sum,rf3_con[[i]]$overall )
}

rf_rf3_sum <- as.data.frame(rf3_sum)
rf3_sum$file <- srf

save(rf3_mod, rf3_con, rf3_sum, file = "rf3_results.RData")
#-------------------------------------------------------------------------------------------------------------------------#
#------------------------ RF4 - fit a random forest model to each surface reflectance mosaic + NDVI ----------------------#
#---------------------------------------------------------------------------------------------------      ----------------#
# create an empty list for rf model outputs
rf4_mod <- list()
rf4_con <- list()

# loop through each file and fit a model
for(i in 1:length(srf))
{
  # read in the reflectance file
  d <- rast(srf[i])
  
  # simplify band names
  names(d) <- paste("B", 1:8, sep = "")
  
  # extract pixel values
  sr <- terra::extract(d, veg.p, FUN = NULL)
  
  # calculate ndvi
  sr$ndvi <- (sr$B8-sr$B6)/(sr$B8+sr$B6)
  
  # join with other variables from veg.p
  psr <- full_join(sr, as.data.frame(veg.p)) %>%
    na.omit()
  rm(sr)
  
  rec <- nrow(psr)
  # specify training and validation data sets
  set.seed(11213)
  
  # randomly select half of the records
  sampleSamp <- sample(seq(1,rec),rec/2)
  
  psr$sampleType <- "train"
  
  psr$sampleType[sampleSamp] <- "valid"
  
  
  # create training and validation subsets
  trainD <- psr[psr$sampleType=="train",]
  validD<- psr[psr$sampleType=="valid",]
  
  #------------Random Forest Classification of reflectance data------------#
  # run the Random Forest model
  tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                     number = 10, # number 10 fold
                     repeats = 10) # number of repeats
  ###random forests
  #Typically square root of number of variables
  rf.grid <- expand.grid(mtry=1:3) # number of variables available for splitting at each tree node
  
  rf_model <- caret::train(x = trainD[,2:10], #digital number data
                           y = as.factor(trainD$assoc), #land class we want to predict
                           method = "rf", #use random forest
                           metric="Accuracy", #assess by accuracy
                           importance = TRUE, # calculate variable importance
                           trControl = tc, #use parameter tuning method
                           tuneGrid = rf.grid) #parameter tuning grid
  
  rf4_mod[[i]] <- rf_model
  rf4_con[[i]] <- confusionMatrix(predict(rf_model,validD[,2:10]),as.factor(validD$assoc))
}

rf4_sum <- rf4_con[[1]]$overall

for(i in 2:length(rf4_con))
{
  rf4_sum <- rbind(rf4_sum,rf4_con[[i]]$overall )
}

rf4_sum <- as.data.frame(rf4_sum)
rf4_sum$file <- srf

#-------------------------------------------------------------------------------------------------------------------#
#------------------------ fit a random forest model multiple surface reflectance mosaics ---------------------------#
#-------------------------------------------------------------------------------------------------------------------#

# read in the reflectance data
#d <- rast(srf[c(5,9,13,24)])

# simplify band names
names(d) <- c(paste("B", 1:8,".",1, sep = ""),
              paste("B", 1:8,".",2, sep = ""),
              paste("B", 1:8,".",3, sep = ""),
              paste("B", 1:8,".",4, sep = ""))

# extract pixel values
sr <- terra::extract(d, veg.p, FUN = NULL)


# join with other variables from veg.p
psr2 <- full_join(sr, as.data.frame(veg.p)) %>%
  na.omit()
rm(sr)

rec <- nrow(psr2)
# specify training and validation data sets
set.seed(11213)

# randomly select half of the records
sampleSamp <- sample(seq(1,rec),rec/2)

psr2$sampleType <- "train"

psr2$sampleType[sampleSamp] <- "valid"


# create training and validation subsets
trainD <- psr2[psr2$sampleType=="train",]
validD<- psr2[psr2$sampleType=="valid",]

#------------Random Forest Classification of reflectance data------------#
# run the Random Forest model
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:5) # number of variables available for splitting at each tree node

rf4_model <- caret::train(x = trainD[,2:33], #digital number data
                          y = as.factor(trainD$assoc), #land class we want to predict
                          method = "rf", #use random forest
                          metric="Accuracy", #assess by accuracy
                          importance = TRUE, # calculate variable importance
                          trControl = tc, #use parameter tuning method
                          tuneGrid = rf.grid) #parameter tuning grid

confusionMatrix(predict(rf2_model,validD[,2:17]),as.factor(validD$assoc))  
confusionMatrix(predict(rf3_model,validD[,2:25]),as.factor(validD$assoc))
confusionMatrix(predict(rf4_model,validD[,2:33]),as.factor(validD$assoc))

#-------------------------------------------------------------------------------------------------------------------#
#------------------------ fit a random forest model multiple surface reflectance mosaics ---------------------------#
#-------------------------------------------------------------------------------------------------------------------#



for(i in 1:10)
{
  # read in a random sample of 4 bandsreflectance data
  d <- rast(sample(srf,4))
  
  # simplify band names
  names(d) <- c(paste("B", 1:8,".",1, sep = ""),
                paste("B", 1:8,".",2, sep = ""),
                paste("B", 1:8,".",3, sep = ""),
                paste("B", 1:8,".",4, sep = ""))
  
  # extract pixel values
  sr <- terra::extract(d, veg.p, FUN = NULL)
  
  
  # join with other variables from veg.p
  psr <- full_join(sr, as.data.frame(veg.p)) %>%
    na.omit()
  rm(sr)
  
  rec <- nrow(psr)
  # specify training and validation data sets
  set.seed(11213)
  
  # randomly select half of the records
  sampleSamp <- sample(seq(1,rec),rec/2)
  
  psr$sampleType <- "train"
  
  psr$sampleType[sampleSamp] <- "valid"
  
  # create training and validation subsets
  trainD <- psr[psr$sampleType=="train",]
  validD<- psr[psr$sampleType=="valid",]
  
  #------------Random Forest Classification of reflectance data------------#
  # run the Random Forest model
  tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                     number = 10, # number 10 fold
                     repeats = 10) # number of repeats
  ###random forests
  #Typically square root of number of variables
  rf.grid <- expand.grid(mtry=1:5) # number of variables available for splitting at each tree node
  
  rf_model <- caret::train(x = trainD[,2:33], #digital number data
                            y = as.factor(trainD$assoc), #land class we want to predict
                            method = "rf", #use random forest
                            metric="Accuracy", #assess by accuracy
                            importance = TRUE, # calculate variable importance
                            trControl = tc, #use parameter tuning method
                            tuneGrid = rf.grid) #parameter tuning grid
  
  rfx_mod[[i]] <- rf_model
  rfx_con[[i]] <- confusionMatrix(predict(rf_model,validD[,2:33]),as.factor(validD$assoc))
  
  
}











confusionMatrix(predict(rf2_model,validD[,2:17]),as.factor(validD$assoc))  
confusionMatrix(predict(rf3_model,validD[,2:25]),as.factor(validD$assoc))
confusionMatrix(predict(rf4_model,validD[,2:33]),as.factor(validD$assoc))

#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#
#------------------------ save the best model(s) and make predictions ----------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#

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
