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

#set working directory depending on which computer being used
ifelse(Sys.info()['sysname'] == "Darwin",
       setwd("/Users/mloranty/Library/CloudStorage/GoogleDrive-mloranty@colgate.edu/My Drive/Documents/research/giraffe"),
       setwd("G:/My Drive/Documents/research/giraffe"))

# read in the stacks of evi and savi data along with sample plot polygons
veg.p <- vect("eh_veg_data/DB_EtoshaHeights_VegTransects_5m_buffer.shp")
eh.evi <- rast(list.files(path = "eh_planet/evi/", pattern = glob2rx("*.tif"), full.names = T))
eh.savi <- rast(list.files(path = "eh_planet/savi/", pattern = glob2rx("*.tif"), full.names = T))
eh.nirv <- rast(list.files(path = "eh_planet/nirv/", pattern = glob2rx("*.tif"), full.names = T))

# set layer names 
names(eh.evi) <- c(substr(list.files(path = "eh_planet/evi/",pattern = glob2rx("*.tif")),1,11))
names(eh.savi) <- c(substr(list.files(path = "eh_planet/savi/",pattern = glob2rx("*.tif")),1,11))
names(eh.nirv) <- c(substr(list.files(path = "eh_planet/nirv/",pattern = glob2rx("*.tif")),1,11))

# not sure why this didn't carry through from the processing step...
veg.p <- project(veg.p, eh.evi)

# extract evi and savi values for each plot
# plt.evi <- terra::extract(eh.evi[[1]],veg.p, weights = T)
# plt.savi <- terra::extract(eh.savi,veg.p)
plt.evi <- zonal(eh.evi,veg.p, fun = "mean", na.rm = T)
plt.savi <- zonal(eh.savi,veg.p, fun = "mean", na.rm = T)
plt.nirv <- zonal(eh.nirv,veg.p, fun = "mean", na.rm = T)

# set column headers based on file names
#colnames(plt.evi) <- c("ID", substr(list.files(path = "eh_planet/evi/"),1,11)) # differs based on zonal vs. extract
# colnames(plt.evi) <- c(substr(list.files(path = "eh_planet/evi/",pattern = glob2rx("*.tif")),1,11))
# colnames(plt.savi) <- c(substr(list.files(path = "eh_planet/savi/",pattern = glob2rx("*.tif")),1,11))



# add variables for analysis/plotting
plt.evi <- cbind(plt.evi, veg.p[,1:6])
plt.savi <- cbind(plt.savi, veg.p[,1:6])
plt.nirv <- cbind(plt.nirv, veg.p[,1:6])
# pivot longer 
pel <- pivot_longer(plt.evi, cols = starts_with("EH"))
psl <- pivot_longer(plt.savi, cols = starts_with("EH"))
pnl <- pivot_longer(plt.nirv, cols = starts_with("EH"))

# change varibale names
pel <- rename(pel, evi = value)
psl <- rename(psl, savi = value)
pnl <- rename(pnl, nirv = value)
# join these dataframes
plt.vi <- full_join(pel, psl)
plt.vi <- full_join(plt.vi, pnl)
#rm(pel,psl,plt.evi,plt.savi)

# add time stamp here
plt.vi$timestamp <- strptime(as.numeric(substr(plt.vi$name,4,11)), 
                             format = "%Y%m%d")

# create a factor for veg community
plt.vi$VegComm <- as.factor(plt.vi$Associati0)

# group by veg class
vcl <- plt.vi %>%
  group_by(VegComm,timestamp) %>%
  summarise(evi = mean(evi, na.rm = T),
            svi = mean(savi, na.rm = T),
            nrv = mean(nirv, na.rm = T),
            elev_m = mean(elev_m, na.rm = T))

# can only plot with POSIXct class
vcl$ts <- as.POSIXct(vcl$timestamp)

# plot evi timeseries
e <- ggplot(data = vcl, aes(x = ts, y = evi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  scale_fill_discrete()

# plot savi time series
s <- ggplot(data = vcl, aes(x = ts, y = svi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  scale_fill_discrete()

n <- ggplot(data = vcl, aes(x = ts, y = nrv, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  scale_fill_discrete()


# make boxplots of mean January vi for 2023 & 2024
sj23 <- plt.vi %>%
  filter(month(timestamp) == 1 & year(timestamp) == 2023) %>% 
  ggplot(aes(x = VegComm, y = savi, color = VegComm)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA) + 
  ylim(c(0.1,0.35))

ej23 <- plt.vi %>%
  filter(month(timestamp) == 1 & year(timestamp) == 2023) %>% 
  ggplot(aes(x = VegComm, y = evi, color = VegComm)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA) + 
  ylim(c(0.1,0.35))

sj24 <- plt.vi %>%
  filter(month(timestamp) == 1 & year(timestamp) == 2024) %>% 
  ggplot(aes(x = VegComm, y = savi, color = VegComm)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA) + 
  ylim(c(0.1,0.35))

ej24 <- plt.vi %>%
  filter(month(timestamp) == 1 & year(timestamp) == 2024) %>% 
  ggplot(aes(x = VegComm, y = evi, color = VegComm)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA) + 
  ylim(c(0.1,0.35))

ej24 <- plt.vi %>%
  filter(month(timestamp) == 1 & year(timestamp) == 2024) %>% 
  ggplot(aes(x = VegComm, y = evi, color = VegComm)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA) + 
  ylim(c(0.1,0.35))

nj24 <- plt.vi %>%
  filter(month(timestamp) == 1 & year(timestamp) == 2024) %>% 
  ggplot(aes(x = VegComm, y = nirv, color = VegComm)) +
  geom_boxplot(notch = TRUE, outlier.shape = NA) + 
  ylim(c(0.03,0.07))
# make plots of vi maps through time
ggplot() +
  geom_spatraster(data = eh.savi) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.1,0.45))

#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#
# RANDOM FOREST CLASSIFICATION 

# select which layers to use - just for workflow development purposes at this point
lyr <- 19:24

# create data frame for validation/training
savi <- plt.savi[,lyr]

# add veg association as a factor
savi$lc <- as.numeric(as.factor(veg.p$Associati0))

# remove na
savi <- na.omit(savi)

# IS ths necessary, or just to help keep track of things? 
#landClass <- data.frame(lcID = 1:3,
#                      landCover = c ("ground", "tall", "water"))

# specify training and validation data sets
set.seed(11213)

# randomly select half of the records
sampleSamp <- sample(seq(1,172),172/2)

savi$sampleType <- "train"

savi$sampleType[sampleSamp] <- "valid"


# create training and validation subsets
trainD <- savi[savi$sampleType=="train",]
validD<- savi[savi$sampleType=="valid",]


#------------Random Forest Classification of RGB data------------#
# run the Random Forest model
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:2) # number of variables available for splitting at each tree node

rf_model <- caret::train(x = trainD[,c(1:6)], #digital number data
                         y = as.factor(trainD$lc), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model

# evaluate validation data
confusionMatrix(predict(rf_model,validD[,1:6]),as.factor(validD$lc))

# apply RF model to rgb data
sv <- eh.savi[[lyr]]
rf_prediction <- predict(sv, rf_model, na.rm = T,
                         filename = "eh_rf_lc_test.tif",
                         overwrite = T, progress = T)

#plot the data
plot(rf_prediction)










# not ready to delete just yet #
######################################################################
wd(ts)
wy(ts)



e <- list.files(path = "eh_planet/evi/", full.names = T)
s <- list.files(path = "eh_planet/savi/", full.names = T)
e1 <- rast(e[1])
e2 <- rast(e[2])
e3 <- rast(e[3])

t <- rast(mos[1])
et <- evi(t)
st <- savi(t)

u <- rast(mos[2])
compa