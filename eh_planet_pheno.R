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

# change variable names
pel <- rename(pel, evi = value)
psl <- rename(psl, savi = value)
pnl <- rename(pnl, nirv = value)

# write these files to csv
write.csv(pel, file = "eh_planet/eh_plot_evi.csv", row.names = F)
write.csv(psl, file = "eh_planet/eh_plot_savi.csv", row.names = F)
write.csv(pnl, file = "eh_planet/eh_plot_nirv.csv", row.names = F)


# read them in for analyses on the go
pel <- na.omit(read.csv("eh_planet/eh_plot_evi.csv", header = T))
psl <- na.omit(read.csv("eh_planet/eh_plot_savi.csv", header = T))
pnl <- na.omit(read.csv("eh_planet/eh_plot_nirv.csv", header = T))

prcp <- read.csv("eh_met_data/daily_precip.csv", header = T)

eh.prcp <- prcp %>%
  group_by(tmstmp) %>%
  summarise(eh.precip = mean(precip.mm))

eh.prcp$tmstmp <- as.POSIXct(strptime(eh.prcp$tmstmp,
                                      format = "%Y-%m-%d"))

ta <- read.csv("eh_met_data/daily_air_temp.csv", header = T)

eh.ta <- ta %>%
  group_by(tmstmp) %>%
  summarise(airTemp = mean(airTemp, na.rm = T))

eh.ta$tmstmp <- as.POSIXct(strptime(eh.ta$tmstmp,
                                      format = "%Y-%m-%d"))
# something wonky here with a many to many relationship, this shouldn't be the case - it should be one to one
# join these dataframes
plt.vi <- full_join(pel, psl)
plt.vi <- full_join(plt.vi, pnl)
#rm(pel,psl,plt.evi,plt.savi)

# add time stamp here
plt.vi$timestamp <- strptime(as.numeric(substr(plt.vi$name,4,11)), 
                             format = "%Y%m%d")

# create a factor for veg community
plt.vi$VegComm <- as.factor(plt.vi$Associati0)

# create a factor for the larger groups
plt.vi$vg <- case_when(
  plt.vi$Associati0 %in% c(1:4) ~ "Mountain", 
  plt.vi$Associati0 %in% c(5:7) ~ "Wetland", 
  plt.vi$Associati0 > 7 ~ "Savanna Shrubland"
)

# aggregate by veg class
vcl <- plt.vi %>%
  group_by(VegComm,timestamp) %>%
  summarise(evi = mean(evi, na.rm = T),
            svi = mean(savi, na.rm = T),
            nrv = mean(nirv, na.rm = T),
            elev_m = mean(elev_m, na.rm = T))

# can only plot with POSIXct class
vcl$ts <- as.POSIXct(vcl$timestamp)

# aggregate by veg group
vgr <- plt.vi %>%
  group_by(vg,timestamp) %>%
  summarise(evi = mean(evi, na.rm = T),
            svi = mean(savi, na.rm = T),
            nrv = mean(nirv, na.rm = T),
            elev_m = mean(elev_m, na.rm = T))

# can only plot with POSIXct class
vgr$ts <- as.POSIXct(vgr$timestamp)

#------------------------------------------------------------#
My_Theme = theme(
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12))

# barplot of site precip
p <- ggplot(data = eh.prcp,
       aes(x = tmstmp, y = eh.precip)) +
        geom_bar(stat = "identity", position = "dodge") +
        xlim(c(min(vcl$ts), max(vcl$ts))) +
        labs(y = "Precipitation (mm)",
             x = NULL) +
      My_Theme

# plot evi timeseries
e <- ggplot(data = vcl, aes(x = ts, y = evi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "EVI", 
       x = NULL,
       title = "2023-2024") +
  scale_fill_discrete() +
  My_Theme

e <- ggplot(data = vgr, aes(x = ts, y = evi, color = vg, group = vg)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "EVI", 
       x = NULL,
       title = "2023-2024") +
  scale_fill_discrete() +
  My_Theme

# plot savi time series by association
s <- ggplot(data = vcl, aes(x = ts, y = svi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "SAVI", 
       x = NULL,
       title = "2023-2024") +
  My_Theme +
 # theme(legend.position = "top") +
  scale_fill_discrete()

# plot savi time series by veg group
s <- ggplot(data = vgr, aes(x = ts, y = svi, color = vg, group = vg)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "SAVI", 
       x = NULL,
       title = "2023-2024") +
  My_Theme +
  # theme(legend.position = "top") +
  scale_fill_discrete()

n <- ggplot(data = vcl, aes(x = ts, y = nrv, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "NIRv", 
       x = NULL,
       title = "2023-2024") +
  scale_fill_discrete() +
  My_Theme

n <- ggplot(data = vgr, aes(x = ts, y = nrv, color = vg, group = vg)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "NIRv", 
       x = NULL,
       title = "2023-2024") +
  scale_fill_discrete() +
  My_Theme

# two panel precip vi plot
s + p +plot_layout((ncol=1))
ggsave("sawma_figures/savi_precip.png", width = 10, height = 8, units = "in")

e + p  + plot_layout((ncol=1))
ggsave("sawma_figures/evi_precip.png", width = 10, height = 8, units = "in")

n + p +plot_layout((ncol=1))
ggsave("sawma_figures/nirv_precip.png", width = 10, height = 8, units = "in")


#---------------------------------------------#
# trying to make a plot with precip and savi 
ggplot() +
  geom_line(data = vcl, aes(x = ts, y = svi, color = VegComm, group = VegComm)) +
  geom_bar(data = eh.prcp, aes(x = tmstmp, y = (eh.precip*0.001)+0.125), stat = "identity") +
 # xlim(c(min(vcl$ts), max(vcl$ts))) + 
  ylim(c(0.125, 0.25))

ggplot() +
  geom_bar(data = eh.prcp, aes(x = tmstmp, y = eh.precip), stat = "identity") +
  geom_line(data = vcl, aes(x = ts, y = svi*200, color = VegComm, group = VegComm)) +
  xlim(c(min(vcl$ts), max(vcl$ts))) + 
  ylim(c(0.125, 0.25))
#---------------------------------------------#

#---------------------------------------------#
# plot with temp and precip
ggplot() +
  geom_bar(data = eh.prcp, aes(x = tmstmp, y = eh.precip), stat = "identity") +
  geom_line(data = eh.ta, aes(x = tmstmp, y = airTemp*1.67)) +
  xlim(c(min(vcl$ts), max(vcl$ts)))

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