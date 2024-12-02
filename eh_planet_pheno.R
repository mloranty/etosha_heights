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
eh.ndvi <- rast(list.files(path = "eh_planet/ndvi/", pattern = glob2rx("*.tif"), full.names = T))

# set layer names 
names(eh.evi) <- c(substr(list.files(path = "eh_planet/evi/",pattern = glob2rx("*.tif")),1,11))
names(eh.savi) <- c(substr(list.files(path = "eh_planet/savi/",pattern = glob2rx("*.tif")),1,11))
names(eh.nirv) <- c(substr(list.files(path = "eh_planet/nirv/",pattern = glob2rx("*.tif")),1,11))
names(eh.ndvi) <- c(substr(list.files(path = "eh_planet/ndvi/",pattern = glob2rx("*.tif")),1,11))

# not sure why this didn't carry through from the processing step...
veg.p <- project(veg.p, eh.evi)

# extract mean vi values for each plot
plt.evi <- zonal(eh.evi,veg.p, fun = "mean", na.rm = T)
plt.savi <- zonal(eh.savi,veg.p, fun = "mean", na.rm = T)
plt.nirv <- zonal(eh.nirv,veg.p, fun = "mean", na.rm = T)
plt.ndvi <- zonal(eh.ndvi,veg.p, fun = "mean", na.rm = T)

# extract standard deviation of vi values for each plot
plt.evi.sd <- zonal(eh.evi,veg.p, fun = "sd", na.rm = T)
plt.savi.sd <- zonal(eh.savi,veg.p, fun = "sd", na.rm = T)
plt.nirv.sd <- zonal(eh.nirv,veg.p, fun = "sd", na.rm = T)
plt.ndvi.sd <- zonal(eh.ndvi,veg.p, fun = "sd", na.rm = T)


# add variables for analysis/plotting
plt.evi <- cbind(plt.evi, veg.p[,1:6])
plt.savi <- cbind(plt.savi, veg.p[,1:6])
plt.nirv <- cbind(plt.nirv, veg.p[,1:6])
plt.ndvi <- cbind(plt.ndvi, veg.p[,1:6])

# add variables for analysis/plotting
plt.evi.sd <- cbind(plt.evi.sd, veg.p[,1:6])
plt.savi.sd <- cbind(plt.savi.sd, veg.p[,1:6])
plt.nirv.sd <- cbind(plt.nirv.sd, veg.p[,1:6])
plt.ndvi.sd <- cbind(plt.ndvi.sd, veg.p[,1:6])

# pivot longer 
pel <- pivot_longer(plt.evi, cols = starts_with("EH"))
psl <- pivot_longer(plt.savi, cols = starts_with("EH"))
pnl <- pivot_longer(plt.nirv, cols = starts_with("EH"))
pnd <- pivot_longer(plt.ndvi, cols = starts_with("EH"))

# pivot longer 
pel.sd <- pivot_longer(plt.evi.sd, cols = starts_with("EH"))
psl.sd <- pivot_longer(plt.savi.sd, cols = starts_with("EH"))
pnl.sd <- pivot_longer(plt.nirv.sd, cols = starts_with("EH"))
pnd.sd <- pivot_longer(plt.ndvi.sd, cols = starts_with("EH"))

# change variable names
pel <- rename(pel, evi = value)
psl <- rename(psl, savi = value)
pnl <- rename(pnl, nirv = value)
pnd <- rename(pnd, ndvi = value)

# change variable names
pel.sd <- rename(pel.sd, evi.sd = value)
psl.sd <- rename(psl.sd, savi.sd = value)
pnl.sd <- rename(pnl.sd, nirv.sd = value)
pnd.sd <- rename(pnd.sd, ndvi.sd = value)

# join the two data frames
pel <- full_join(pel, pel.sd)
psl <- full_join(psl, psl.sd)
pnl <- full_join(pnl, pnl.sd)
pnd <- full_join(pnd, pnd.sd)

# write these files to csv
write.csv(pel, file = "eh_planet/eh_plot_evi.csv", row.names = F)
write.csv(psl, file = "eh_planet/eh_plot_savi.csv", row.names = F)
write.csv(pnl, file = "eh_planet/eh_plot_nirv.csv", row.names = F)
write.csv(pnd, file = "eh_planet/eh_plot_ndvi.csv", row.names = F)

# read them in for analyses on the go
pel <- na.omit(read.csv("eh_planet/eh_plot_evi.csv", header = T))
psl <- na.omit(read.csv("eh_planet/eh_plot_savi.csv", header = T))
pnl <- na.omit(read.csv("eh_planet/eh_plot_nirv.csv", header = T))
pnd <- na.omit(read.csv("eh_planet/eh_plot_ndvi.csv", header = T))

# read coords for veg height data from 2024, omit unnecessary data and transform coordinates to match other layers
ht.crd <- read.csv("eh_veg_data/EH_canopy_height_coords_2024.csv", header = T)
ht.crd <- vect(ht.crd, geom = c("lon.dd", "lat.dd"), crs = "+proj=longlat +datum=WGS84")[,1]
ht.crd <- project(ht.crd, veg.p)

# match plots by proximity
crsrf <- as.data.frame(nearby(ht.crd, veg.p, k = 1))
names(crsrf) <- c("Plot", "VMrec")
crsrf$releve <- veg.p$Releve._n0[crsrf$VMrec]

# read height data, summarise by plot, and join with releve info
ht <- read.csv("eh_veg_data/EH_canopy_height_2024.csv", header = T) %>%
  group_by(Plot) %>%
  summarise(ht = mean(Ht, na.rm = T),
            ht.sd = sd(Ht, na.rm = T))

# join height and cross ref data
ht <- full_join(ht, crsrf)


# join these dataframes
plt.vi <- full_join(pel, psl)
plt.vi <- full_join(plt.vi, pnl)
plt.vi <- full_join(plt.vi, pnd)
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
            ndvi = mean(ndvi, na.rm = T),
            elev_m = mean(elev_m, na.rm = T))

# can only plot with POSIXct class
vcl$ts <- as.POSIXct(vcl$timestamp)

# aggregate by veg group
vgr <- plt.vi %>%
  group_by(vg,timestamp) %>%
  summarise(evi = mean(evi, na.rm = T),
            svi = mean(savi, na.rm = T),
            nrv = mean(nirv, na.rm = T),
            ndvi = mean(ndvi, na.rm = T),
            elev_m = mean(elev_m, na.rm = T))

# can only plot with POSIXct class
vgr$ts <- as.POSIXct(vgr$timestamp)

# thinking about looking at changes in standard deviation through time
# for both sample plots, and across the entire study area (each raster)

# plot-level SD
tm.sd <- plt.vi %>%
  group_by(timestamp) %>%
  summarise(evi.sd = sd(evi, na.rm = T),
            savi.sd = sd(savi, na.rm = T),
            nirv.sd =sd(nirv, na.rm = T),
            ndvi.sd =sd(ndvi, na.rm = T))

tm.sd$timestamp <- as.POSIXct(tm.sd$timestamp)

# study area SD from across the images
eh.sd <- tm.sd
  
for (i in 1:nlyr(eh.nirv))
{
  eh.sd$evi.sd[i] <- sd(values(eh.evi[[i]]), na.rm = T)
  eh.sd$savi.sd[i] <- sd(values(eh.savi[[i]]), na.rm = T)
  eh.sd$nirv.sd[i] <- sd(values(eh.nirv[[i]]), na.rm = T)
  eh.sd$ndvi.sd[i] <- sd(values(eh.ndvi[[i]]), na.rm = T)
}

# make wide ndvi
dw <- pivot_wider(pnd,names_from = name, values_from = ndvi)
dw$

#------------------------------------------------------------#
#------------------- MET data--------------------------------#
#------------------------------------------------------------#  
  # read temp and precip data
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

#------------------------------------------------------------#
#------------------- PLOTS ----------------------------------#
#------------------------------------------------------------#

# theme for the plots
My_Theme = theme(
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10))

#-------------------met variables------------------------#    
# barplot of site precip
p <- ggplot(data = eh.prcp,
       aes(x = tmstmp, y = eh.precip)) +
        geom_bar(stat = "identity", position = "dodge") +
        xlim(c(min(vcl$ts), max(vcl$ts))) +
        labs(y = "Precipitation (mm)",
             x = NULL) +
        My_Theme

t <- ggplot(data = eh.ta, aes(x= tmstmp, y = airTemp)) +
        geom_line() + 
        xlim(c(min(vcl$ts), max(vcl$ts))) +
        labs(y = "Air Temperature", 
             x = NULL) + 
        My_Theme
#-------------------EVI------------------------#    
# plot evi timeseries
ea <- ggplot(data = vcl, aes(x = ts, y = evi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "EVI", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# plot evi time series by veg group
eg <- ggplot(data = vgr, aes(x = ts, y = evi, color = vg, group = vg)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "EVI", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# plot standard deviation of EVI
e.sd <- ggplot() + 
  geom_line(data = tm.sd, aes(x = timestamp, y = evi.sd)) + 
  geom_point(data = tm.sd, aes(x = timestamp, y = evi.sd)) +
  geom_line(data = eh.sd, aes(x = timestamp, y = evi.sd, color = "red")) +
  geom_point(data = eh.sd, aes(x = timestamp, y = evi.sd, color = "red")) + 
  labs(#color = "Vegetation \nCommunity", 
       y = "EVI Standard Deviation", 
       x = NULL) +
  theme(legend.position = "none") + 
  My_Theme

#-------------------SAVI------------------------#    
# plot savi time series by association
sa <- ggplot(data = vcl, aes(x = ts, y = svi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "SAVI", 
       x = NULL) +
  My_Theme +
 # theme(legend.position = "top") +
  scale_fill_discrete()

# plot savi time series by veg group
sg <- ggplot(data = vgr, aes(x = ts, y = svi, color = vg, group = vg)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "SAVI", 
       x = NULL) +
  My_Theme +
  # theme(legend.position = "top") +
  scale_fill_discrete()

# plot standard deviation of SAVi
s.sd <- ggplot() + 
  geom_line(data = tm.sd, aes(x = timestamp, y = savi.sd)) + 
  geom_point(data = tm.sd, aes(x = timestamp, y = savi.sd)) +
  geom_line(data = eh.sd, aes(x = timestamp, y = savi.sd, color = "red")) +
  geom_point(data = eh.sd, aes(x = timestamp, y = savi.sd, color = "red")) + 
  labs(#color = "Vegetation \nCommunity", 
    y = "SAVI Standard Deviation", 
    x = NULL) +
  theme(legend.position = "none") + 
  My_Theme 
  

  
#-------------------NIRv------------------------#    
na <- ggplot(data = vcl, aes(x = ts, y = nrv, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "NIRv", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

ng <- ggplot(data = vgr, aes(x = ts, y = nrv, color = vg, group = vg)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "NIRv", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# plot standard deviation of NIRv
n.sd <- ggplot() + 
  geom_line(data = tm.sd, aes(x = timestamp, y = nirv.sd)) + 
  geom_point(data = tm.sd, aes(x = timestamp, y = nirv.sd)) +
  geom_line(data = eh.sd, aes(x = timestamp, y = nirv.sd, color = "red")) +
  geom_point(data = eh.sd, aes(x = timestamp, y = nirv.sd, color = "red")) + 
  labs(#color = "Vegetation \nCommunity", 
    y = "NIRv Standard Deviation", 
    x = NULL) +
  theme(legend.position = "none") + 
  My_Theme

#-------------------NDVI------------------------#    
# upper and lower y-acis limits
yl <- 0.15
yu <- 0.37

da <- ggplot(data = vcl, aes(x = ts, y = ndvi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "NDVI", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# mountains only 
dmt <- vcl %>%
  filter(VegComm %in% c(1:4)) %>%
  ggplot( aes(x = ts, y = ndvi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  ylim(yl,yu) +  
  labs(color = "Vegetation \nCommunity", 
       y = "NDVI", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# wetlands only 
dw <- vcl %>%
  filter(VegComm %in% c(5:7)) %>%
  ggplot( aes(x = ts, y = ndvi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  ylim(yl,yu) +
  labs(color = "Vegetation \nCommunity", 
       y = "NDVI", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# savanna only 
ds <- vcl %>%
  filter(as.numeric(VegComm) > 7) %>%
  ggplot( aes(x = ts, y = ndvi, color = VegComm, group = VegComm)) +
  geom_point() + 
  geom_line() +
  ylim(yl,yu) +
  labs(color = "Vegetation \nCommunity", 
       y = "NDVI", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# broad veg groups
dg <- ggplot(data = vgr, aes(x = ts, y = ndvi, color = vg, group = vg)) +
  geom_point() + 
  geom_line() +
  labs(color = "Vegetation \nCommunity", 
       y = "NDVI", 
       x = NULL) +
  scale_fill_discrete() +
  My_Theme

# plot standard deviation of NDVI
d.sd <- ggplot() + 
  geom_line(data = tm.sd, aes(x = timestamp, y = ndvi.sd)) + 
  geom_point(data = tm.sd, aes(x = timestamp, y = ndvi.sd)) +
  geom_line(data = eh.sd, aes(x = timestamp, y = ndvi.sd, color = "red")) +
  geom_point(data = eh.sd, aes(x = timestamp, y = ndvi.sd, color = "red")) + 
  labs(#color = "Vegetation \nCommunity", 
    y = "NDVI Standard Deviation", 
    x = NULL) +
  theme(legend.position = "none") + 
  My_Theme
# two panel precip vi plot
sa + p +plot_layout((ncol=1))
ggsave("sawma_figures/savi_precip.png", width = 10, height = 8, units = "in")

s.sd + sa + p +plot_layout((ncol=1))
ggsave("sawma_figures/savi_sd_precip.png", width = 10, height = 8, units = "in")

ea + p  + plot_layout((ncol=1))
ggsave("sawma_figures/evi_precip.png", width = 10, height = 8, units = "in")

e.sd + ea + p  + plot_layout((ncol=1))
ggsave("sawma_figures/evi_sd_precip.png", width = 10, height = 8, units = "in")

na + p +plot_layout((ncol=1))
ggsave("sawma_figures/nirv_precip.png", width = 10, height = 8, units = "in")

n.sd + na + p +plot_layout((ncol=1))
ggsave("sawma_figures/nirv_sd_precip.png", width = 10, height = 8, units = "in")

da + p +plot_layout((ncol=1))
ggsave("sawma_figures/ndvi_precip.png", width = 10, height = 8, units = "in")

d.sd + da + p +plot_layout((ncol=1))
ggsave("sawma_figures/ndvi_sd_precip.png", width = 10, height = 8, units = "in")

dmt + dw + ds + plot_layout((ncol=1))
ggsave("sawma_figures/ndvi_veg.png", width = 10, height = 8, units = "in")

dg + eg + sg + ng + plot_layout((ncol=1))
ggsave("sawma_figures/vi_comp.png", width = 10, height = 8, units = "in")
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
map.s <- ggplot() +
  geom_spatraster(data = eh.savi[[9]]) +
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.35), name = "SAVI")

map.e <- ggplot() +
  geom_spatraster(data = eh.evi[[9]]) +
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.35), name = "EVI")

map.n <- ggplot() +
  geom_spatraster(data = eh.ndvi[[9]]) +
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.5), name = "NDVI")

map.n + map.e + map.s + plot_layout((ncol=1))
ggsave("sawma_figures/vi_comp_map.png", width = 10, height = 8, units = "in")

map.n <- ggplot() +
  geom_spatraster(data = aggregate(eh.ndvi[[9]], fact = 100, fun = "mean")) +
  #xlim(15.25, 15.5) + 
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.5), name = "NDVI")

# test land cover map
lc <- rast("sawma_figures/eh_rf_lc_test.tif")
ggplot() +
  geom_spatraster(data = lc) + 
  scale_fill_brewer(palette = "Set3")

plot(lc, col = rainbow(12))
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