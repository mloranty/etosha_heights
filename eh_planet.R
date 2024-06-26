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

#set working directory
#setwd("G:/My Drive/Documents/research/giraffe")
setwd("L:/projects/etosha_heights/")

# define evi function for 8-band planet images
# note the reflectance values are multiplied by 10000
evi <- function(x){
  b <- x[[2]]/10000
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  2.5*((n-r)/(n+6*r-7.5*b+1))
}

# define soil adjusted vi (savi) function for 8-band planet images
# L = 0.5 after Huete et al 1988
# note the reflectance values are multiplied by 10000
savi <- function(x){
  r <- x[[6]]/10000
  n <- x[[8]]/10000
  ((1.5*(n-r))/(n+r+0.5))
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
mos <- list.files(pattern = "composite.tif", recursive = T, full.names = T)

#extract the directory name to use for output filenaming and to get the date
 f <- sapply(strsplit(mos,"/"), FUN = "[", 3)

# extract timestamp from directory name 
ts <- strptime(as.numeric(substr(f,4,11)), 
               format = "%Y%m%d", tz = "")


# evi output filenames
eo <- paste("eh_planet/evi/",f,"_evi.tif", sep = "")

# evi output filenames
so <- paste("eh_planet/savi/",f,"_savi.tif", sep = "")

#reference raster to align everything to the same extent
ref <- rast(mos[1])

# calculate evi and savi and write to file
# note we ran into some memory issues here
for(i in 1:length(mos))
{
  x <- rast(mos[i])
  e <- evi(x)
  s <- savi(x)
  e <- resample(e,ref, filename = eo[i], overwrite = T)
  s <- resample(s,ref, filename = so[i], overwrite = T)
 # writeRaster(e,eo[i], overwrite = T)
 # writeRaster(s,so[i], overwrite = T)
  rm(x,s,e)
}


# read eh veg data for validation
vg <-read.csv("eh_veg_data/DB_EtoshaHeights_VegTransects.csv", 
              header = T)

# convert to spatial vector
veg <- vect(vg, geom = c("lon_dd", "lat_dd"), crs = "epsg:4326")

# buffer to 10m diameter based on Marufu thesis
veg.p <- buffer(veg, 5)
veg.p <- project(veg.p, ref)
writeVector(veg.p, filename = "eh_veg_data/DB_EtoshaHeights_VegTransects_5m_buffer.shp")

#-------------------------------------------------------------------------------------------

# read in the stacks of evi and savi data along with sample plot polygons
veg.p <- vect("eh_veg_data/DB_EtoshaHeights_VegTransects_5m_buffer.shp")
eh.evi <- rast(list.files(path = "eh_planet/evi/", pattern = glob2rx("*.tif"), full.names = T))
eh.savi <- rast(list.files(path = "eh_planet/savi/", pattern = glob2rx("*.tif"), full.names = T))

# not sure why this didn't carry through from the processing step...
veg.p <- project(veg.p, eh.evi)

# extract evi and savi values for each plot
# plt.evi <- terra::extract(eh.evi[[1]],veg.p, weights = T)
# plt.savi <- terra::extract(eh.savi,veg.p)
plt.evi <- zonal(eh.evi,veg.p, fun = "mean", na.rm = T)
plt.savi <- zonal(eh.savi,veg.p, fun = "mean", na.rm = T)

# set column headers based on file names
#colnames(plt.evi) <- c("ID", substr(list.files(path = "eh_planet/evi/"),1,11)) # differs based on zonal vs. extract
colnames(plt.evi) <- c(substr(list.files(path = "eh_planet/evi/",pattern = glob2rx("*.tif")),1,11))
colnames(plt.savi) <- c(substr(list.files(path = "eh_planet/savi/",pattern = glob2rx("*.tif")),1,11))

names(eh.evi) <- c(substr(list.files(path = "eh_planet/evi/",pattern = glob2rx("*.tif")),1,11))
names(eh.savi) <- c(substr(list.files(path = "eh_planet/savi/",pattern = glob2rx("*.tif")),1,11))

# add variables for analysis/plotting
plt.evi <- cbind(plt.evi, veg.p[,1:6])
plt.savi <- cbind(plt.savi, veg.p[,1:6])

# pivot longer 
pel <- pivot_longer(plt.evi, cols = starts_with("EH"))
psl <- pivot_longer(plt.savi, cols = starts_with("EH"))

# change varibale names
pel <- rename(pel, evi = value)
psl <- rename(psl, savi = value)

# join these dataframes
plt.vi <- full_join(pel, psl)
rm(pel,psl,plt.evi,plt.savi)

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

# make plots of vi maps through time
ggplot() +
  geom_spatraster(data = eh.savi) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.1,0.45))
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