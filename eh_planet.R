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

#veg.utm <- project(veg, ref)
# read in the stacks of evi and savi data
eh.evi <- rast(list.files(path = "eh_planet/evi/", full.names = T))
eh.savi <- rast(list.files(path = "eh_planet/savi/", full.names = T))

# extract evi and savi values for each plot
plt.evi <- terra::extract(eh.evi,veg)
plt.savi <- terra::extract(eh.savi,veg)

# set column headers based on file names
colnames(plt.evi) <- c("ID", substr(list.files(path = "eh_planet/evi/"),1,11))
colnames(plt.savi) <- c("ID", substr(list.files(path = "eh_planet/savi/"),1,11))

# add variables for analysis/plotting
plt.evi <- cbind(plt.evi, veg[,1:6])
plt.savi <- cbind(plt.savi, veg[,1:6])

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
                            format = "%Y%m%d", usetz = FALSE)


# group by veg class
vcl <- plt.vi %>%
  group_by(Association,timestamp) %>%
  summarise(evi = mean(evi, na.rm = T),
            svi = mean(savi, na.rm = T),
            elev_m = mean(elev_m, na.rm = T))

vcl$ts <- as.POSIXct(vcl$timestamp)

e <- ggplot() +
  geom_point(data = vcl, aes(x = ts, y = evi, color = Association, group = Association))

s <- ggplot() +
  geom_point(data = vcl, aes(x = ts, y = svi, color = Association, group = Association))

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