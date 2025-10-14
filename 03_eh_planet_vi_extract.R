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

#set working directory

#set working directory depending on which computer being used
ifelse(Sys.info()['sysname'] == "Darwin",
       setwd("/Users/mloranty/Library/CloudStorage/GoogleDrive-mloranty@colgate.edu/My Drive/Documents/research/giraffe"),
       setwd("L:/projects/etosha_heights/"))
#       setwd("G:/My Drive/Documents/research/giraffe"))


#---------------------------------------------------------------------------------------------------------------------------------#
# read in the stacks of vi data along with sample plot polygons
#---------------------------------------------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------------------------------------------#
# extract all pixels within each plot for RF modeling
#---------------------------------------------------------------------------------------------------------------------------------#

# clean up veg data to append to pixel values
veg.r <- veg.p[,1:3]  # retain only necessary rows
names(veg.r) <- c("assoc", "class", "releve")
veg.r$ID <- 1:nrow(veg.p) # add an ID column

veg.r$hab <- case_when(        # create variable for habitat type
  veg.r$assoc %in% c(1:4) ~ 1, # Mountain
  veg.r$assoc %in% c(5:7) ~ 2, # Wetland 
  veg.r$assoc > 7 ~ 3)         # Savanna

#------------------------------------------------------------------------------------#
# extract vi values and append veg classes
#------------------------------------------------------------------------------------#
# note we can use exact = T to get fractional pixel coverage
# not sure this is necessary since releves are meant to be representative
p.evi <- terra::extract(eh.evi, veg.r, FUN = NULL) %>% #extract pixel values
  full_join(veg.r, copy = T) %>%
  na.omit()
write.csv(p.evi, "eh_vi_extracts/eh_plot_pix_evi.csv", row.names = F)

p.ndvi <- terra::extract(eh.ndvi, veg.r, FUN = NULL) %>% #extract pixel values
  full_join(veg.r, copy = T) %>%
  na.omit()
write.csv(p.ndvi, "eh_vi_extracts/eh_plot_pix_ndvi.csv", row.names = F)

p.nir <- terra::extract(eh.nirv, veg.r, FUN = NULL) %>% #extract pixel values
  full_join(veg.r, copy = T) %>%
  na.omit()
write.csv(p.nir, "eh_vi_extracts/eh_plot_pix_nirv.csv", row.names = F)

p.savi <- terra::extract(eh.savi, veg.r, FUN = NULL) %>% #extract pixel values
  full_join(veg.r, copy = T) %>%
  na.omit()
write.csv(p.savi, "eh_vi_extracts/eh_plot_pix_savi.csv", row.names = F)

rm(veg.r)

#---------------------------------------------------------------------------------------------------------------------------------#
# calculate plot-level means for data analysis
#---------------------------------------------------------------------------------------------------------------------------------#
veg.p <- veg.p[,1:6]
names(veg.p) <- c("assoc", "class", "releve", "date", "elev", "aspect")

pnd <- pivot_longer(p.ndvi, cols = starts_with("EH")) %>%
  group_by(releve, name) %>%
  summarise(ndvi = mean(value, na.rm = T),
            ndvi.sd = sd(value, na.rm = T)) 
  
 
pel <- pivot_longer(p.evi, cols = starts_with("EH")) %>%
  group_by(releve, name) %>%
  summarise(evi = mean(value, na.rm = T),
            evi.sd = sd(value, na.rm = T))  
  

psl <- pivot_longer(p.savi, cols = starts_with("EH")) %>%
  group_by(releve, name) %>%
  summarise(savi = mean(value, na.rm = T),
            savi.sd = sd(value, na.rm = T))  
  
  
pnl <- pivot_longer(p.nir, cols = starts_with("EH")) %>%
  group_by(releve, name) %>%
  summarise(nir = mean(value, na.rm = T),
            nir.sd = sd(value, na.rm = T))

plt.vi <- full_join(as.data.frame(veg.p),pnd)
plt.vi <- full_join(plt.vi2,pel)
plt.vi <- full_join(plt.vi2,psl)
plt.vi <- full_join(plt.vi2,pnl)

#---------------------------------------------------------------------------------------------------------------------------------#
# add field data on veg height
#---------------------------------------------------------------------------------------------------------------------------------#

# read coords for veg height data from 2024, omit unnecessary data and transform coordinates to match other layers
ht.crd <- read.csv("eh_veg_data/EH_canopy_height_coords_2024.csv", header = T)
ht.crd <- vect(ht.crd, geom = c("lon.dd", "lat.dd"), crs = "+proj=longlat +datum=WGS84")[,1]
ht.crd <- project(ht.crd, veg.p)

# match plots by proximity
crsrf <- as.data.frame(nearby(ht.crd, veg.p, k = 1))
names(crsrf) <- c("Plot", "VMrec")
crsrf$releve <- veg.p$releve[crsrf$VMrec]

# read height data, summarise by plot, and join with releve info
ht <- read.csv("eh_veg_data/EH_canopy_height_2024.csv", header = T) %>%
  group_by(Plot) %>%
  summarise(ht = mean(Ht, na.rm = T),
            ht.sd = sd(Ht, na.rm = T))

# join height and cross ref data
ht <- full_join(ht, crsrf)

# join with plot-level vi data
plt.vi <- left_join(plt.vi, ht)

# write to csv 
write.csv(plt.vi,"eh_vi_extracts/eh_plot_mean_vi.csv", row.names = F)
#---------------------------------------------------------------------------------------------------------------------------------#
# calculate mean/sd values for each vi layer
#---------------------------------------------------------------------------------------------------------------------------------#

# study area SD from across the images
eh.sd <- as.data.frame(names(eh.ndvi))
eh.sd[,2:9] <- 1.0
names(eh.sd) <- c("name", "ndvi","ndvi.sd", "evi","evi.sd", "savi","savi.sd", "nirv", "nirv.sd")


for (i in 1:nlyr(eh.nirv))
{
  eh.sd$evi[i] <- mean(values(eh.evi[[i]]), na.rm = T)
  eh.sd$savi[i] <- mean(values(eh.savi[[i]]), na.rm = T)
  eh.sd$nirv[i] <- mean(values(eh.nirv[[i]]), na.rm = T)
  eh.sd$ndvi[i] <- mean(values(eh.ndvi[[i]]), na.rm = T)
  eh.sd$evi.sd[i] <- sd(values(eh.evi[[i]]), na.rm = T)
  eh.sd$savi.sd[i] <- sd(values(eh.savi[[i]]), na.rm = T)
  eh.sd$nirv.sd[i] <- sd(values(eh.nirv[[i]]), na.rm = T)
  eh.sd$ndvi.sd[i] <- sd(values(eh.ndvi[[i]]), na.rm = T)
}

write.csv(eh.sd, "eh_vi_extracts/eh_site_vi.csv", row.names = F)
#---------------------------------------------------------------------------------------------------------------------------------#
# probably start new script here for data analysis
#---------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------#


# make plots of vi maps through time
map.s <- ggplot() +
  geom_spatraster(data = eh.savi[[15]]) +
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.35), name = "SAVI")

map.e <- ggplot() +
  geom_spatraster(data = eh.evi[[15]]) +
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.35), name = "EVI")

map.n <- ggplot() +
  geom_spatraster(data = eh.ndvi[[15]]) +
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.5), name = "NDVI")

map.v <- ggplot() +
  geom_spatraster(data = eh.nirv[[15]]) +
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.06), name = "NIRv")

map.n + map.e + map.s + map.v + plot_layout((ncol=1))
ggsave("sawma_figures/agu_vi_comp_map20240430b.png", width = 8, height = 12, units = "in")

map.n <- ggplot() +
  geom_spatraster(data = aggregate(eh.ndvi[[9]], fact = 100, fun = "mean")) +
  #xlim(15.25, 15.5) + 
  #facet_wrap(~lyr, ncol = 4) +
  scale_fill_viridis_c(limits = c(0.0,0.5), name = "NDVI")

da
ggsave("sawma_figures/agu_ndvi.png", width = 8, height = 8, units = "in")
# test land cover map
lc <- rast("sawma_figures/eh_rf_lc_test.tif")
ggplot() +
  geom_spatraster(data = lc) + 
  scale_fill_brewer(palette = "Set3")

plot(lc, col = rainbow(12))
#-------------------------------------------------------------#




#------------------------------------------------------------#
#--------- fit phenology curves to seasonal data-------------#
#------------------------------------------------------------#


vgr$date <- as.Date(vgr$timestamp)
vcl$date <- as.Date(vcl$timestamp)


mt <- filter(vgr, vg == "Mountain")


mt.phn <- zoo(mt$ndvi, mt$timestamp)
mt.ex <- greenExplore(mt.phn)
plotExplore(mt.ex)

