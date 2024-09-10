######################################
#
# analyze Etosha Heights micromet data
#
# MML 09/09/2024
######################################

rm(list = ls())

#set working directory
setwd("G:/My Drive/Documents/research/giraffe")
#setwd("L:/projects/etosha_heights/")

# list data files
df <- list.files(path = "SLEEC_met_data/", pattern = ".csv", full.names = T)


# test some code
t <- read.csv(df[1], skip = 2, header = T)
