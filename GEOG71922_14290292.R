
# Assessment 2
setwd("D:/Project/SE/A2")
#install package
install.packages(c("terra", "gdm","vegan", "devtools","ggplot2","tidyr"))
library(devtools)
devtools::install_version("vegetarian",version = "1.2")

library(sf)
library(terra)
library(mapview)
library(dplyr)
library(vegan)

#load data
beetles_comm<- read.csv("data/scot_beetle_community.csv")
beetles_env<- read.csv("data/scot_beetle_env.csv")

LCM<-rast("data/LCMUK_2000.tif")

#inspect data
# view the first six rows of the data
head(beetles_comm) 
head(beetles_env)

#Check raster information
#LCM

#Plot land cover raster
plot(LCM)