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

#join two cvs.file by sites number
names(beetles_comm) <- make.names(names(beetles_comm))
names(beetles_env) <- make.names(names(beetles_env))

# Check the shared site column
names(beetles_comm)
names(beetles_env)

# Join community data and environmental data by Sites
beetles_joined <- beetles_env %>%
  inner_join(beetles_comm, by = "Sites")

#head(beetles_joined)

#remove unseless columns
beetles <- beetles_joined %>%
  select(-X.1, -X.y) %>%
  rename(X = X.x)

head(beetles)
#names(beetles)