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

beetles_xy <- data.frame(
  X = beetles$X,
  Y = beetles$Y
)

beetles_sp <- vect(
  beetles_xy,
  geom = c("X", "Y"),
  crs = "EPSG:27700"
)
plot(beetles_sp)

# Reproject LCM to EPSG:27700
LCM<- project(LCM,crs(beetles_sp), method = "near")

# Plot raster and overlay beetle points
plot(LCM)
points(beetles_sp, col = "red", pch = 20)


#access levels of the raster by treating them as categorical data
LCM<-as.factor(LCM)

#create 2 buffers
buffers_500 <- terra::buffer(beetles_sp, width = 500)
buffers_3000 <- terra::buffer(beetles_sp, width = 3000)

#class(buffers_500)
#class(LCM)
#crs(buffers_500)
#crs(LCM)

# choose land-cover classes
woodland_codes<- c(1, 2)
openland_codes<-c(3,4)
wetland_codes <-c(5,7)
urban_codes<-c(6)
water_codes<-c(8,9)

# function to reclassify selected land-cover classes to 1
make_binary <- function(LCM, codes){
  # default = 0 for all classes
  reclass <- rep(0, nrow(levels(LCM)[[1]]))
  # set selected classes to 1
  reclass[levels(LCM)[[1]]$ID %in% codes] <- 1
  # create reclassification matrix
  RCmatrix <- cbind(levels(LCM)[[1]]$ID, reclass)
  RCmatrix <- apply(RCmatrix, 2, as.numeric)
  # classify raster
  binary_lc <- classify(LCM, RCmatrix)
  return(binary_lc)
}


# create binary land-cover rasters
woodland<- make_binary(LCM, woodland_codes)
openland<- make_binary(LCM, openland_codes)
wetland<- make_binary(LCM, wetland_codes)
urban<- make_binary(LCM, urban_codes)
water<- make_binary(LCM, water_codes)

#plot
#plot(woodland)
#plot(urban)
#plot(beetles_sp, add = TRUE, col = "red", pch = 20)

#plot(urban)


#function to extract percentage land-cover inside buffers
extract_precent<-function(lc_raster, buffers, radius){
  # extract land-cover values within buffers
  sites.lc <- extract(
    lc_raster,
    buffers,
    fun = "sum",
    na.rm = TRUE
  )
  
  #calculate raster cell area
  cellArea <- prod(res(lc_raster))
  
  #calculate land-cover area inside each buffer
  lc.area <- sites.lc[, 2] * cellArea
  
  #calculate buffer area
  buffer.area <- pi * radius^2
  
  #calculate percentage cover
  lc.percent <- lc.area / buffer.area * 100
  return(lc.percent)
}

# extract land-cover percentage within 500 m buffers
beetles$woodland_500<-extract_precent(woodland, buffers_500, 500)
beetles$urban_500<-extract_precent(urban, buffers_500, 500)

# extract land-cover percentage within 3000 m buffers
beetles$woodland_3000<- extract_precent(woodland, buffers_3000, 3000)
beetles$urban_3000<- extract_precent(urban, buffers_3000, 3000)


#show results
head(beetles)

# check range of values
range(beetles$woodland_500, na.rm = TRUE)
range(beetles$urban_500, na.rm = TRUE)
range(beetles$woodland_3000, na.rm = TRUE)
range(beetles$urban_3000, na.rm = TRUE)

