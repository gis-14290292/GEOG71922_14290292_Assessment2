
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


#prepare beetles community matrix
beetles_species <- beetles%>% select(sp1:sp22)

#head(beetles_species)


beetles_NMDS = metaMDS(beetles_species, k = 2, trymax = 100, distance = "bray")

stressplot(beetles_NMDS)
#plot(beetles_NMDS, display = "sites", type = "p")

#Environmental variables for envfit
env_500 <- beetles %>% select(pH,Moist,Litter, Bryophyte,CanopyHeight,Plants_m2,Elevation,Management,
                              urban_500, woodland_500)

env_3000 <- beetles %>% select(pH,Moist,Litter,Bryophyte,CanopyHeight,Plants_m2,Elevation,Management,
                               woodland_3000,urban_3000)

fit_500 = envfit(beetles_NMDS, env_500, perm= 9999)
fit_3000 = envfit(beetles_NMDS, env_3000, perm= 9999)

fit_500
fit_3000


beetles$Management_group <- cut(as.numeric(beetles$Management),breaks = 3,include.lowest = TRUE,
                                labels = c("Low management", "Medium management", "High management"))



# add sites, coloured by management group
# Set colours
colvec <- c(
  "Low management" = "darkgreen",
  "Medium management" = "orange",
  "High management" = "red"
)

plot(beetles_NMDS, display = "sites", type = "p")

plot( fit_500, add = TRUE, col = "black",cex=0.7)
points(beetles_NMDS, display = "sites", col = colvec[as.factor(beetles$Management_group)],cex=1.8,
       scaling = 3, pch = 21)
# add legend
legend("bottomright",legend = levels(as.factor(beetles$Management_group)),pt.bg = colvec,col = colvec,
       pch = 21,bty = "n")


beetles_hell <- decostand(beetles_species,method = "hellinger")



beetles_rda_500 = rda(beetles_hell ~ pH + Moist + Litter + Bryophyte + CanopyHeight +
                        Plants_m2 + Elevation + Management +woodland_500 + urban_500, data = beetles)

# Summarise the model
sumRda_500=summary(beetles_rda_500)

# Explanatory power of the first two RDA axes
sumRda_500$cont$importance[2, "RDA1"]
sumRda_500$cont$importance[2, "RDA2"]

#set up ordination space
plot(beetles_rda_500, type = "n", scaling = 3)

plot(fit_500, add = TRUE, col = "black",cex=0.7)

#add sites
points(beetles_rda_500, display = "sites", col = "black",cex=1.8,
       scaling = 3, pch = 21, bg = colvec[beetles$Management_group])


#add legend
with(beetles, legend("bottomright", legend = levels(as.factor(beetles$Management_group)), bty = "n",
                     col = colvec, pch = 21,cex=0.8, pt.bg = colvec))
#add species
points(beetles_rda_500, display = "species", pch = 3, cex = 2, col = "black")
#add species names
text(beetles_rda_500, display = "species", col = "blue", cex = 0.7,pos=1,offset=-1.5)




