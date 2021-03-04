library(blockCV)

library(raster)
library(rgdal)

library(ggplot2)

library(sdm)
library(usdm)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

bird=readOGR(dsn="presabs_Ba_rand_studyarea.shp")

proj4string(bird) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
proj4string(all_predictor) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

landcover=subset(all_predictor, c(1,2,3,4), drop=FALSE)
lidar=subset(all_predictor, c(6,7,8,9,10,11,12,13,14,15,16), drop=FALSE)
optical=subset(all_predictor, c(17,18,19,20,22,23,24,25,26), drop=FALSE)
radar=subset(all_predictor, c(29,30,31,32,34,35,36,37,38,40), drop=FALSE)

# VIF

vif_lidar=vifstep(lidar,th=3)
vif_optical=vifstep(optical,th=3)
vif_radar=vifstep(radar,th=3)
vif_landcover=vifstep(landcover,th=3)

lidar_vif=exclude(lidar,vif_lidar)
optical_vif=exclude(optical,vif_optical)
radar_vif=exclude(radar,vif_radar)
landcover_vif=exclude(landcover,vif_landcover)

# intersect
mydata <- raster::extract(rasters, birds, df = TRUE)
#mydata$occurrence <- as.factor(birds$occurrence)
mydata$occurrence <- birds$occurrence

mydata_clean=mydata[complete.cases(mydata), ]
mydata_clean <- mydata_clean[,-1]

# create a shp file
mydata$x <- birds$x
mydata$y <- birds$y

mydata_clean2=mydata[complete.cases(mydata), ]
mydata_clean2 <- mydata_clean2[,-1]

mydata_clean3=mydata_clean2

coordinates(mydata_clean2)=~x+y
proj4string(mydata_clean2)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Spatial blocking

sb <- spatialBlock(speciesData = mydata_clean2,
                   species = "occurrence",
                   rasterLayer = rasters,
                   theRange = 85000, # size of the blocks
                   k = 2,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)

folds <- sb$folds

# with sdm package

data_forsdm_lidar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:8,17)],test=mydata_clean[testSet, c(1:8,17)])
data_forsdm_lidar

model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('boot'),n=25)
model_lidar

for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  data_forsdm_lidar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:8,17)],test=mydata_clean[testSet, c(1:8,17)])
  
  model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'))
  print(model_lidar)
  
}
