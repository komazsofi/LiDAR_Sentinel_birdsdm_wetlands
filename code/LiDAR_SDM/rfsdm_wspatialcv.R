library(blockCV)

library(raster)
library(rgdal)

library(randomForest)
library(precrec)

library(ggplot2)

library(sdm)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/both/"
setwd(workingdirectory)

birdsfile="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs_GrW_rand_studyarea.shp"

# Import
birds = readOGR(dsn=birdsfile)
rasters=stack("lidar_sentinel.grd")

proj4string(birds) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
proj4string(rasters) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

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

# RF
folds <- sb$folds

testTable <- mydata_clean2
testTable$pred <- NA

for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  rf <- randomForest(x=mydata_clean[trainSet, c(1:8)], y=mydata_clean[trainSet, 17], 
                      ntree=25, importance =T)
  varImpPlot(rf)
  testTable$pred[testSet] <- predict(rf, mydata_clean[testSet, c(1:8,17)], type='response') # predict the test set
}

precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$occurrence)
autoplot(precrec_obj)

for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  rf <- randomForest(x=mydata_clean[trainSet, c(9:16)], y=mydata_clean[trainSet, 17], 
                     ntree=25, importance =T)
  varImpPlot(rf)
  testTable$pred[testSet] <- predict(rf, mydata_clean[testSet, c(9:16,17)], type='response') # predict the test set
}

precrec_obj2 <- evalmod(scores = testTable$pred, labels = testTable$occurrence)
autoplot(precrec_obj2)

for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  rf <- randomForest(x=mydata_clean[trainSet, c(1:16)], y=mydata_clean[trainSet, 17], 
                     ntree=25, importance =T)
  varImpPlot(rf)
  testTable$pred[testSet] <- predict(rf, mydata_clean[testSet, c(1:16,17)], type='response') # predict the test set
}

precrec_obj3 <- evalmod(scores = testTable$pred, labels = testTable$occurrence)
autoplot(precrec_obj3)

# with sdm package

data_forsdm_lidar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:8,17)],test=mydata_clean[testSet, c(1:8,17)])
data_forsdm_lidar

model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('boot'),n=25)
model_lidar

for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  data_forsdm_lidar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:8,17)],test=mydata_clean[testSet, c(1:8,17)])
  
  model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('boot'),n=25)
  print(model_lidar)
  
}
