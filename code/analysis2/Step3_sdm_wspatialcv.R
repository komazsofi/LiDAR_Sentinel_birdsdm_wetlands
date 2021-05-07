library(blockCV)

library(raster)
library(rgdal)

library(ggplot2)

library(sdm)
library(usdm)

library(ROSE)

library(spatialEco)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

presabs=read.csv("presabs_Sn_apr2.csv")
#presabs=read.csv("presabs_GrW_apr2.csv")
presabs=presabs[,-1]

mydata_clean2=presabs

coordinates(mydata_clean2)=~x+y
proj4string(mydata_clean2)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# spatial thinning

sub.presabs <- subsample.distance(mydata_clean2, size = 2696, d = 20, replacement=FALSE,latlong=FALSE)
#sub.presabs <- subsample.distance(mydata_clean2, size = 1420, d = 20, replacement=FALSE,latlong=FALSE)

table(sub.presabs$occurrence)
# optimal blocksize

sac <- spatialAutoRange(rasterLayer = all_predictor,
                        sampleNumber = 5000,
                        doParallel = TRUE,
                        showPlots = TRUE)

# Spatial blocking rub sb until do not have 0 in test

sb <- spatialBlock(speciesData = sub.presabs,
                   species = "occurrence",
                   rasterLayer = all_predictor,
                   theRange = 8000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   numLimit=0)

folds <- sb$folds

# running sdm per every train-test block
mydata_clean= as(sub.presabs, "data.frame")

trainSet1 <- unlist(folds[[1]][1]) 
testSet1 <- unlist(folds[[1]][2])

train1=mydata_clean[trainSet1,]
train1_sampl <- ovun.sample(occurrence ~ ., data = train1, method = "under", p=0.5, seed = 1)$data
test1=mydata_clean[testSet1, ]
test1_sampl <- ovun.sample(occurrence ~ ., data = test1, method = "under", p=0.5, seed = 1)$data

data_forsdm_lidar1 <- sdmData(formula=occurrence~., train=train1_sampl[, c(7:15,23)],test=test1_sampl[, c(7:15,23)])
model_lidar1 <- sdm(occurrence~.,data=data_forsdm_lidar1,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_sentinel1 <- sdmData(formula=occurrence~., train=train1_sampl[, c(16:22,23)],test=test1_sampl[, c(16:22,23)])
model_sentinel1 <- sdm(occurrence~.,data=data_forsdm_sentinel1,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_landc1 <- sdmData(formula=occurrence~., train=train1_sampl[, c(1:6,23)],test=test1_sampl[, c(1:6,23)])
model_landc1 <- sdm(occurrence~.,data=data_forsdm_landc1,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidsent1 <- sdmData(formula=occurrence~., train=train1_sampl[, c(7:22,23)],test=test1_sampl[, c(7:22,23)])
model_lidsent1 <- sdm(occurrence~.,data=data_forsdm_lidsent1,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidall1 <- sdmData(formula=occurrence~., train=train1_sampl[, c(1:22,23)],test=test1_sampl[, c(1:22,23)])
model_lidall1 <- sdm(occurrence~.,data=data_forsdm_lidall1,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet2 <- unlist(folds[[2]][1]) 
testSet2 <- unlist(folds[[2]][2])

train2=mydata_clean[trainSet2, ]
train2_sampl <- ovun.sample(occurrence ~ ., data = train2, method = "under", p=0.5, seed = 1)$data
test2=mydata_clean[testSet2, ]
test2_sampl <- ovun.sample(occurrence ~ ., data = test2, method = "under", p=0.5, seed = 1)$data

data_forsdm_lidar2 <- sdmData(formula=occurrence~., train=train2_sampl[, c(7:15,23)],test=test2_sampl[, c(7:15,23)])
model_lidar2 <- sdm(occurrence~.,data=data_forsdm_lidar2,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_sentinel2 <- sdmData(formula=occurrence~., train=train2_sampl[, c(16:22,23)],test=test2_sampl[, c(16:22,23)])
model_sentinel2 <- sdm(occurrence~.,data=data_forsdm_sentinel2,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_landc2 <- sdmData(formula=occurrence~., train=train2_sampl[, c(1:6,23)],test=test2_sampl[, c(1:6,23)])
model_landc2 <- sdm(occurrence~.,data=data_forsdm_landc2,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidsent2 <- sdmData(formula=occurrence~., train=train2_sampl[, c(7:22,23)],test=test2_sampl[, c(7:22,23)])
model_lidsent2 <- sdm(occurrence~.,data=data_forsdm_lidsent2,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidall2 <- sdmData(formula=occurrence~., train=train2_sampl[, c(1:22,23)],test=test2_sampl[, c(1:22,23)])
model_lidall2 <- sdm(occurrence~.,data=data_forsdm_lidall2,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet3 <- unlist(folds[[3]][1]) 
testSet3 <- unlist(folds[[3]][2])

train3=mydata_clean[trainSet3, ]
train3_sampl <- ovun.sample(occurrence ~ ., data = train3, method = "under", p=0.5, seed = 1)$data
test3=mydata_clean[testSet3, ]
test3_sampl <- ovun.sample(occurrence ~ ., data = test3, method = "under", p=0.5, seed = 1)$data

data_forsdm_lidar3 <- sdmData(formula=occurrence~., train=train3_sampl[, c(7:15,23)],test=test3_sampl[, c(7:15,23)])
model_lidar3 <- sdm(occurrence~.,data=data_forsdm_lidar3,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_sentinel3 <- sdmData(formula=occurrence~., train=train3_sampl[, c(16:22,23)],test=test3_sampl[, c(16:22,23)])
model_sentinel3 <- sdm(occurrence~.,data=data_forsdm_sentinel3,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_landc3 <- sdmData(formula=occurrence~., train=train3_sampl[, c(1:6,23)],test=test3_sampl[, c(1:6,23)])
model_landc3 <- sdm(occurrence~.,data=data_forsdm_landc3,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidsent3 <- sdmData(formula=occurrence~., train=train3_sampl[, c(7:22,23)],test=test3_sampl[, c(7:22,23)])
model_lidsent3 <- sdm(occurrence~.,data=data_forsdm_lidsent3,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidall3 <- sdmData(formula=occurrence~., train=train3_sampl[, c(1:22,23)],test=test3_sampl[, c(1:22,23)])
model_lidall3 <- sdm(occurrence~.,data=data_forsdm_lidall3,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet4 <- unlist(folds[[4]][1]) 
testSet4 <- unlist(folds[[4]][2])

train4=mydata_clean[trainSet4, ]
train4_sampl <- ovun.sample(occurrence ~ ., data = train4, method = "under", p=0.5, seed = 1)$data
test4=mydata_clean[testSet4, ]
test4_sampl <- ovun.sample(occurrence ~ ., data = test4, method = "under", p=0.5, seed = 1)$data

data_forsdm_lidar4 <- sdmData(formula=occurrence~., train=train4_sampl[, c(7:15,23)],test=test4_sampl[, c(7:15,23)])
model_lidar4 <- sdm(occurrence~.,data=data_forsdm_lidar4,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_sentinel4 <- sdmData(formula=occurrence~., train=train4_sampl[, c(16:22,23)],test=test4_sampl[, c(16:22,23)])
model_sentinel4 <- sdm(occurrence~.,data=data_forsdm_sentinel4,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_landc4 <- sdmData(formula=occurrence~., train=train4_sampl[, c(1:6,23)],test=test4_sampl[, c(1:6,23)])
model_landc4 <- sdm(occurrence~.,data=data_forsdm_landc4,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidsent4 <- sdmData(formula=occurrence~., train=train4_sampl[, c(7:22,23)],test=test4_sampl[, c(7:22,23)])
model_lidsent4 <- sdm(occurrence~.,data=data_forsdm_lidsent4,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidall4 <- sdmData(formula=occurrence~., train=train4_sampl[, c(1:22,23)],test=test4_sampl[, c(1:22,23)])
model_lidall4 <- sdm(occurrence~.,data=data_forsdm_lidall4,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet5 <- unlist(folds[[5]][1]) 
testSet5 <- unlist(folds[[5]][2])

train5=mydata_clean[trainSet5, ]
train5_sampl <- ovun.sample(occurrence ~ ., data = train5, method = "under", p=0.5, seed = 1)$data
test5=mydata_clean[testSet5, ]
test5_sampl <- ovun.sample(occurrence ~ ., data = test5, method = "under", p=0.5, seed = 1)$data

data_forsdm_lidar5 <- sdmData(formula=occurrence~., train=train5_sampl[, c(7:15,23)],test=test5_sampl[, c(7:15,23)])
model_lidar5 <- sdm(occurrence~.,data=data_forsdm_lidar5,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_sentinel5 <- sdmData(formula=occurrence~., train=train5_sampl[, c(16:22,23)],test=test5_sampl[, c(16:22,23)])
model_sentinel5 <- sdm(occurrence~.,data=data_forsdm_sentinel5,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_landc5 <- sdmData(formula=occurrence~., train=train5_sampl[, c(1:6,23)],test=test5_sampl[, c(1:6,23)])
model_landc5 <- sdm(occurrence~.,data=data_forsdm_landc5,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidsent5 <- sdmData(formula=occurrence~., train=train5_sampl[, c(7:22,23)],test=test5_sampl[, c(7:22,23)])
model_lidsent5 <- sdm(occurrence~.,data=data_forsdm_lidsent5,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

data_forsdm_lidall5 <- sdmData(formula=occurrence~., train=train5_sampl[, c(1:22,23)],test=test5_sampl[, c(1:22,23)])
model_lidall5 <- sdm(occurrence~.,data=data_forsdm_lidall5,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

m_merged_lidar=model_lidar1+model_lidar2+model_lidar3+model_lidar4+model_lidar5
m_merged_sentinel=model_sentinel1+model_sentinel2+model_sentinel3+model_sentinel4+model_sentinel5
m_merged_landc=model_landc1+model_landc2+model_landc3+model_landc4+model_landc5
m_merged_lidsent=model_lidsent1+model_lidsent2+model_lidsent3+model_lidsent4+model_lidsent5
m_merged_all=model_lidall1+model_lidall2+model_lidall3+model_lidall4+model_lidall5

# Export

setwd("D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April_results/")

write.sdm(m_merged_lidar,"merged_Sn_lidar2",overwrite = TRUE)
write.sdm(m_merged_sentinel,"merged_Sn_sentinel2",overwrite = TRUE)
write.sdm(m_merged_landc,"merged_Sn_landc2",overwrite = TRUE)
write.sdm(m_merged_lidsent,"merged_Sn_lidsent2",overwrite = TRUE)
write.sdm(m_merged_all,"merged_Sn_all2",overwrite = TRUE)

#write.sdm(m_merged_lidar,"merged_GrW_lidar2",overwrite = TRUE)
#write.sdm(m_merged_sentinel,"merged_GrW_sentinel2",overwrite = TRUE)
#write.sdm(m_merged_landc,"merged_GrW_landc2",overwrite = TRUE)
#write.sdm(m_merged_lidsent,"merged_Grw_lidsent2",overwrite = TRUE)
#write.sdm(m_merged_all,"merged_GrW_all2",overwrite = TRUE)