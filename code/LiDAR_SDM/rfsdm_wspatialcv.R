library(blockCV)

library(raster)
library(rgdal)

library(ggplot2)

library(sdm)
library(usdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

bird=readOGR(dsn="presabs_GrW_rand_studyarea.shp")

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

rasters=stack(lidar_vif,optical_vif,radar_vif,landcover_vif)

# intersect
mydata <- raster::extract(rasters, bird, df = TRUE)
#mydata$occurrence <- as.factor(birds$occurrence)
mydata$occurrence <- bird$occurrence
mydata$x <- bird$x
mydata$y <- bird$y

mydata_clean=mydata[complete.cases(mydata), ]
mydata_clean <- mydata_clean[,-1]

# !!!!needs to be manually adjusted!!!!

pres=mydata_clean[mydata_clean$occurrence==1,]
abs=mydata_clean[mydata_clean$occurrence==0,]

pres_sampled=pres[sample(nrow(pres), 675), ]
presabs=rbind(pres_sampled,abs)

abs_sampled=abs[sample(nrow(abs), 250), ]
presabs=rbind(pres,abs_sampled)

# create a shp file

mydata_clean2=presabs

coordinates(mydata_clean2)=~x+y
proj4string(mydata_clean2)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

raster::shapefile(mydata_clean2,"mydata_clean2_Sn",overwrite=TRUE)

# Spatial blocking

sb <- spatialBlock(speciesData = mydata_clean2,
                   species = "occurrence",
                   rasterLayer = rasters,
                   theRange = 25000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   numLimit=80)

folds <- sb$folds

# with sdm package
mydata_clean=presabs

accuracy=data.frame(matrix(ncol = 9, nrow = 0))

feaimp=data.frame(matrix(ncol = 1, nrow = 19))


for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  data_forsdm_lidar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:8,20)],test=mydata_clean[testSet, c(1:8,20)])
  model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_radar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(12:15,20)],test=mydata_clean[testSet, c(12:15,20)])
  model_radar <- sdm(occurrence~.,data=data_forsdm_radar,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_optical <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(9:11,20)],test=mydata_clean[testSet, c(9:11,20)])
  model_optical <- sdm(occurrence~.,data=data_forsdm_optical,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_sentinel <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(9:15,20)],test=mydata_clean[testSet, c(9:15,20)])
  model_sentinel <- sdm(occurrence~.,data=data_forsdm_sentinel,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_landc <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(16:19,20)],test=mydata_clean[testSet, c(16:19,20)])
  model_landc <- sdm(occurrence~.,data=data_forsdm_landc,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_lidrad <- sdmData(formula=occurrence~., train=mydata_clean[trainSet,c(1:8,11:19,20)],test=mydata_clean[testSet, c(1:8,11:19,20)])
  model_lidrad <- sdm(occurrence~.,data=data_forsdm_lidrad,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_lidopt <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:8,9:11,20)],test=mydata_clean[testSet, c(1:8,9:11,20)])
  model_lidopt <- sdm(occurrence~.,data=data_forsdm_lidopt,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_lidsent <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:8,9:15,20)],test=mydata_clean[testSet, c(1:8,9:15,20)])
  model_lidsent <- sdm(occurrence~.,data=data_forsdm_lidsent,methods=c('rf'),replication=c('boot'),n=20)
  
  data_forsdm_lidall <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:19,20)],test=mydata_clean[testSet,c(1:19,20)])
  model_lidall <- sdm(occurrence~.,data=data_forsdm_lidall,methods=c('rf'),replication=c('boot'),n=20)
  
  model_lidar_acc=getEvaluation(model_lidar,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_radar_acc=getEvaluation(model_radar,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_optical_acc=getEvaluation(model_optical,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_sentinel_acc=getEvaluation(model_sentinel,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_landc_acc=getEvaluation(model_landc,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_lidrad_acc=getEvaluation(model_lidrad,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_lidopt_acc=getEvaluation(model_lidopt,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_lidsent_acc=getEvaluation(model_lidsent,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  model_lidall_acc=getEvaluation(model_lidall,stat=c('AUC','TSS','Deviance','Kappa'),opt=1)
  
  newline <- data.frame(t(c(mean(model_lidar_acc$AUC),mean(model_radar_acc$AUC),mean(model_optical_acc$AUC),
                            mean(model_sentinel_acc$AUC),mean(model_landc_acc$AUC),mean(model_lidrad_acc$AUC),
                            mean(model_lidopt_acc$AUC),mean(model_lidsent_acc$AUC),mean(model_lidall_acc$AUC))))
  
  accuracy <- rbind(accuracy, newline)
  
  newline2 <- data.frame(model_lidall@models[["occurrence"]][["rf"]][["1"]]@varImportance[["training"]]@varImportance$AUCtest)
  feaimp=cbind(feaimp,newline2)
}

names(accuracy) <- c("AUC_lidar","AUC_radar","AUC_optical","AUC_sentinel","AUC_landcover",
                     "AUC_lidrad","AUC_lidopt","AUC_lidsent","AUC_lidall") 

# export
write.csv(accuracy,"accuracy_GrW_blockcv_25km_5_30perc.csv")
write.csv(feaimp,"feaimp_GrW_blockcv_25km_5_30perc.csv")

# whout blockCV

data_forsdm_lidar <- sdmData(formula=occurrence~., train=presabs[, c(1:7,19)])
model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('boot'),n=25,test.percent=40)

data_forsdm_radar <- sdmData(formula=occurrence~., train=presabs[, c(11:14,19)])
model_radar <- sdm(occurrence~.,data=data_forsdm_radar,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)

data_forsdm_optical <- sdmData(formula=occurrence~., train=presabs[, c(8:10,19)])
model_optical <- sdm(occurrence~.,data=data_forsdm_optical,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)

data_forsdm_sentinel <- sdmData(formula=occurrence~., train=presabs[, c(8:14,19)])
model_sentinel <- sdm(occurrence~.,data=data_forsdm_sentinel,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)

data_forsdm_landc <- sdmData(formula=occurrence~., train=presabs[, c(15:18,19)])
model_landc <- sdm(occurrence~.,data=data_forsdm_landc,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)

data_forsdm_lidrad <- sdmData(formula=occurrence~., train=presabs[, c(1:7,11:14,19)])
model_lidrad <- sdm(occurrence~.,data=data_forsdm_lidrad,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)

data_forsdm_lidopt <- sdmData(formula=occurrence~., train=presabs[, c(1:7,8:10,19)])
model_lidopt <- sdm(occurrence~.,data=data_forsdm_lidopt,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)

data_forsdm_lidsent <- sdmData(formula=occurrence~., train=presabs[, c(1:7,8:14,19)])
model_lidsent <- sdm(occurrence~.,data=data_forsdm_lidsent,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)

data_forsdm_lidall <- sdmData(formula=occurrence~., train=presabs[, c(1:18,19)])
model_lidall <- sdm(occurrence~.,data=data_forsdm_lidall,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
