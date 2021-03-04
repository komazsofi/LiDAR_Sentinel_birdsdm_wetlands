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

abs_sampled=abs[sample(nrow(abs), 150), ]
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
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0,
                   numLimit=80)

folds <- sb$folds

# with sdm package
mydata_clean=presabs

accuracy=data.frame(matrix(ncol = 9, nrow = 0))
names(accuracy) <- c("AUC_lidar","AUC_radar","AUC_optical","AUC_sentinel","AUC_landcover",
                     "AUC_lidrad","AUC_lidopt","AUC_lidsent","AUC_lidall") 

feaimp=data.frame(matrix(ncol = 1, nrow = 18))


for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  data_forsdm_lidar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:7,19)],test=mydata_clean[testSet, c(1:7,19)])
  model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'))
  
  data_forsdm_radar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(11:14,19)],test=mydata_clean[testSet, c(11:14,19)])
  model_radar <- sdm(occurrence~.,data=data_forsdm_radar,methods=c('rf'))
  
  data_forsdm_optical <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(8:10,19)],test=mydata_clean[testSet, c(8:10,19)])
  model_optical <- sdm(occurrence~.,data=data_forsdm_optical,methods=c('rf'))
  
  data_forsdm_sentinel <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(8:14,19)],test=mydata_clean[testSet, c(8:14,19)])
  model_sentinel <- sdm(occurrence~.,data=data_forsdm_sentinel,methods=c('rf'))
  
  data_forsdm_landc <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(15:18,19)],test=mydata_clean[testSet, c(15:18,19)])
  model_landc <- sdm(occurrence~.,data=data_forsdm_landc,methods=c('rf'))
  
  data_forsdm_lidrad <- sdmData(formula=occurrence~., train=mydata_clean[trainSet,c(1:7,11:14,19)],test=mydata_clean[testSet, c(1:7,11:14,19)])
  model_lidrad <- sdm(occurrence~.,data=data_forsdm_lidrad,methods=c('rf'))
  
  data_forsdm_lidopt <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:7,8:10,19)],test=mydata_clean[testSet, c(1:7,8:10,19)])
  model_lidopt <- sdm(occurrence~.,data=data_forsdm_lidopt,methods=c('rf'))
  
  data_forsdm_lidsent <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:7,8:14,19)],test=mydata_clean[testSet, c(1:7,8:14,19)])
  model_lidsent <- sdm(occurrence~.,data=data_forsdm_lidsent,methods=c('rf'))
  
  data_forsdm_lidall <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(1:18,19)],test=mydata_clean[testSet,c(1:18,19)])
  model_lidall <- sdm(occurrence~.,data=data_forsdm_lidall,methods=c('rf'))
  
  newline <- data.frame(t(c(model_lidar@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_radar@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_optical@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_sentinel@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_landc@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_lidrad@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_lidopt@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_lidsent@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC,
                            model_lidall@models[["occurrence"]][["rf"]][["1"]]@evaluation[["test.indep"]]@statistics$AUC)))
  
  accuracy <- rbind(accuracy, newline)
  
  newline2 <- data.frame(model_lidall@models[["occurrence"]][["rf"]][["1"]]@varImportance[["training"]]@varImportance$AUCtest)
  feaimp=cbind(feaimp,newline2)
}

# feature importance visualization
feaimp$matrix.ncol...1..nrow...18.<-model_lidall@models[["occurrence"]][["rf"]][["1"]]@varImportance[["training"]]@varImportance$variables
forbarplot=data.frame(metrics=feaimp[,1], MeanAUCtest=rowMeans(feaimp[,-1]),lower=apply(feaimp[,-1], 1, FUN=min),upper=apply(feaimp[,-1], 1, FUN=max))

forbarplot$color<-0
forbarplot$color[1:8]<-1
forbarplot$color[9:11]<-2
forbarplot$color[12:15]<-3
forbarplot$color[16:19]<-4

ggplot(forbarplot, aes(x=metrics, y=MeanAUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 12)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4","4"="blue"),name="Metrics type",labels=c("LiDAR","Optical","RADAR","Landcover"))

# export
write.csv(accuracy,"accuracy_GrW_blockcv_25km_5_30perc.csv")
write.csv(forbarplot,"forbarplot_GrW_blockcv_25km_5_30perc.csv")

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

vi_lidall <- getVarImp(model_lidall,method=c('rf'))
plot(vi_lidall)