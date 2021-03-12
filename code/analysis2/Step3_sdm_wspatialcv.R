library(blockCV)

library(raster)
library(rgdal)

library(ggplot2)

library(sdm)
library(usdm)

library(ROSE)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

#presabs=read.csv("presabs_Sn.csv")
presabs=read.csv("presabs_GrW.csv")
presabs=presabs[,-1]

presabs_sampl <- ovun.sample(occurrence ~ ., data = presabs, method = "both", p=0.5, seed = 1)$data
table(presabs_sampl$occurrence)

mydata_clean2=presabs_sampl

coordinates(mydata_clean2)=~x+y
proj4string(mydata_clean2)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Spatial blocking rub sb until do not have 0 in test

sb <- spatialBlock(speciesData = mydata_clean2,
                   species = "occurrence",
                   rasterLayer = all_predictor,
                   theRange = 25000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   numLimit=50)

#saveRDS(sb, file = "sb_Sn.rds")
#saveRDS(sb, file = "sb_GrW.rds")

folds <- sb$folds

# with sdm package
mydata_clean=presabs_sampl

accuracy=data.frame(matrix(ncol = 6, nrow = 50))
names(accuracy)<-c("modelID","AUC","Deviance","TSS","Kappa","RStype")

feaimp=data.frame(matrix(ncol = 4, nrow = 17))
names(feaimp)<-c("variables","AUCtest","lower","upper")


for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  
  data_forsdm_lidar <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(5:11,19)],test=mydata_clean[testSet, c(5:11,19)])
  model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)
  
  data_forsdm_sentinel <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(12:18,19)],test=mydata_clean[testSet, c(12:18,19)])
  model_sentinel <- sdm(occurrence~.,data=data_forsdm_sentinel,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)
  
  data_forsdm_landc <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(2:4,19)],test=mydata_clean[testSet, c(2:4,19)])
  model_landc <- sdm(occurrence~.,data=data_forsdm_landc,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)
  
  data_forsdm_lidsent <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(5:18,19)],test=mydata_clean[testSet, c(5:18,19)])
  model_lidsent <- sdm(occurrence~.,data=data_forsdm_lidsent,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)
  
  data_forsdm_lidall <- sdmData(formula=occurrence~., train=mydata_clean[trainSet, c(2:18,19)],test=mydata_clean[testSet,c(2:18,19)])
  model_lidall <- sdm(occurrence~.,data=data_forsdm_lidall,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)
  
  model_lidar_acc=getEvaluation(model_lidar,stat=c('AUC','TSS','Deviance','Kappa'))
  model_sentinel_acc=getEvaluation(model_sentinel,stat=c('AUC','TSS','Deviance','Kappa'))
  model_landc_acc=getEvaluation(model_landc,stat=c('AUC','TSS','Deviance','Kappa'))
  model_lidsent_acc=getEvaluation(model_lidsent,stat=c('AUC','TSS','Deviance','Kappa'))
  model_lidall_acc=getEvaluation(model_lidall,stat=c('AUC','TSS','Deviance','Kappa'))
  
  model_lidar_acc$RStype<-"lidar"
  model_sentinel_acc$RStype<-"sentinel"
  model_landc_acc$RStype<-"landc"
  model_lidsent_acc$RStype<-"lidsent"
  model_lidall_acc$RStype<-"lidall"
  
  newline=rbind(model_lidar_acc,model_sentinel_acc,model_landc_acc,model_lidsent_acc,model_lidall_acc)
  accuracy <- rbind(accuracy, newline)
  
  vi <- getVarImp(model_lidall, method=c('glm','maxent','rf'))
  newline2=vi@varImportanceMean[["AUCtest"]]
  feaimp=rbind(feaimp,newline2)
  
}

accuracy=accuracy[complete.cases(accuracy), ]
accuracy$modeltype<-"maxent"

accuracy$modeltype[accuracy$modelID <21] <- "glm"
accuracy$modeltype[accuracy$modelID >39] <- "rf"


feaimp=feaimp[complete.cases(feaimp), ]

write.csv(accuracy,"GrW_acc_test.csv")
write.csv(feaimp,"GrW_feaimp_test.csv")

#write.csv(accuracy,"Sn_acc_test.csv")
#write.csv(feaimp,"Sn_feaimp_test.csv")

# merge sdm objects

trainSet1 <- unlist(folds[[1]][1]) 
testSet1 <- unlist(folds[[1]][2])

data_forsdm_lidall1 <- sdmData(formula=occurrence~., train=mydata_clean[trainSet1, c(2:18,19)],test=mydata_clean[testSet1,c(2:18,19)])
model_lidall1 <- sdm(occurrence~.,data=data_forsdm_lidall1,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet2 <- unlist(folds[[2]][1]) 
testSet2 <- unlist(folds[[2]][2])

data_forsdm_lidall2 <- sdmData(formula=occurrence~., train=mydata_clean[trainSet2, c(2:18,19)],test=mydata_clean[testSet2,c(2:18,19)])
model_lidall2 <- sdm(occurrence~.,data=data_forsdm_lidall2,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet3 <- unlist(folds[[3]][1]) 
testSet3 <- unlist(folds[[3]][2])

data_forsdm_lidall3 <- sdmData(formula=occurrence~., train=mydata_clean[trainSet3, c(2:18,19)],test=mydata_clean[testSet3,c(2:18,19)])
model_lidall3 <- sdm(occurrence~.,data=data_forsdm_lidall3,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet4 <- unlist(folds[[4]][1]) 
testSet4 <- unlist(folds[[4]][2])

data_forsdm_lidall4 <- sdmData(formula=occurrence~., train=mydata_clean[trainSet4, c(2:18,19)],test=mydata_clean[testSet4,c(2:18,19)])
model_lidall4 <- sdm(occurrence~.,data=data_forsdm_lidall4,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

trainSet5 <- unlist(folds[[5]][1]) 
testSet5 <- unlist(folds[[5]][2])

data_forsdm_lidall5 <- sdmData(formula=occurrence~., train=mydata_clean[trainSet5, c(2:18,19)],test=mydata_clean[testSet5,c(2:18,19)])
model_lidall5 <- sdm(occurrence~.,data=data_forsdm_lidall5,methods=c('glm','maxent','rf'),replication=c('boot'),n=20)

m_merged_GrW=model_lidall1+model_lidall2+model_lidall3+model_lidall4+model_lidall5

write.sdm(m_merged_GrW,"m_merged_GrW",overwrite = TRUE)