library(dismo)
library(raster)
library(mlr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(mmpf)
library(iml)
library(patchwork)
library(sdm)

#workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

presabs=read.csv("presabs_Sn.csv")
presabs$occurrence<-factor(presabs$occurrence)
presabs=presabs[,-1]

# start mlr

set.seed(11)

tasklid = makeClassifTask(data = presabs[,c(5:12,20)], target = "occurrence",
                       positive = "1", coordinates = presabs[,c(21,22)])
tasksent = makeClassifTask(data = presabs[,c(13:19,20)], target = "occurrence",
                       positive = "1", coordinates = presabs[,c(21,22)])
tasklandc = makeClassifTask(data = presabs[,c(1:4,20)], target = "occurrence",
                       positive = "1", coordinates = presabs[,c(21,22)])
tasklidsent = makeClassifTask(data = presabs[,c(5:19,20)], target = "occurrence",
                       positive = "1", coordinates = presabs[,c(21,22)])
taskall = makeClassifTask(data = presabs[,c(1:19,20)], target = "occurrence",
                       positive = "1", coordinates = presabs[,c(21,22)])

lrnRF = makeLearner("classif.randomForest",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

perf_level_spCV = makeResampleDesc(method = "SpRepCV", folds = 4, reps = 25)
#perf_level_spCV = makeResampleDesc(method = "RepCV", folds = 4, reps = 25)

# accuracy measure

cvRF_lid = resample(learner = lrnRF, task =tasklid,
                resampling = perf_level_spCV, 
                measures = list(auc,tpr,tnr))

cvRF_sent = resample(learner = lrnRF, task =tasksent,
                resampling = perf_level_spCV, 
                measures = list(auc,tpr,tnr))

cvRF_landc = resample(learner = lrnRF, task =tasklandc,
                resampling = perf_level_spCV, 
                measures = list(auc,tpr,tnr))

cvRF_lidsent = resample(learner = lrnRF, task =tasklidsent,
                resampling = perf_level_spCV, 
                measures = list(auc,tpr,tnr))

cvRF_all = resample(learner = lrnRF, task =taskall,
                resampling = perf_level_spCV, 
                measures = list(auc,tpr,tnr))

acc_sprepcv_lid=cvRF_lid[["measures.test"]]
acc_sprepcv_lid$tss=acc_sprepcv_lid$tpr+acc_sprepcv_lid$tnr-1
acc_sprepcv_lid=acc_sprepcv_lid[complete.cases(acc_sprepcv_lid), ]

acc_sprepcv_sent=cvRF_sent[["measures.test"]]
acc_sprepcv_sent$tss=acc_sprepcv_sent$tpr+acc_sprepcv_sent$tnr-1
acc_sprepcv_sent=acc_sprepcv_sent[complete.cases(acc_sprepcv_sent), ]

acc_sprepcv_landc=cvRF_landc[["measures.test"]]
acc_sprepcv_landc$tss=acc_sprepcv_landc$tpr+acc_sprepcv_landc$tnr-1
acc_sprepcv_landc=acc_sprepcv_landc[complete.cases(acc_sprepcv_landc), ]

acc_sprepcv_lidsent=cvRF_lidsent[["measures.test"]]
acc_sprepcv_lidsent$tss=acc_sprepcv_lidsent$tpr+acc_sprepcv_lidsent$tnr-1
acc_sprepcv_lidsent=acc_sprepcv_lidsent[complete.cases(acc_sprepcv_lidsent), ]

acc_sprepcv_all=cvRF_all[["measures.test"]]
acc_sprepcv_all$tss=acc_sprepcv_all$tpr+acc_sprepcv_all$tnr-1
acc_sprepcv_all=acc_sprepcv_all[complete.cases(acc_sprepcv_all), ]

print(paste("AUC lidar:",mean(acc_sprepcv_lid$auc),"+_",sd(acc_sprepcv_lid$auc)))
print(paste("TSS lidar:",mean(acc_sprepcv_lid$tss),"+_",sd(acc_sprepcv_lid$tss)))

print(paste("AUC sentinel:",mean(acc_sprepcv_sent$auc),"+_",sd(acc_sprepcv_sent$auc)))
print(paste("TSS sentinel:",mean(acc_sprepcv_sent$tss),"+_",sd(acc_sprepcv_sent$tss)))

print(paste("AUC landcover:",mean(acc_sprepcv_landc$auc),"+_",sd(acc_sprepcv_landc$auc)))
print(paste("TSS landcover:",mean(acc_sprepcv_landc$tss),"+_",sd(acc_sprepcv_landc$tss)))

print(paste("AUC lidarsentinel:",mean(acc_sprepcv_lidsent$auc),"+_",sd(acc_sprepcv_lidsent$auc)))
print(paste("TSS lidarsentinel:",mean(acc_sprepcv_lidsent$tss),"+_",sd(acc_sprepcv_lidsent$tss)))

print(paste("AUC all:",mean(acc_sprepcv_all$auc),"+_",sd(acc_sprepcv_all$auc)))
print(paste("TSS all:",mean(acc_sprepcv_all$tss),"+_",sd(acc_sprepcv_all$tss)))

# sdm

data_forsdm_lidar <- sdmData(formula=occurrence~., train=presabs[, c(5:12,20)])
model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('cv','boot'),cv.folds=4,n=25,test.percent=30)

data_forsdm_sentinel <- sdmData(formula=occurrence~., train=presabs[, c(13:19,20)])
model_sentinel <- sdm(occurrence~.,data=data_forsdm_sentinel,methods=c('rf'),replication=c('cv','boot'),cv.folds=4,n=25,test.percent=30)

data_forsdm_landc <- sdmData(formula=occurrence~., train=presabs[, c(1:4,20)])
model_landc <- sdm(occurrence~.,data=data_forsdm_landc,methods=c('rf'),replication=c('cv','boot'),cv.folds=4,n=25,test.percent=30)

data_forsdm_lidsent <- sdmData(formula=occurrence~., train=presabs[, c(5:19,20)])
model_lidsent <- sdm(occurrence~.,data=data_forsdm_lidsent,methods=c('rf'),replication=c('cv','boot'),cv.folds=4,n=25,test.percent=30)

data_forsdm_lidall <- sdmData(formula=occurrence~., train=presabs[, c(1:19,20)])
model_lidall <- sdm(occurrence~.,data=data_forsdm_lidall,methods=c('rf'),replication=c('cv','boot'),cv.folds=4,n=25,test.percent=30)

write.sdm(model_lidar,'RFmean_Sn_lidar_bootcv')
write.sdm(model_sentinel,'RFmean_Sn_sentinel_bootcv')
write.sdm(model_landc,'RFmean_Sn_landc_bootcv')
write.sdm(model_lidsent,'RFmean_Sn_lidsent_bootcv')
write.sdm(model_lidall,'RFmean_Sn_lidall_bootcv')

acc_lidar=getEvaluation(model_lidar,stat=c('TSS','AUC'),opt=1)
print(paste("AUC lidar:",mean(acc_lidar$AUC),"+_",sd(acc_lidar$AUC)))
print(paste("TSS lidar:",mean(acc_lidar$TSS),"+_",sd(acc_lidar$TSS)))

acc_sentinel=getEvaluation(model_sentinel,stat=c('TSS','AUC'),opt=1)
print(paste("AUC sentinel:",mean(acc_sentinel$AUC),"+_",sd(acc_sentinel$AUC)))
print(paste("TSS sentinel:",mean(acc_sentinel$TSS),"+_",sd(acc_sentinel$TSS)))

acc_landc=getEvaluation(model_landc,stat=c('TSS','AUC'),opt=1)
print(paste("AUC landc:",mean(acc_landc$AUC),"+_",sd(acc_landc$AUC)))
print(paste("TSS landc:",mean(acc_landc$TSS),"+_",sd(acc_landc$TSS)))

acc_lidsent=getEvaluation(model_lidsent,stat=c('TSS','AUC'),opt=1)
print(paste("AUC lidarsentinel:",mean(acc_lidsent$AUC),"+_",sd(acc_lidsent$AUC)))
print(paste("TSS lidarsentinel:",mean(acc_lidsent$TSS),"+_",sd(acc_lidsent$TSS)))

acc_lidall=getEvaluation(model_lidall,stat=c('TSS','AUC'),opt=1)
print(paste("AUC all:",mean(acc_lidall$AUC),"+_",sd(acc_lidall$AUC)))
print(paste("TSS all:",mean(acc_lidall$TSS),"+_",sd(acc_lidall$TSS)))
