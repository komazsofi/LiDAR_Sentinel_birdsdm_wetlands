library(raster)
library(mlr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(mmpf)
library(patchwork)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

#presabs=read.csv("presabs_Sn.csv")
#presabs=read.csv("presabs_Ba.csv")
presabs=read.csv("presabs_GrW.csv")
presabs$occurrence<-factor(presabs$occurrence)
presabs=presabs[,-1]

# start mlr

set.seed(11)

tasklid = makeClassifTask(data = presabs[,c(5:11,19)], target = "occurrence",
                          positive = "1", coordinates = presabs[,c(20,21)])
tasksent = makeClassifTask(data = presabs[,c(12:18,19)], target = "occurrence",
                           positive = "1", coordinates = presabs[,c(20,21)])
tasklandc = makeClassifTask(data = presabs[,c(1:4,19)], target = "occurrence",
                            positive = "1", coordinates = presabs[,c(20,21)])
tasklidsent = makeClassifTask(data = presabs[,c(5:18,19)], target = "occurrence",
                              positive = "1", coordinates = presabs[,c(20,21)])
taskall = makeClassifTask(data = presabs[,c(1:18,19)], target = "occurrence",
                          positive = "1", coordinates = presabs[,c(20,21)])

table(getTaskTargets(taskall))

# Sn rate 2
# Ba rate 2.5
# GrW rate 4 absence

task.over_lid = oversample(tasklid, rate = 4)
task.over_sent = oversample(tasksent, rate = 4)
task.over_landc = oversample(tasklandc, rate = 4)
task.over_lidsent = oversample(tasklidsent, rate = 4)
task.over_all = oversample(taskall, rate = 4)

table(getTaskTargets(task.over_all))

lrnRF = makeLearner("classif.randomForest",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

#perf_level_spCV = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 20)
perf_level_spCV = makeResampleDesc(method = "RepCV", folds = 5, reps = 20)

# accuracy measure

cvRF_lid = resample(learner = lrnRF, task =task.over_lid,
                    resampling = perf_level_spCV, 
                    measures = list(auc,tpr,tnr))

cvRF_sent = resample(learner = lrnRF, task =task.over_sent,
                     resampling = perf_level_spCV, 
                     measures = list(auc,tpr,tnr))

cvRF_landc = resample(learner = lrnRF, task =task.over_landc,
                      resampling = perf_level_spCV, 
                      measures = list(auc,tpr,tnr))

cvRF_lidsent = resample(learner = lrnRF, task =task.over_lidsent,
                        resampling = perf_level_spCV, 
                        measures = list(auc,tpr,tnr))

cvRF_all = resample(learner = lrnRF, task =task.over_all,
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

sink(paste("acc_GrW_rep.txt",sep=""))

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

sink()