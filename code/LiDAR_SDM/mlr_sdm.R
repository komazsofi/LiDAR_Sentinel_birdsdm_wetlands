library(dismo)
library(raster)
library(mlr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(mmpf)
library(iml)
library(patchwork)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

bird=readOGR(dsn="presabs_GrW_rand_studyarea.shp")

# Intersect
mydata <- raster::extract(all_predictor, bird, df = TRUE)
mydata$occurrence <- bird$occurrence
mydata$x <- bird$x
mydata$y <- bird$y

mydata_clean=mydata[complete.cases(mydata), ]
mydata_clean <- mydata_clean[,-1]

# !!!!needs to be manually adjusted!!!!

pres=mydata_clean[mydata_clean$occurrence==1,]
abs=mydata_clean[mydata_clean$occurrence==0,]

#pres_sampled=pres[sample(nrow(pres), 675), ]
#presabs=rbind(pres_sampled,abs)

abs_sampled=abs[sample(nrow(abs), 200), ]
presabs=rbind(pres,abs_sampled)

presabs$occurrence<-factor(presabs$occurrence)

# start mlr

set.seed(11)

task = makeClassifTask(data = presabs[,c(1:19,20)], target = "occurrence",
                       positive = "1", coordinates = presabs[,c(21,22)])

lrnRF = makeLearner("classif.randomForest",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

perf_level_spCV = makeResampleDesc(method = "SpRepCV", folds = 3, reps = 25)

cvRF = resample(learner = lrnRF, task =task,
                resampling = perf_level_spCV, 
                measures = mlr::auc)

ctrl = makeTuneControlRandom(maxit = 10L)

paramsRF <- makeParamSet(
  makeIntegerParam("mtry",lower = 1,upper = 8),
  makeIntegerParam("nodesize",lower = 1,upper = 10)
)

tuneRF = tuneParams (learner = lrnRF, 
                     task=task,
                     resampling = perf_level_spCV,
                     par.set = paramsRF,
                     control = ctrl, 
                     measures = mlr::auc,
                     show.info = TRUE)

lrn = makeTuneWrapper(lrnRF, resampling = perf_level_spCV, par.set = paramsRF, control = ctrl)

model_all = train(lrn, task)

# analyse

predictor = Predictor$new(model_all, data = presabs[,c(1:19)], y = presabs$occurrence)
imp = FeatureImp$new(predictor, loss = "ce")
plot(imp)

pdp.obj = Partial$new(predictor, feature = "optical_NDVIsd_hor_100m")
plot(pdp.obj)

effect = FeatureEffects$new(predictor,method="pdp")
effect$plot(features = "optical_NDVIsd_hor_100m")

