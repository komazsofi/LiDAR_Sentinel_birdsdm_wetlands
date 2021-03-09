library(raster)
library(mlr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(mmpf)
library(patchwork)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

presabs_Sn=read.csv("presabs_Sn.csv")
presabs_Ba=read.csv("presabs_Ba.csv")
presabs_GrW=read.csv("presabs_GrW.csv")

presabs_Sn$occurrence<-factor(presabs_Sn$occurrence)
presabs_Sn=presabs_Sn[,-1]
presabs_Ba$occurrence<-factor(presabs_Ba$occurrence)
presabs_Ba=presabs_Ba[,-1]
presabs_GrW$occurrence<-factor(presabs_GrW$occurrence)
presabs_GrW=presabs_GrW[,-1]

# create the area of interest (based on a defined polygon) for prediction

studyarea = readOGR(dsn="studyarea1.shp")

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

proj4string(all_predictor) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

extract <- crop(all_predictor, extent(studyarea))
predictors_crop <- mask(extract,studyarea)

predictors_crop_df <- as.data.frame(predictors_crop, xy = T, na.rm = T)

# start mlr

set.seed(11)

tasklid_Sn = makeClassifTask(data = presabs_Sn[,c(5:11,19)], target = "occurrence",
                             positive = "1", coordinates = presabs_Sn[,c(20,21)])

tasklid.over_Sn = oversample(tasklid_Sn, rate = 2)

tasksent_Sn = makeClassifTask(data = presabs_Sn[,c(12:18,19)], target = "occurrence",
                             positive = "1", coordinates = presabs_Sn[,c(20,21)])

tasksent.over_Sn = oversample(tasksent_Sn, rate = 2)

tasklandc_Sn = makeClassifTask(data = presabs_Sn[,c(1:4,19)], target = "occurrence",
                             positive = "1", coordinates = presabs_Sn[,c(20,21)])

tasklandc.over_Sn = oversample(tasklandc_Sn, rate = 2)

tasklid_Ba = makeClassifTask(data = presabs_Ba[,c(5:11,19)], target = "occurrence",
                             positive = "1", coordinates = presabs_Ba[,c(20,21)])

tasklid.over_Ba = oversample(tasklid_Ba, rate = 2)

tasksent_Ba = makeClassifTask(data = presabs_Ba[,c(12:18,19)], target = "occurrence",
                              positive = "1", coordinates = presabs_Ba[,c(20,21)])

tasksent.over_Ba = oversample(tasksent_Ba, rate = 2)

tasklandc_Ba = makeClassifTask(data = presabs_Ba[,c(1:4,19)], target = "occurrence",
                               positive = "1", coordinates = presabs_Ba[,c(20,21)])

tasklandc.over_Ba = oversample(tasklandc_Ba, rate = 2)

tasklid_GrW = makeClassifTask(data = presabs_GrW[,c(5:11,19)], target = "occurrence",
                             positive = "1", coordinates = presabs_GrW[,c(20,21)])

tasklid.over_GrW = oversample(tasklid_GrW, rate = 4)

tasksent_GrW = makeClassifTask(data = presabs_GrW[,c(12:18,19)], target = "occurrence",
                              positive = "1", coordinates = presabs_GrW[,c(20,21)])

tasksent.over_GrW = oversample(tasksent_GrW, rate = 4)

tasklandc_GrW = makeClassifTask(data = presabs_GrW[,c(1:4,19)], target = "occurrence",
                               positive = "1", coordinates = presabs_GrW[,c(20,21)])

tasklandc.over_GrW = oversample(tasklandc_GrW, rate = 4)

# fit model

lrnRF = makeLearner("classif.randomForest",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

modellid_Sn = mlr::train(lrnRF, tasklid.over_Sn)
modelsent_Sn = mlr::train(lrnRF, tasksent.over_Sn)
modellandc_Sn = mlr::train(lrnRF, tasklandc.over_Sn)

modellid_Ba = mlr::train(lrnRF, tasklid.over_Ba)
modelsent_Ba = mlr::train(lrnRF, tasksent.over_Ba)
modellandc_Ba = mlr::train(lrnRF, tasklandc.over_Ba)

modellid_GrW = mlr::train(lrnRF, tasklid.over_GrW)
modelsent_GrW = mlr::train(lrnRF, tasksent.over_GrW)
modellandc_GrW = mlr::train(lrnRF, tasklandc.over_GrW)

# predict

predictlid_Sn=predict(modellid_Sn, newdata = predictors_crop_df[,c(7:13)])
predictlid_Sn_coord=cbind(predictlid_Sn,predictors_crop_df[,c(1,2)])

Sn_predlid_raster=rasterFromXYZ(predictlid_Sn_coord[,c(4,5,2)])

predictsent_Sn=predict(modelsent_Sn, newdata = predictors_crop_df[,c(14:20)])
predictsent_Sn_coord=cbind(predictsent_Sn,predictors_crop_df[,c(1,2)])

Sn_predsent_raster=rasterFromXYZ(predictsent_Sn_coord[,c(4,5,2)])

predictlandc_Sn=predict(modellandc_Sn, newdata = predictors_crop_df[,c(3:6)])
predictlandc_Sn_coord=cbind(predictlandc_Sn,predictors_crop_df[,c(1,2)])

Sn_predlandc_raster=rasterFromXYZ(predictlandc_Sn_coord[,c(4,5,2)])

predictlid_Ba=predict(modellid_Ba, newdata = predictors_crop_df[,c(7:13)])
predictlid_Ba_coord=cbind(predictlid_Ba,predictors_crop_df[,c(1,2)])

Ba_predlid_raster=rasterFromXYZ(predictlid_Ba_coord[,c(4,5,2)])

predictsent_Ba=predict(modelsent_Ba, newdata = predictors_crop_df[,c(14:20)])
predictsent_Ba_coord=cbind(predictsent_Ba,predictors_crop_df[,c(1,2)])

Ba_predsent_raster=rasterFromXYZ(predictsent_Ba_coord[,c(4,5,2)])

predictlandc_Ba=predict(modellandc_Ba, newdata = predictors_crop_df[,c(3:6)])
predictlandc_Ba_coord=cbind(predictlandc_Ba,predictors_crop_df[,c(1,2)])

Ba_predlandc_raster=rasterFromXYZ(predictlandc_Ba_coord[,c(4,5,2)])

predictlid_GrW=predict(modellid_GrW, newdata = predictors_crop_df[,c(7:13)])
predictlid_GrW_coord=cbind(predictlid_GrW,predictors_crop_df[,c(1,2)])

GrW_predlid_raster=rasterFromXYZ(predictlid_GrW_coord[,c(4,5,2)])

predictsent_GrW=predict(modelsent_GrW, newdata = predictors_crop_df[,c(14:20)])
predictsent_GrW_coord=cbind(predictsent_GrW,predictors_crop_df[,c(1,2)])

GrW_predsent_raster=rasterFromXYZ(predictsent_GrW_coord[,c(4,5,2)])

predictlandc_GrW=predict(modellandc_GrW, newdata = predictors_crop_df[,c(3:6)])
predictlandc_GrW_coord=cbind(predictlandc_GrW,predictors_crop_df[,c(1,2)])

GrW_predlandc_raster=rasterFromXYZ(predictlandc_GrW_coord[,c(4,5,2)])

rasters=stack(Sn_predlid_raster,Sn_predsent_raster,Sn_predlandc_raster,
              Ba_predlid_raster,Ba_predsent_raster,Ba_predlandc_raster,
              GrW_predlid_raster,GrW_predsent_raster,GrW_predlandc_raster)

writeRaster(rasters,"predictedmaps.grd",overwrite=TRUE)
rasters=stack("predictedmaps.grd")

# visualization
