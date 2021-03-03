library(sdm)
library(rgdal)
library(raster)
library(usdm)
library(ggcorrplot)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

bird=readOGR(dsn="presabs_Sn_rand_studyarea.shp")

lidar=subset(all_predictor, c(1,2,3,4,5,6,7,8,9,10,11), drop=FALSE)
optical=subset(all_predictor, c(12,13,14,15,16,17,18,19,20,21), drop=FALSE)
radar=subset(all_predictor, c(22,23,24,25,26,27,28,29,30,31,32,33,34,35), drop=FALSE)

# VIF

vif_lidar=vifstep(lidar,th=3)
vif_optical=vifstep(optical,th=3)
vif_radar=vifstep(radar,th=3)

lidar_vif=exclude(lidar,vif_lidar)
optical_vif=exclude(optical,vif_optical)
radar_vif=exclude(radar,vif_radar)

# sdm only lidar

data_forsdm_lidar <- sdmData(formula=occurrence~., train=bird[,-c(1,2)], predictors=lidar_vif)
data_forsdm_lidar

model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
model_lidar

# interpretations
rcurve(model_lidar)
vi_lidar <- getVarImp(model_lidar,method=c('rf'))
plot(vi_lidar)

# sdm only optical

data_forsdm_optical <- sdmData(formula=occurrence~., train=bird[,-c(1,2)], predictors=optical_vif)
data_forsdm_optical

model_optical <- sdm(occurrence~.,data=data_forsdm_optical,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
model_optical

# interpretations
rcurve(model_optical)
vi_optical <- getVarImp(model_optical,method=c('rf'))
plot(vi_optical)

# sdm only radar

data_forsdm_radar <- sdmData(formula=occurrence~., train=bird[,-c(1,2)], predictors=radar_vif)
data_forsdm_radar

model_radar <- sdm(occurrence~.,data=data_forsdm_radar,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
model_radar

# interpretations
rcurve(model_radar)
vi_radar <- getVarImp(model_radar,method=c('rf'))
plot(vi_radar)

# sdm optical+radar

sentinel=stack(optical_vif,radar_vif)

data_forsdm_sentinel <- sdmData(formula=occurrence~., train=bird[,-c(1,2)], predictors=sentinel)
data_forsdm_sentinel

model_sentinel <- sdm(occurrence~.,data=data_forsdm_sentinel,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
model_sentinel

# interpretations
rcurve(model_sentinel)
vi_sentinel <- getVarImp(model_sentinel,method=c('rf'))
plot(vi_sentinel)

# sdm lidar+optical

lidar_optical=stack(optical_vif,lidar_vif)

data_forsdm_lidar_optical <- sdmData(formula=occurrence~., train=bird[,-c(1,2)], predictors=lidar_optical)
data_forsdm_lidar_optical

model_lidar_optical <- sdm(occurrence~.,data=data_forsdm_lidar_optical,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
model_lidar_optical

# interpretations
rcurve(model_lidar_optical)
vi_lidar_optical <- getVarImp(model_lidar_optical,method=c('rf'))
plot(vi_lidar_optical)

# sdm lidar+radar

lidar_radar=stack(lidar_vif,radar_vif)

data_forsdm_lidar_radar <- sdmData(formula=occurrence~., train=bird[,-c(1,2)], predictors=lidar_radar)
data_forsdm_lidar_radar

model_lidar_radar <- sdm(occurrence~.,data=data_forsdm_lidar_radar,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
model_lidar_radar

# interpretations
rcurve(model_lidar_radar)
vi_lidar_radar <- getVarImp(model_lidar_radar,method=c('rf'))
plot(vi_lidar_radar)

# sdm lidar+optical+radar

all=stack(optical_vif,radar_vif,lidar_vif)

data_forsdm_all <- sdmData(formula=occurrence~., train=bird[,-c(1,2)], predictors=all)
data_forsdm_all

model_all <- sdm(occurrence~.,data=data_forsdm_all,methods=c('rf'),replication=c('boot'),n=25,test.percent=30)
model_all

# interpretations
rcurve(model_all)
vi_all <- getVarImp(model_all,method=c('rf'))
plot(vi_all)

# Export models

write.sdm(model_lidar,'RFmean_Sn_lidar_boot_n25')
write.sdm(model_optical,'RFmean_Sn_optical_boot_n25')
write.sdm(model_radar,'RFmean_Sn_radar_boot_n25')
write.sdm(model_sentinel,'RFmean_Sn_sentinel_boot_n25')
write.sdm(model_lidar_optical,'RFmean_Sn_lidaroptical_boot_n25')
write.sdm(model_lidar_radar,'RFmean_Sn_lidarradar_boot_n25')
write.sdm(model_all,'RFmean_Sn_all_boot_n25')