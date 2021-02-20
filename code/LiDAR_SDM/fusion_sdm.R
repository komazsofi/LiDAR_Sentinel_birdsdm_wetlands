library(sdm)
library(rgdal)
library(raster)
library(usdm)
library(ggcorrplot)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/both/"
setwd(workingdirectory)

birdsfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs_GrW_rand_studyarea.shp"

# Import
birds = readOGR(dsn=birdsfile)
studyarea = readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/studyarea2.shp")

filelist=list.files(pattern = "*.tif")
radar=stack(filelist)

lidar=stack("lidar.grd")

radar2=resample(lidar,radar)

rasters=stack(radar2,radar)
#writeRaster(rasters,"lidar_sentinel.grd",overwrite=TRUE)

# check for collinearity

vif=vifstep(rasters,th=5)
rasters=exclude(rasters,vif)

#radar_selected <- dropLayer(radar_selected, c(9))

# sdm only lidar

data_forsdm_lidar <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=lidar)
data_forsdm_lidar

model_lidar <- sdm(occurrence~.,data=data_forsdm_lidar,methods=c('rf'),replication=c('boot'),n=25)
model_lidar

# interpretations
rcurve(model_lidar)
vi_lidar <- getVarImp(model_lidar,method=c('rf'))
plot(vi_lidar)

# sdm only sentinel

data_forsdm_sentinel <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=radar)
data_forsdm_sentinel

model_sentinel <- sdm(occurrence~.,data=data_forsdm_sentinel,methods=c('rf'),replication=c('boot'),n=25)
model_sentinel

# interpretations
rcurve(model_sentinel)
vi_sentinel <- getVarImp(model_sentinel,method=c('rf'))
plot(vi_sentinel)

# sdm fuse

data_forsdm <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=rasters)
data_forsdm

model <- sdm(occurrence~.,data=data_forsdm,methods=c('rf'),replication=c('boot'),n=25)
model

# interpretations
rcurve(model)
vi <- getVarImp(model,method=c('rf'))
plot(vi)

# export models

write.sdm(model_lidar,'RFmean_GrW_LiDAR_small_boot_n5')
write.sdm(model_sentinel,'RFmean_GrW_Sentinel_small_boot_n5')
write.sdm(model,'RFmean_GrW_Both_small_boot_n5')

# predict for area of interests

lidar3 <- crop(lidar, extent(studyarea))
lidar_crop <- mask(lidar3,studyarea)

radar3 <- crop(radar, extent(studyarea))
radar_crop <- mask(radar3,studyarea)

all3 <- crop(rasters, extent(studyarea))
all_crop <- mask(all3,studyarea)

plidar <- predict(model_lidar, newdata=lidar_crop, filename='',mean=T)
psentinel <- predict(model_sentinel, newdata=radar_crop, filename='',mean=T)
pall <- predict(model, newdata=all_crop, filename='',mean=T)

# analyze

data=data_forsdm@features
data$occ <- 0
data$occ[164:237]<-1

data2=data[data$occ==1,]

r <- cor(data2[2:17])

ggcorrplot(r,
           type = "lower",
           lab = TRUE)
