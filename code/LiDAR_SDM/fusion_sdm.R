library(sdm)
library(rgdal)
library(raster)
library(usdm)
library(ggcorrplot)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/both/"
setwd(workingdirectory)

birdsfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs_800rand_studyarea.shp"

# Import
birds = readOGR(dsn=birdsfile)
studyarea = readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/smallstudyareas.shp")

filelist=list.files(pattern = "*.tif")
radar=stack(filelist)

lidar=stack("lidar.grd")

radar2=resample(lidar,radar)

rasters=stack(radar2,radar)
writeRaster(rasters,"lidar_sentinel.grd",overwrite=TRUE)

# check for collinearity

vif=vifstep(rasters,th=5)

radar_selected=exclude(rasters,vif)
radar_selected=rasters

# sdm modelling study area

data_forsdm <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=radar_selected)
data_forsdm

model <- sdm(occurrence~.,data=data_forsdm,methods=c('rf'),replication=c('boot'),n=25)
model

model2 <- sdm(occurrence~.,data=data_forsdm,methods=c('maxent'),replication=c('boot'),n=25)
model2
#write.sdm(model,'ensemble_GRW_LiDAR_NL_cv5_boot_n5') 

# extract area of interest

radar2 <- crop(radar_selected, extent(studyarea))
radar_crop <- mask(radar2,studyarea)

p2 <- predict(model, newdata=radar_crop, filename='',mean=T)

# interpretations
rcurve(model)
vi <- getVarImp(model,method=c('rf'))
plot(vi)

# analyze

data=data_forsdm@features
data$occ <- 0
data$occ[164:237]<-1

data2=data[data$occ==1,]

r <- cor(data2[2:17])

ggcorrplot(r,
           type = "lower",
           lab = TRUE)
