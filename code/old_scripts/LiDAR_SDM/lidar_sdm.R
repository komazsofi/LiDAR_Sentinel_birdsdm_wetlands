library(sdm)
library(rgdal)
library(raster)
library(usdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/lidar/masked2/"
setwd(workingdirectory)

birdsfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs_800rand_studyarea.shp"

# Import
birds = readOGR(dsn=birdsfile)
studyarea = readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/studyarea.shp")
studyarea2 = readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/smallstudyareas.shp")

filelist=list.files(pattern = "*.tif")
lidar=stack(filelist)
names(lidar) <- c("HH_reedveg_prop","C_ppr","HH_sd_low","HH_sd","VD_1_2","VD_2_3","VD_0_1","VV_FHD","VV_p25","VV_p95","VV_std")

# extract area of interest

lidar2 <- crop(lidar, extent(studyarea))
lidar_crop <- mask(lidar2,studyarea)

# check for collinearity

vif=vifstep(lidar_crop,th=5)
lidar_selected=exclude(lidar_crop,vif)
writeRaster(lidar_selected,"D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/both/lidar.grd",overwrite=TRUE)

# sdm modelling study area

data_forsdm <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=lidar_selected)
data_forsdm

model <- sdm(occurrence~.,data=data_forsdm,methods=c('rf'),replication=c('boot'),n=10)
model
#write.sdm(model,'ensemble_GRW_LiDAR_NL_cv5_boot_n5') 

# extract area of interest

lidar3 <- crop(lidar_selected, extent(studyarea2))
lidar_crop2 <- mask(lidar3,studyarea2)

#p1 <- ensemble(model, newdata=lidar_crop_df[,-c(1,2)], filename='',setting=list(method='weighted',stat='AUC'))
p2 <- predict(model, newdata=lidar_crop2, filename='',mean=T)

# interpretations
rcurve(model)
vi <- getVarImp(model,method=c('rf'))
plot(vi)
