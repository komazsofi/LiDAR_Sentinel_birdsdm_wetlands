library(sdm)
library(rgdal)
library(raster)
library(usdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/radar/merged/masked/"
setwd(workingdirectory)

birdsfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs.shp"

# Import
birds = readOGR(dsn=birdsfile)
studyarea = readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/smallstudyareas.shp")

filelist=list.files(pattern = "*.tif")
radar=stack(filelist)

# check for collinearity

vif=vifstep(radar,th=5)

radar_selected=exclude(radar,vif)

# sdm modelling study area

data_forsdm <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=radar_selected)
data_forsdm

model <- sdm(occurrence~.,data=data_forsdm,methods=c('rf'),replication=c('cv','boot'),cv.folds=5,n=5)
model
#write.sdm(model,'ensemble_GRW_LiDAR_NL_cv5_boot_n5') 

# extract area of interest

radar2 <- crop(radar_selected, extent(studyarea))
radar_crop <- mask(radar2,studyarea)

p2 <- predict(model, newdata=radar_crop, filename='',mean=T)

# interpretations
rcurve(model)
vi <- getVarImp(model,method=c('rf'))
plot(vi)