library(sdm)
library(rgdal)
library(raster)
library(usdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/lidar/masked2/"
setwd(workingdirectory)

birdsfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs.shp"
studyareafile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/studyarea.shp"

# Import
birds = readOGR(dsn=birdsfile)
studyarea = readOGR(dsn=studyareafile)

filelist=list.files(pattern = "*.tif")
lidar=stack(filelist)
names(lidar) <- c("HH_reedveg_prop","C_ppr","HH_sd_low","HH_sd","VD_1_2","VD_2_3","VD_0_1","VV_FHD","VV_kurto","VV_p25","VV_p95","VV_std")

# extract area of interest

lidar2 <- crop(lidar, extent(studyarea))
landcover_crop <- mask(lidar2,studyarea)

# check for collinearity

vifstep(lidar2,th=5)

# sdm modelling

data_forsdm <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=lidar)
data_forsdm

model <- sdm(occurrence~.,data=data_forsdm,methods=c('brt','rf','maxent'),replication=c('cv','boot'),cv.folds=10,n=50)

p1 <- ensemble(model, newdata=landcover_crop, filename='',setting=list(method='weighted',stat='AUC'),mean=T)

# interpretations
rcurve(model,id=1:150)
vi <- getVarImp(model,method=c('brt','rf','maxent'))
plot(vi)