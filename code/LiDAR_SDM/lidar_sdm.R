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
names(lidar) <- c("reed_prop","ppr","density1_2","density2_3","density0_1","fhd","kurto","perc_25","height","vv_std")

# extract area of interest

lidar2 <- crop(lidar, extent(studyarea))
landcover_crop <- mask(lidar2,studyarea)

# check collinearity

vifstep(lidar2,th=5)

# sdm

data_forsdm <- sdmData(formula=occurrence~reed_prop+ppr+density1_2+density2_3+density0_1+fhd+kurto+perc_25+height+vv_std, train=birds, predictors=lidar2)
data_forsdm

model <- sdm(occurrence~.,data=data_forsdm,methods=c('brt','rf','maxent'),replication=c('boot'),n=50)

rcurve(model,id=1:150)
vi <- getVarImp(model,method=c('brt','rf','maxent'))
plot(vi)

#p1 <- predict(model,newdata=landcover_crop,filename='',method=c('brt','rf','maxent'))
#writeRaster(p1,"GrW_sdm_predict.tif",overwrite=TRUE)

p1 <- ensemble(model, newdata=landcover_crop, filename='',setting=list(method='weighted',stat='AUC'))