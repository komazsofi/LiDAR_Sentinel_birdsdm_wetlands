library(sdm)
library(rgdal)
library(raster)
library(usdm)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/lidar/masked2/"
setwd(workingdirectory)

birdsfile="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs.shp"
studyareafile="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/studyarea.shp"

# Import
birds = readOGR(dsn=birdsfile)
studyarea = readOGR(dsn=studyareafile)

filelist=list.files(pattern = "*.tif")
lidar=stack(filelist)
names(lidar) <- c("reed_prop","ppr","density1_2","density2_3","density0_1","fhd","kurto","perc_25","height","vv_std")

# extract area of interest

lidar2 <- crop(lidar, extent(studyarea))

# check collinearity

vifcor(lidar2,th=0.7,method='spearman')
