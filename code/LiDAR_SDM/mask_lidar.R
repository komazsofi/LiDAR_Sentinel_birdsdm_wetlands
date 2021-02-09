library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
library(stringr)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/lidar/"
setwd(workingdirectory)

landcoverfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/LGN7.tif"
humanobjectfile="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/input_formask/powerlines_buff20.shp"

# Import
landcover=raster(landcoverfile)
humanobject = readOGR(dsn=humanobjectfile)

# process files

filelist=list.files(pattern = "*.tif")
  
lidar=stack(filelist[1])
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

landcover_crop=crop(landcover,extent(lidar))
  
formask <- setValues(raster(landcover_crop), NA)
formask[landcover_crop==16 |landcover_crop==30 | landcover_crop==41 | landcover_crop==42 | landcover_crop==43 | landcover_crop==45] <- 1
proj4string(formask)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
lgn7_mask_resampled=resample(formask,lidar)
  
# apply
lidar_masked <- mask(lidar, lgn7_mask_resampled)
  


