library(sdm)
library(rgdal)
library(raster)
library(usdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(path="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/radar/merged/masked/",pattern = "*.tif",full.names = TRUE)
sentinel=stack(filelist)

filelist2=list.files(path="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/lidar/",pattern = "*.tif",full.names = TRUE)
lidar=stack(filelist2)

ahn3_acq_sp=readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/ahn3_measuretime.shp")
areaofinterest=ahn3_acq_sp[(ahn3_acq_sp@data$OBJECTID==5 | ahn3_acq_sp@data$OBJECTID==6 | ahn3_acq_sp@data$OBJECTID==11),]

# Crop lidar

lidar2 <- crop(lidar, extent(areaofinterest))
lidar_crop <- mask(lidar2, areaofinterest)

names(lidar_crop) <- c("HH_reedveg_prop","C_ppr","HH_sd_low","HH_sd","VD_1_2","VD_2_3","VD_0_1","VV_FHD","VV_p25","VV_p95","VV_std")

# Resampling
sentinel2=resample(sentinel,lidar_crop)
rasters=stack(lidar_crop,sentinel2)

#writeRaster(rasters,"lidarsentinelmerged.grd",overwrite=TRUE)
writeRaster(rasters,filename=names(rasters),bylayer=TRUE,format="GTiff")
