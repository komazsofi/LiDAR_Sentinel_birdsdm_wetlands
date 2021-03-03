library(sdm)
library(rgdal)
library(raster)
library(usdm)
library(snow)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(path="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/radar/merged/masked/",pattern = "*.tif",full.names = TRUE)
sentinel=stack(filelist)

filelist2=list.files(path="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/lidar/",pattern = "*.tif",full.names = TRUE)
lidar=stack(filelist2)

ahn3_acq_sp=readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/ahn3_measuretime.shp")
areaofinterest=ahn3_acq_sp[(ahn3_acq_sp@data$OBJECTID==5 | ahn3_acq_sp@data$OBJECTID==6 | ahn3_acq_sp@data$OBJECTID==11),]

landcoverfile=stack("D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/LGN7.tif")

# Crop lidar

lidar2 <- crop(lidar, extent(areaofinterest))
lidar_crop <- mask(lidar2, areaofinterest)

names(lidar_crop) <- c("HH_reedveg_prop","C_ppr","HH_sd_low","HH_sd","VD_1_2","VD_2_3","VD_0_1","VV_FHD","VV_p25","VV_p95","VV_std")

# Resampling
sentinel2=resample(sentinel,lidar_crop)
rasters=stack(lidar_crop,sentinel2)

#writeRaster(rasters,"lidarsentinelmerged.grd",overwrite=TRUE)
writeRaster(rasters,filename=names(rasters),bylayer=TRUE,format="GTiff")

# add landcover file 

filelist=list.files(path="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/",pattern = "*.tif",full.names = TRUE)
all_predictor=stack(filelist)

landcover2 <- crop(landcoverfile, extent(areaofinterest))
landcover <- mask(landcover2, areaofinterest)

landcover_resamp=resample(landcover,all_predictor[[1]])
landcover_resamp_mask <- mask(landcover_resamp,all_predictor[[1]],maskvalue=NA)

# calc lancover metrics

reed_class=reclassify(landcover_resamp_mask, c(c(-Inf,41,0,42,42,1,43,Inf,0)))
saltmarsh_class=reclassify(landcover_resamp_mask, c(c(-Inf,29,0,30,30,1,31,Inf,0)))
swamp_class=reclassify(landcover_resamp_mask, c(c(-Inf,40,0,41,41,1,42,Inf,0)))
water_class=reclassify(landcover_resamp_mask, c(c(-Inf,15,0,16,16,1,17,Inf,0)))

beginCluster(15)

landcover_propreed=clusterR(reed_class, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))
landcover_propsaltmarsh=clusterR(saltmarsh_class, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))
landcover_propswamp=clusterR(swamp_class, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))
landcover_propwater=clusterR(water_class, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))

endCluster()

writeRaster(landcover_resamp_mask,'landcover_cat.tif',overwrite=TRUE)
writeRaster(landcover_propreed,'landcover_propreed.tif',overwrite=TRUE)
writeRaster(landcover_propswamp,'landcover_propswamp.tif',overwrite=TRUE)
writeRaster(landcover_propsaltmarsh,'landcover_propsaltmarsh.tif',overwrite=TRUE)
writeRaster(landcover_propwater,'landcover_propwater.tif',overwrite=TRUE)

