library(rgdal)
library(raster)
library(sdm)
library(ggplot2)
library(snow)
library(gridExtra)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/radar/merged/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
radar=stack(filelist)

# calc. horizontal metrics

water_class=reclassify(radar[[11]], c(c(-Inf,-25,1,-25,Inf,0)))

veg_masked_vh <- mask(radar[[11]],water_class,maskvalue=1)
veg_masked_vv <- mask(radar[[15]],water_class,maskvalue=1)
veg_masked_ndvi <- mask(radar[[3]],water_class,maskvalue=1)

beginCluster(15)

sd_VH=clusterR(veg_masked_vh, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
sd_VV=clusterR(veg_masked_vv, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
sd_ndvi=clusterR(veg_masked_ndvi, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
prop_water=clusterR(water_class, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))

sd_VH_5=clusterR(veg_masked_vh, focal, args=list(w=matrix(1,5,5), fun=sd, pad=TRUE,na.rm = TRUE))
sd_VV_5=clusterR(veg_masked_vv, focal, args=list(w=matrix(1,5,5), fun=sd, pad=TRUE,na.rm = TRUE))
sd_ndvi_5=clusterR(veg_masked_ndvi, focal, args=list(w=matrix(1,5,5), fun=sd, pad=TRUE,na.rm = TRUE))
prop_water_5=clusterR(water_class, focal, args=list(w=matrix(1,5,5), fun=sum, pad=TRUE,na.rm = TRUE))

endCluster()

writeRaster(sd_VH,'radar_VHsd_hor_100m.tif',overwrite=TRUE)
writeRaster(sd_VV,'radar_VVsd_hor_100m.tif',overwrite=TRUE)
writeRaster(sd_ndvi,'optical_NDVIsd_hor_100m.tif',overwrite=TRUE)
writeRaster(prop_water,'radar_prop_water_100m.tif',overwrite=TRUE)

writeRaster(sd_VH_5,'radar_VHsd_hor_50m.tif',overwrite=TRUE)
writeRaster(sd_VV_5,'radar_VVsd_hor_50m.tif',overwrite=TRUE)
writeRaster(sd_ndvi_5,'optical_NDVIsd_hor_50m.tif',overwrite=TRUE)
writeRaster(prop_water_5,'radar_prop_water_50m.tif',overwrite=TRUE)