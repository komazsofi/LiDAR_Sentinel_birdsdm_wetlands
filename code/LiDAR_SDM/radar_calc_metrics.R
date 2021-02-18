library(rgdal)
library(raster)
library(sdm)
library(ggplot2)
library(snow)
library(gridExtra)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/radar2/merged/"
setwd(workingdirectory)

birdsfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs_800rand_studyarea.shp"

# Import

filelist=list.files(pattern = "*.tif")
radar=stack(filelist)

birds = readOGR(dsn=birdsfile)

# calc. horizontal metrics

water_class=reclassify(radar[[1]], c(c(-Inf,-25,1,-25,Inf,0)))

veg_masked_vh <- mask(radar[[1]],water_class,maskvalue=1)
veg_masked_vv <- mask(radar[[3]],water_class,maskvalue=1)
veg_masked_ndvi <- mask(radar[[7]],water_class,maskvalue=1)

beginCluster(15)

sd_VH=clusterR(veg_masked_vh, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
sd_VV=clusterR(veg_masked_vv, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
sd_ndvi=clusterR(veg_masked_ndvi, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
prop_water=clusterR(water_class, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))

endCluster()

writeRaster(sd_VH,'radar_metricsd_sd_VH_hor.tif',overwrite=TRUE)
writeRaster(sd_VV,'radar_metricsd_sd_VV_hor.tif',overwrite=TRUE)
writeRaster(sd_ndvi,'radar_metricsd_sd_ndvi_hor.tif',overwrite=TRUE)
writeRaster(prop_water,'radar_metricprop_water.tif',overwrite=TRUE)