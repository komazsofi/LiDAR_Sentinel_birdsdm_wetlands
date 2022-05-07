library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(snow)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/lidar/"
setwd(workingdirectory)

heightfile="ahn3_feat_10m_1m_veg_TILE_000_BAND_perc_95_normalized_height.tif"

# Import
height=raster(heightfile)

# calculate horizontal metrics

height_class_reed=reclassify(height, c(c(-Inf,1,0,1,3,1,3,5,0,5,Inf,0)))
height_class_lowveg=reclassify(height, c(c(-Inf,5,1,5,Inf,0)))

height_masked <- mask(height,height_class_lowveg,maskvalue=0)

beginCluster(15)

sd_dsm=clusterR(height, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
sd_dsm_low=clusterR(height_masked, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
#prop_reedveg=clusterR(height_class_reed, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))

endCluster()

# export

writeRaster(sd_dsm,"ahn3_feat_10m_1m_all_TILE_000_BAND_sd_dsm.tif",overwrite=TRUE)
writeRaster(sd_dsm_low,"ahn3_feat_10m_1m_all_TILE_000_BAND_sd_dsm_low.tif",overwrite=TRUE)
#writeRaster(prop_reedveg,"ahn3_feat_10m_1m_all_TILE_000_BAND_prop_reedveg.tif",overwrite=TRUE)
