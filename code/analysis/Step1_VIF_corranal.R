library(raster)
library(rgdal)

library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(ggpubr)

library(sdm)
library(usdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

proj4string(all_predictor) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

landcover=subset(all_predictor, c(1,2,3,4), drop=FALSE)
lidar=subset(all_predictor, c(5:15), drop=FALSE)
optical=subset(all_predictor, c(16:20), drop=FALSE)
radar=subset(all_predictor, c(21:30), drop=FALSE)

# VIF

vif_lidar=vifstep(lidar,th=3)
vif_optical=vifstep(optical,th=3)
vif_radar=vifstep(radar,th=3)
vif_landcover=vifstep(landcover,th=3)

saveRDS(vif_lidar,file="vif_lidar.rds")
saveRDS(vif_optical,file="vif_optical.rds")
saveRDS(vif_radar,file="vif_radar.rds")
saveRDS(vif_landcover,file="vif_landcover.rds")

lidar_vif=exclude(lidar,vif_lidar)
optical_vif=exclude(optical,vif_optical)
radar_vif=exclude(radar,vif_radar)
landcover_vif=exclude(landcover,vif_landcover)

rasters=stack(lidar_vif,optical_vif,radar_vif,landcover_vif)

vif_rasters=vifstep(rasters,th=3)
saveRDS(vif_rasters,file="vif_rasters.rds")
