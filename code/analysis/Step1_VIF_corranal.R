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

landcover=subset(all_predictor, c(1:6), drop=FALSE)
lidar=subset(all_predictor, c(7:18), drop=FALSE)
sentinel=subset(all_predictor, c(19:33), drop=FALSE)

# VIF

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April/"
setwd(workingdirectory)

vif_lidar=vifstep(lidar,th=3)
vif_sentinel=vifstep(sentinel,th=3)
vif_landcover=vifstep(landcover,th=3)

setwd("D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April_results/")

saveRDS(vif_lidar,file="vif_lidar2.rds")
saveRDS(vif_sentinel,file="vif_sentinel2.rds")
saveRDS(vif_landcover,file="vif_landcover2.rds")

lidar_vif=exclude(lidar,vif_lidar)
sentinel_vif=exclude(sentinel,vif_sentinel)
landcover_vif=exclude(landcover,vif_landcover)

rasters=stack(lidar_vif,sentinel_vif,landcover_vif)

vif_rasters=vifstep(rasters,th=3)
saveRDS(vif_rasters,file="vif_rasters2.rds")
