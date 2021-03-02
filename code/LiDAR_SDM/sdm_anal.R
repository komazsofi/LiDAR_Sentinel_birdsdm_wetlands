library(sdm)
library(rgdal)
library(raster)
library(usdm)
library(ggcorrplot)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

# VIF

vif=vifstep(all_predictor,th=3)