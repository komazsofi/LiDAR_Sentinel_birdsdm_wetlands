library(rgdal)
library(raster)
library(sp)
library(rgeos)

library(dplyr)
library(stringr)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/"
setwd(workingdirectory)

birdsfile="Reedland_bird_observations.shp"
ahn3_acqfile="ahn3_measuretime.shp"
landcoverfile="LGN7.tif"
#surveyfile="BMPplots_12530.shp"
surveyfile="BMPplots_12380.shp"

# Import
birds_sp = readOGR(dsn=birdsfile)
ahn3_acq_sp = readOGR(dsn=ahn3_acqfile)
landcover=raster(landcoverfile)
surveyplot = readOGR(dsn=surveyfile)

# Filter presence according to the needs

#birdsel_sp=birds_sp[(birds_sp@data$species=="Grote Karekiet" & birds_sp@data$year==2016),]
birdsel_sp=birds_sp[(birds_sp@data$species=="Snor" & birds_sp@data$year==2016),]
bird_ahn3ac=raster::intersect(birdsel_sp,ahn3_acq_sp)

#bird_ahn3ac_filt=bird_ahn3ac[(bird_ahn3ac@data$year==bird_ahn3ac@data$Jaar),]
bird_ahn3ac_filt=bird_ahn3ac[(bird_ahn3ac@data$OBJECTID==5),]
ahn3_acq_sp_filt=ahn3_acq_sp[(ahn3_acq_sp@data$OBJECTID==5 | ahn3_acq_sp@data$OBJECTID==6 | ahn3_acq_sp@data$OBJECTID==11),]

surveyplot_filt=raster::intersect(surveyplot,ahn3_acq_sp_filt)

# Cut landcover map for area of interest

landcover2 <- crop(landcover, extent(ahn3_acq_sp_filt))
landcover_crop <- mask(landcover2, ahn3_acq_sp_filt)

# select required landcover classes and create the mask

bird_ahn3ac_filt_wlc=raster::extract(landcover_crop,bird_ahn3ac_filt)
bird_ahn3ac_filt@data$landcover <- bird_ahn3ac_filt_wlc

formask <- setValues(raster(landcover_crop), NA)
formask[landcover_crop==16 |landcover_crop==30 | landcover_crop==41 | landcover_crop==42 | landcover_crop==43 | landcover_crop==45] <- 1
proj4string(formask)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# where not to place absences
bird_ahn3ac_filt_buff <- gBuffer(bird_ahn3ac_filt, width = 200)
proj4string(bird_ahn3ac_filt_buff)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")


# generate absences
absence_cand=spsample(surveyplot_filt,n=2500,"random")
absence_cand.df=as.data.frame(absence_cand)
absence_cand.df$occurrence <- 0

absence_cand.df$X_obs=absence_cand.df$x
absence_cand.df$Y_obs=absence_cand.df$y
coordinates(absence_cand.df)=~X_obs+Y_obs
proj4string(absence_cand.df)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

absence_cand_add1=raster::extract(formask,absence_cand.df)
absence_cand.df@data$wetland_mask <- absence_cand_add1

absence_cand.df_int <- gIntersects(absence_cand.df,bird_ahn3ac_filt_buff, byid=TRUE)
absence_cand.df@data$presence <- absence_cand.df_int[,1]

absence=absence_cand.df@data[absence_cand.df@data$wetland_mask==1 & absence_cand.df@data$presence==FALSE,]
absence=absence[complete.cases(absence), ]

absence$X_obs=absence$x
absence$Y_obs=absence$y
coordinates(absence)=~X_obs+Y_obs
proj4string(absence)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# create pres-abs file for SDM

absence.df=as.data.frame(absence)
presence.df=as.data.frame(bird_ahn3ac_filt)

absence.dfsel=subset(absence.df,select=c(1,2,3))
presence.dfsel=subset(presence.df,select=c(6,7,10))
names(presence.dfsel)<- names(absence.dfsel)

presabs=rbind(absence.dfsel,presence.dfsel)

presabs$X_obs=presabs$x
presabs$Y_obs=presabs$y
coordinates(presabs)=~X_obs+Y_obs
proj4string(presabs)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Export
raster::shapefile(presabs,"presabs_Sn_rand_studyarea",overwrite=TRUE)

#writeRaster(landcover_crop,'landcover_crop.tif',overwrite=TRUE)
#writeRaster(formask,"wetland_mask.tif",overwrite=TRUE)
