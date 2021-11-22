library(rgdal)
library(raster)
library(sp)
library(rgeos)

workingdirectory="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Revision/"
setwd(workingdirectory)

ahn3_acqfile="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/1_Dataset/Filters/ahn3_measuretime.shp"
maskfile="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/1_Dataset/Processed_data_forsdm/lidar_VV_p25.tif"
#surveyfile="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/1_Dataset/Birddata/BMPplots_12380.shp"
surveyfile="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/1_Dataset/Birddata/BMPplots_12530.shp"
birdsfile="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/1_Dataset/Birddata/Reedland_bird_observations.shp"

# Import
birds_sp = readOGR(dsn=birdsfile)
ahn3_acq_sp = readOGR(dsn=ahn3_acqfile)
datalayers=raster(maskfile)
surveyplot = readOGR(dsn=surveyfile)

# Import
birds_sp = readOGR(dsn=birdsfile)
ahn3_acq_sp = readOGR(dsn=ahn3_acqfile)
datalayers=raster(maskfile)
surveyplot = readOGR(dsn=surveyfile)

# Filter presence according to the needs

birdsel_sp=birds_sp[(birds_sp@data$species=="Grote Karekiet"),]
#birdsel_sp=birds_sp[(birds_sp@data$species=="Snor"),]
bird_ahn3ac=raster::intersect(birdsel_sp,ahn3_acq_sp)

bird_ahn3ac_filt=bird_ahn3ac[(bird_ahn3ac@data$year==bird_ahn3ac@data$Jaar),]
bird_ahn3ac_filt2=bird_ahn3ac_filt[(bird_ahn3ac_filt@data$OBJECTID==5 | bird_ahn3ac_filt@data$OBJECTID==6 | bird_ahn3ac_filt@data$OBJECTID==11),]

ahn3_acq_sp_filt=ahn3_acq_sp[(ahn3_acq_sp@data$OBJECTID==5 | ahn3_acq_sp@data$OBJECTID==6 | ahn3_acq_sp@data$OBJECTID==11),]
surveyplot_filt=raster::intersect(surveyplot,ahn3_acq_sp_filt)

# create mask and generate pseudo absences

mask_r <- datalayers 
mask_r[!is.na(mask_r)] <- 1 

# generate absences
absence_cand=spsample(surveyplot_filt,n=10000,"random")
absence_cand.df=as.data.frame(absence_cand)
absence_cand.df$occurrence <- 0

absence_cand.df$X_obs=absence_cand.df$x
absence_cand.df$Y_obs=absence_cand.df$y
coordinates(absence_cand.df)=~X_obs+Y_obs
proj4string(absence_cand.df)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

absence_cand_add1=raster::extract(mask_r,absence_cand.df)
absence_cand.df@data$wetland_mask <- absence_cand_add1

absence=absence_cand.df@data[absence_cand.df@data$wetland_mask==1,]
absence=absence[complete.cases(absence), ]

absence$X_obs=absence$x
absence$Y_obs=absence$y
coordinates(absence)=~X_obs+Y_obs
proj4string(absence)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# create pres-abs file for SDM

absence.df=as.data.frame(absence)
presence.df=as.data.frame(bird_ahn3ac_filt2)

absence.dfsel=subset(absence.df,select=c(1,2,3))
presence.dfsel=subset(presence.df,select=c(6,7,10))
names(presence.dfsel)<- names(absence.dfsel)

presence.dfsel=presence.dfsel[presence.dfsel$occurrence<2,]

presabs=rbind(absence.dfsel,presence.dfsel)

table(presabs$occurrence)

presabs$X_obs=presabs$x
presabs$Y_obs=presabs$y
coordinates(presabs)=~X_obs+Y_obs
proj4string(presabs)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Export
raster::shapefile(presabs,"presabs_GrW_rand",overwrite=TRUE)
#raster::shapefile(presabs,"presabs_SW_rand",overwrite=TRUE)