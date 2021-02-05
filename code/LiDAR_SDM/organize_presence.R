library(rgdal)
library(raster)
library(plotKML)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presence/"
setwd(workingdirectory)

birdsfile="Reedland_bird_observations.shp"
ahn3_acqfile="ahn3_measuretime.shp"
landcoverfile="LGN7.tif"

# Import
birds_sp = readOGR(dsn=birdsfile)
ahn3_acq_sp = readOGR(dsn=ahn3_acqfile)
landcover=raster(landcoverfile)

# Filter presence according to the needs

birdsel_sp=birds_sp[(birds_sp@data$species=="Grote Karekiet" & birds_sp@data$year==2016),]
bird_ahn3ac=raster::intersect(birdsel_sp,ahn3_acq_sp)

#bird_ahn3ac_filt=bird_ahn3ac[(bird_ahn3ac@data$year==bird_ahn3ac@data$Jaar),]
bird_ahn3ac_filt=bird_ahn3ac[(bird_ahn3ac@data$OBJECTID==5),]

ahn3_acq_sp_filt=ahn3_acq_sp[(ahn3_acq_sp@data$OBJECTID==5),]

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

presrast <- vect2rast(bird_ahn3ac_filt, fname = "number",cell.size=500,file.name="pres_rast500m.tif")

# Export
raster::shapefile(bird_ahn3ac_filt,"bird_ahn3ac_filt2016",overwrite=TRUE)
raster::shapefile(ahn3_acq_sp_filt,"studyarea",overwrite=TRUE)

writeRaster(landcover_crop,'landcover_crop.tif',overwrite=TRUE)
writeRaster(formask,"wetland_mask.tif",overwrite=TRUE)
