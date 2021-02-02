library(rgdal)
library(raster)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presence/"
setwd(workingdirectory)

birdsfile="Reedland_bird_observations.shp"
ahn3_acqfile="ahn3_measuretime.shp"

# Import
birds_sp = readOGR(dsn=birdsfile)
ahn3_acq_sp = readOGR(dsn=ahn3_acqfile)

# Filter presence according to the needs

birdsel_sp=birds_sp[(birds_sp@data$species=="Grote Karekiet" & birds_sp@data$year>2014),]
bird_ahn3ac=raster::intersect(birdsel_sp,ahn3_acq_sp)

bird_ahn3ac_filt=bird_ahn3ac[(bird_ahn3ac@data$year==bird_ahn3ac@data$Jaar),]

# Export
raster::shapefile(bird_ahn3ac_filt,"bird_ahn3ac_filt",overwrite=TRUE)
