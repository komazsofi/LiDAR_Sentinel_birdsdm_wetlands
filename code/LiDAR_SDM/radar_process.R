library(rgdal)
library(raster)
library(gdalUtils)
library(dplyr)
library(stringr)

mosaicList <- function(rasList){
  
  #Internal function to make a list of raster objects from list of files.
  ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster::raster(grd_name)
    }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
  
  #convert every raster path to a raster object and create list of the results
  raster.list <-sapply(rasList, FUN = ListRasters)
  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  #####This function deals with overlapping areas
  raster.list$fun <- mean
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(raster::mosaic, raster.list)
  
  #set crs of output
  crs(mos) <- crs(x = raster(rasList[1]))
  return(mos)
}

workingdirectory="D:/Koma/Sync_PhD/GEE/"
setwd(workingdirectory)

filelist=list.files(pattern = "*.tif")
studyarea=readOGR(dsn="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/studyarea.shp")

# Import

#mosaic_radar=mosaicList(filelist)

for (i in filelist) {
print(i)
  
radarraster=stack(i)
writeRaster(radarraster, filename=paste("D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/radar2/",str_remove(i, ".tif"),"_",names(radarraster),sep=""), bylayer=TRUE,format="GTiff")

}

feanames=stack(filelist[1])
feanames=names(feanames)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/radar2/"
setwd(workingdirectory)

for (j in feanames) {
  print(j)
  
  files_permetric=list.files(pattern = paste("*",j,".tif",sep=""))
  metric=mosaicList(files_permetric)
  
  writeRaster(metric,paste("D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/radar2/merged/radar_metric",j,".tif",sep=""),overwrite=TRUE)
  
}


