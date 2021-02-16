library(rgdal)
library(raster)
library(sdm)
library(ggplot2)
library(snow)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/radar/merged/"
setwd(workingdirectory)

birdsfile="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/presabs_800rand_studyarea.shp"

# Import

filelist=list.files(pattern = "*.tif")
radar=stack(filelist)

birds = readOGR(dsn=birdsfile)

# intersect and analyze
data_forsdm <- sdmData(formula=occurrence~., train=birds[,-c(1,2)], predictors=radar)

data=data_forsdm@features
data$occ <- 0
data$occ[355:433]<-1

data$diffVVQ2Q4=data$radar_metricVV_2-data$radar_metricVV_4
data$diffVVQ2VV=data$radar_metricVV_2-data$radar_metricVV

data$ratVVVHQ2=data$radar_metricVV_2/data$radar_metricVH_2
data$ratVVVH=data$radar_metricVV/data$radar_metricVH

data$diffVVVHQ2=data$radar_metricVV_2-data$radar_metricVH_2
data$diffVVVH=data$radar_metricVV-data$radar_metricVH

ggplot(data, aes(x=occ, y=radar_metricVV_max,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVH_max,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVV_stdDev,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVH_stdDev,group=occ,fill=as.factor(occ)))+geom_boxplot()

ggplot(data, aes(x=occ, y=radar_metricVV_1,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVV_2,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVV_3,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVV_4,group=occ,fill=as.factor(occ)))+geom_boxplot()

ggplot(data, aes(x=occ, y=radar_metricVH_1,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVH_2,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVH_3,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=radar_metricVH_4,group=occ,fill=as.factor(occ)))+geom_boxplot()

ggplot(data, aes(x=occ, y=diffVVQ2Q4,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=diffVVQ2VV,group=occ,fill=as.factor(occ)))+geom_boxplot()

ggplot(data, aes(x=occ, y=ratVVVHQ2,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=ratVVVH,group=occ,fill=as.factor(occ)))+geom_boxplot()

ggplot(data, aes(x=occ, y=diffVVVHQ2,group=occ,fill=as.factor(occ)))+geom_boxplot()
ggplot(data, aes(x=occ, y=diffVVVH,group=occ,fill=as.factor(occ)))+geom_boxplot()

# calc VV/VH

radar_metricVVVH=(radar[[9]])-(radar[[1]])

radar_metricVVVHQ1=radar[[10]]-radar[[2]]
radar_metricVVVHQ2=radar[[11]]-radar[[3]]
radar_metricVVVHQ3=radar[[12]]-radar[[4]]
radar_metricVVVHQ4=radar[[13]]-radar[[5]]

writeRaster(radar_metricVVVH,'radar_metricVVVH.tif',overwrite=TRUE)
writeRaster(radar_metricVVVHQ1,'radar_metricVVVHQ1.tif',overwrite=TRUE)
writeRaster(radar_metricVVVHQ2,'radar_metricVVVHQ2.tif',overwrite=TRUE)
writeRaster(radar_metricVVVHQ3,'radar_metricVVVHQ3.tif',overwrite=TRUE)
writeRaster(radar_metricVVVHQ4,'radar_metricVVVHQ4.tif',overwrite=TRUE)

# calc. seasonal changes

radar_metricVVQ2Q4=radar[[11]]-radar[[13]]
radar_metricVHQ2Q4=radar[[3]]-radar[[5]]

writeRaster(radar_metricVVQ2Q4,'radar_metricVVQ2Q4.tif',overwrite=TRUE)
writeRaster(radar_metricVHQ2Q4,'radar_metricVHQ2Q4.tif',overwrite=TRUE)

# calc. horizontal metrics

water_class=reclassify(radar[[3]], c(c(-Inf,-25,1,-25,Inf,0)))
water_class2=reclassify(radar[[5]], c(c(-Inf,-25,1,-25,Inf,0)))

beginCluster(15)

sd_VVQ2=clusterR(radar[[11]], focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
sd_VHQ2=clusterR(radar[[3]], focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
prop_water_spring=clusterR(water_class, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))
prop_water_winter=clusterR(water_class2, focal, args=list(w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE))

endCluster()

writeRaster(sd_VVQ2,'radar_metricsd_VVQ2_hor.tif',overwrite=TRUE)
writeRaster(sd_VHQ2,'radar_metricsd_VHQ2_hor.tif',overwrite=TRUE)
writeRaster(prop_water_spring,'radar_metricprop_water_spring.tif',overwrite=TRUE)
writeRaster(prop_water_winter,'radar_metricprop_water_winter.tif',overwrite=TRUE)

prop_water_change=prop_water_spring-prop_water_winter

writeRaster(prop_water_change,'radar_metricprop_water_change.tif',overwrite=TRUE)