library(dismo)
library(raster)
library(rgdal)


workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

GrW=readOGR(dsn="presabs_GrW_rand_studyarea.shp")
Sn=readOGR(dsn="presabs_Sn_rand_studyarea.shp")
Ba=readOGR(dsn="presabs_Ba_rand_studyarea.shp")

# Intersect
mydata_GrW <- raster::extract(all_predictor, GrW, df = TRUE)
mydata_Sn <- raster::extract(all_predictor, Sn, df = TRUE)
mydata_Ba <- raster::extract(all_predictor, Ba, df = TRUE)

mydata_GrW$occurrence <- GrW$occurrence
mydata_GrW$x <- GrW$x
mydata_GrW$y <- GrW$y

mydata_Sn$occurrence <- Sn$occurrence
mydata_Sn$x <- Sn$x
mydata_Sn$y <- Sn$y

mydata_Ba$occurrence <- Ba$occurrence
mydata_Ba$x <- Ba$x
mydata_Ba$y <- Ba$y

mydata_GrW_clean=mydata_GrW[complete.cases(mydata_GrW), ]
mydata_GrW_clean <- mydata_GrW_clean[,-1]

mydata_Sn_clean=mydata_Sn[complete.cases(mydata_Sn), ]
mydata_Sn_clean <- mydata_Sn_clean[,-1]

mydata_Ba_clean=mydata_Ba[complete.cases(mydata_Ba), ]
mydata_Ba_clean <- mydata_Ba_clean[,-1]

# Export
write.csv(mydata_GrW_clean,"presabs_GrW.csv")
write.csv(mydata_Sn_clean,"presabs_Sn.csv")
write.csv(mydata_Ba_clean,"presabs_Ba.csv")

# !!!!needs to be manually adjusted!!!!

pres_GrW=mydata_GrW_clean[mydata_GrW_clean$occurrence==1,]
abs_GrW=mydata_GrW_clean[mydata_GrW_clean$occurrence==0,]

abs_GrW_sampled=abs_GrW[sample(nrow(abs_GrW), 200), ]
presabs_GrW=rbind(pres_GrW,abs_GrW_sampled)

pres_Sn=mydata_Sn_clean[mydata_Sn_clean$occurrence==1,]
abs_Sn=mydata_Sn_clean[mydata_Sn_clean$occurrence==0,]

pres_Sn_sampled=pres_Sn[sample(nrow(pres_Sn), 658), ]
presabs_Sn=rbind(pres_Sn_sampled,abs_Sn)

pres_Ba=mydata_Ba_clean[mydata_Ba_clean$occurrence==1,]
abs_Ba=mydata_Ba_clean[mydata_Ba_clean$occurrence==0,]

pres_Ba_sampled=pres_Ba[sample(nrow(pres_Ba), 661), ]
presabs_Ba=rbind(pres_Ba_sampled,abs_Ba)

# Export
write.csv(presabs_GrW,"presabs_GrW_eq.csv")
write.csv(presabs_Sn,"presabs_Sn_eq.csv")
write.csv(presabs_Ba,"presabs_Ba_eq.csv")