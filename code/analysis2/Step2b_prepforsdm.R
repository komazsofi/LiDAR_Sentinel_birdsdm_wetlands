library(dismo)
library(raster)
library(rgdal)
library(ROSE)


workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

filelist=list.files(pattern = "*.tif")
all_predictor=stack(filelist)

GrW=readOGR(dsn="presabs_GrW_rand.shp")
Sn=readOGR(dsn="presabs_SW_rand.shp")

# Intersect
mydata_GrW <- raster::extract(all_predictor, GrW, df = TRUE)
mydata_Sn <- raster::extract(all_predictor, Sn, df = TRUE)

mydata_GrW$occurrence <- GrW$occurrence
mydata_GrW$x <- GrW$x
mydata_GrW$y <- GrW$y

mydata_Sn$occurrence <- Sn$occurrence
mydata_Sn$x <- Sn$x
mydata_Sn$y <- Sn$y

mydata_GrW_clean=mydata_GrW[complete.cases(mydata_GrW), ]
mydata_GrW_clean <- mydata_GrW_clean[,-1]

mydata_Sn_clean=mydata_Sn[complete.cases(mydata_Sn), ]
mydata_Sn_clean <- mydata_Sn_clean[,-1]

table(mydata_GrW_clean$occurrence)
table(mydata_Sn_clean$occurrence)

# handle imbalanced GrW
mydata_GrW_clean_sampl <- ovun.sample(occurrence ~ ., data = mydata_GrW_clean, method = "both", p=0.5, seed = 1)$data
table(mydata_GrW_clean_sampl$occurrence)

# export
write.csv(mydata_GrW_clean_sampl,"presabs_GrW2.csv")
write.csv(mydata_Sn_clean,"presabs_Sn2.csv")