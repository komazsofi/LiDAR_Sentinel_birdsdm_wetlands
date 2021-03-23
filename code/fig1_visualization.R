library(tidyverse)
library(rgee)
library(sf)
library(reshape2)
library(lubridate)
library(geojsonio)

ee_Initialize()

GrW <- ee$FeatureCollection("users/komazsofi/GrW_2017_wgs84_v2")
sf_GrW <- ee_as_sf(x = GrW)
GrW_points = GrW$geometry()

SW <- ee$FeatureCollection("users/komazsofi/Sn_2017_wgs84_sel")
sf_SW <- ee_as_sf(x = SW)
SW_points = SW$geometry()

Bgr <- ee$FeatureCollection("users/komazsofi/Bgr_wgs84_sel")
sf_Bgr <- ee_as_sf(x = Bgr)
Bgr_points = Bgr$geometry()

s1_GrW <- ee$ImageCollection('COPERNICUS/S1_GRD')$
  filterDate('2017-01-01', '2017-12-31')$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$
  filter(ee$Filter$eq('orbitProperties_pass', 'DESCENDING'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filter(ee$Filter$eq('resolution_meters', 10))$
  filterBounds(GrW_points)$
  map(function(x){
    date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
    x$select("VV")
  })

s1_SW <- ee$ImageCollection('COPERNICUS/S1_GRD')$
  filterDate('2017-01-01', '2017-12-31')$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$
  filter(ee$Filter$eq('orbitProperties_pass', 'DESCENDING'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filter(ee$Filter$eq('resolution_meters', 10))$
  filterBounds(SW_points)$
  map(function(x){
    date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
    x$select("VV")
  })

s1_Bgr <- ee$ImageCollection('COPERNICUS/S1_GRD')$
  filterDate('2017-01-01', '2017-12-31')$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$
  filter(ee$Filter$eq('orbitProperties_pass', 'DESCENDING'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filter(ee$Filter$eq('resolution_meters', 10))$
  filterBounds(Bgr_points)$
  map(function(x){
    date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
    x$select("VV")
  })

s1_VV_GrW <- ee_extract(
  x = s1_GrW,
  y = sf_GrW,
  scale = 10,
  fun = ee$Reducer$median(),
  sf = FALSE
)

s1_VV_SW<- ee_extract(
  x = s1_SW,
  y = sf_SW,
  scale = 10,
  fun = ee$Reducer$median(),
  sf = FALSE
)

s1_VV_Bgr<- ee_extract(
  x = s1_Bgr,
  y = sf_Bgr,
  scale = 10,
  fun = ee$Reducer$median(),
  sf = FALSE
)

#export
write.csv(s1_VV_GrW,"s1_VV_GrW_2017.csv")
write.csv(s1_VV_SW,"s1_VV_SW_2017.csv")
write.csv(s1_VV_Bgr,"s1_VV_Bgr_2017.csv")

# visualization

s1_VV_GrW=read.csv("s1_VV_GrW_2017.csv")
s1_VV_SW=read.csv("s1_VV_SW_2017.csv")
s1_VV_Bgr=read.csv("s1_VV_Bgr_2017.csv")

organize_data<- function(s1_VV_GrW) {
  s1_VV_info=colnames(s1_VV_GrW) 
  colnames(s1_VV_GrW) <- substring(colnames(s1_VV_GrW), 14, 21)
  colnames(s1_VV_GrW)[1] <- "X"
  colnames(s1_VV_GrW)[2] <- "id"
  
  s1_VV_m_GrW=s1_VV_GrW %>%
    rownames_to_column 
  
  s1_VV_molt_GrW=melt(s1_VV_m_GrW,id.vars=c("id","X","rowname"))
  s1_VV_molt_GrW$variable=gsub("\\..*","",s1_VV_molt_GrW$variable)
  s1_VV_molt_GrW$variable2=as.Date(s1_VV_molt_GrW$variable,"%Y%m%d")
  
  s1_VV_melted_na_GrW <- na.omit(s1_VV_molt_GrW)
  s1_VV_melted_na_GrW=s1_VV_melted_na_GrW[s1_VV_melted_na_GrW$value>-25,]
  
  s1_VV_melted_m_GrW <- s1_VV_melted_na_GrW %>%
    mutate(month = month(variable2))
  
  s1_VV_melted_m_med_GrW <- s1_VV_melted_m_GrW %>%
    group_by(month) %>%
    summarise(med_VV_month = median(value))
  
  return(s1_VV_melted_m_med_GrW)
  
}

s1_VV_melted_m_med_GrW=organize_data(s1_VV_GrW)
s1_VV_melted_m_med_SW=organize_data(s1_VV_SW)
s1_VV_melted_m_med_Bgr=organize_data(s1_VV_Bgr[-3])

s1_VV_melted_m_med_GrW$class<-"GrW"
s1_VV_melted_m_med_SW$class<-"SW"
s1_VV_melted_m_med_Bgr$class<-"Bgr"

merged_db=rbind(s1_VV_melted_m_med_GrW,s1_VV_melted_m_med_Bgr)

ggplot(merged_db, aes(x = month, y = med_VV_month,colour=class))+geom_point()+geom_line(size=3)+
  theme_bw(base_size = 25)+xlab("Months in a year")+ylab("Backscatter coefficient VV [dB]")+scale_x_discrete(limits=month.abb)+
  scale_colour_manual(values = c("GrW" = "brown","Bgr" = "black"),name="Observation",labels=c("Absence","Presence"))
