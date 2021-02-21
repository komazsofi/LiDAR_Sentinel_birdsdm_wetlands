library(sdm)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(ggpubr)

workingdir="D:/Sync/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper/both/"
setwd(workingdir)

RFmean_GrW_LiDAR=read.sdm("RFmean_GrW_LiDAR_small_boot_n5.sdm")
RFmean_Sn_LiDAR=read.sdm("RFmean_Sn_LiDAR_small_boot_n5.sdm")

RFmean_GrW_Sentinel=read.sdm("RFmean_GrW_Sentinel_small_boot_n5.sdm")
RFmean_Sn_Sentinel=read.sdm("RFmean_Sn_Sentinel_small_boot_n5.sdm")

RFmean_GrW=read.sdm("RFmean_GrW_Both_small_boot_n5.sdm")
RFmean_Sn=read.sdm("RFmean_Sn_Both_small_boot_n5.sdm")

########## Feature importance
# Combined Feature Importance LiDAR

GrW_feaimp=getVarImp(RFmean_GrW_LiDAR)
Sn_feaimp=getVarImp(RFmean_Sn_LiDAR)

GrW_feaimp=GrW_feaimp@varImportanceMean[["corTest"]]
Sn_feaimp=Sn_feaimp@varImportanceMean[["corTest"]]

GrW_feaimp$species<-"GrW"
Sn_feaimp$species<-"SW"

feaimp=rbind(GrW_feaimp,Sn_feaimp)

ggplot(feaimp, aes(x=variables, y=corTest, fill=species)) + geom_bar(stat="identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+coord_flip()+scale_fill_manual(values = c("goldenrod4", "deeppink"))+
  theme_bw(base_size = 20)+xlab("Feature Importance")+ylab("Metrics")

# Combined Feature Importance Sentinel

GrW_feaimp2=getVarImp(RFmean_GrW_Sentinel)
Sn_feaimp2=getVarImp(RFmean_Sn_Sentinel)

GrW_feaimp2=GrW_feaimp2@varImportanceMean[["corTest"]]
Sn_feaimp2=Sn_feaimp2@varImportanceMean[["corTest"]]

GrW_feaimp2$species<-"GrW"
Sn_feaimp2$species<-"SW"

GrW_feaimp2$variables<-c("Prop_water","NDVI_sd_hor","VH_sd_hor","VH_sd_time","VV_max_time","VV_sd_time","NDVI_med_time","NDVI_sd_time")
Sn_feaimp2$variables<-c("Prop_water","NDVI_sd_hor","VH_sd_hor","VH_sd_time","VV_max_time","VV_sd_time","NDVI_med_time","NDVI_sd_time")

feaimp2=rbind(GrW_feaimp2,Sn_feaimp2)

ggplot(feaimp2, aes(x=variables, y=corTest, fill=species)) + geom_bar(stat="identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+coord_flip()+scale_fill_manual(values = c("goldenrod4", "deeppink"))+
  theme_bw(base_size = 20)+xlab("Feature Importance")+ylab("Metrics")

# Combined Feature Importance LiDAR

GrW_feaimp3=getVarImp(RFmean_GrW)
Sn_feaimp3=getVarImp(RFmean_Sn)

GrW_feaimp3=GrW_feaimp3@varImportanceMean[["corTest"]]
Sn_feaimp3=Sn_feaimp3@varImportanceMean[["corTest"]]

GrW_feaimp3$variables<-c("L_HH_reedveg_prop","L_C_ppr","L_HH_sd_low","L_HH_sd","L_VD_1_2","L_VD_2_3","L_VV_p25","L_VV_std","S_Prop_water","S_NDVI_sd_hor","S_VH_sd_hor","S_VH_sd_time","S_VV_max_time","S_VV_sd_time","S_NDVI_med_time","S_NDVI_sd_time")
Sn_feaimp3$variables<-c("L_HH_reedveg_prop","L_C_ppr","L_HH_sd_low","L_HH_sd","L_VD_1_2","L_VD_2_3","L_VV_p25","L_VV_std","S_Prop_water","S_NDVI_sd_hor","S_VH_sd_hor","S_VH_sd_time","S_VV_max_time","S_VV_sd_time","S_NDVI_med_time","S_NDVI_sd_time")

GrW_feaimp3$species<-"GrW"
Sn_feaimp3$species<-"SW"

feaimp3=rbind(GrW_feaimp3,Sn_feaimp3)

ggplot(feaimp3, aes(x=variables, y=corTest, fill=species)) + geom_bar(stat="identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+coord_flip()+scale_fill_manual(values = c("goldenrod4", "deeppink"))+
  theme_bw(base_size = 20)+xlab("Feature Importance")+ylab("Metrics")

########## Correlation among metrics

data_GrW=RFmean_Sn@data@features
names(data_GrW)<-c("rID","L_HH_reedveg_prop","L_C_ppr","L_HH_sd_low","L_HH_sd","L_VD_1_2","L_VD_2_3","L_VV_p25","L_VV_std","S_Prop_water","S_NDVI_sd_hor","S_VH_sd_hor","S_VH_sd_time","S_VV_max_time","S_VV_sd_time","S_NDVI_med_time","S_NDVI_sd_time")

data_GrW$occ <- 0
data_GrW$occ[164:237]<-1

r_grw <- cor(data_GrW[2:17],method = "spearman")

ggcorrplot(r_grw,
           type = "lower",
           lab = TRUE)

ggscatter(data_GrW, x = "L_HH_reedveg_prop", y = "S_NDVI_sd_time", add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray",size=3),conf.int = TRUE) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = 3,label.y = 0.29,size=10) +
  geom_point(color="deeppink",size=2,show.legend = TRUE) + theme_bw(base_size = 20)

ggscatter(data_GrW, x = "L_VD_2_3", y = "S_NDVI_sd_time", add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray",size=3),conf.int = TRUE) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = 0,label.y = 0,size=10) +
  geom_point(color="deeppink",size=2,show.legend = TRUE) + theme_bw(base_size = 20)
