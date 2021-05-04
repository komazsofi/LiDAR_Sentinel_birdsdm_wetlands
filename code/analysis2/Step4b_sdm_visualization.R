library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)
library(sdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April_results/"
setwd(workingdirectory)

# Import
m_merged_GrW=read.sdm("merged_GrW_all.sdm")
m_merged_Sn=read.sdm("merged_Sn_all.sdm")

# Feature importance

#vi <- getVarImp(m_merged_GrW, method=c('glm','maxent','rf'))
vi <- getVarImp(m_merged_GrW, method=c('maxent'))
#vi <- getVarImp(m_merged_GrW, method=c('rf'))
feaimp_GrW_dfvis=vi@varImportanceMean[["AUCtest"]]

feaimp_GrW_dfvis$variables[17]<-"optical_NDVIsd_hor"
feaimp_GrW_dfvis$variables[20]<-"radar_VHsd_hor"
feaimp_GrW_dfvis$variables[22]<-"radar_VVsd_hor"

feaimp_GrW_dfvis$color<-0
feaimp_GrW_dfvis$color[1:6]<-3
feaimp_GrW_dfvis$color[7:15]<-1
feaimp_GrW_dfvis$color[16:23]<-2

#vi2 <- getVarImp(m_merged_Sn, method=c('glm','maxent','rf'))
vi2 <- getVarImp(m_merged_Sn, method=c('maxent'))
#vi2 <- getVarImp(m_merged_Sn, method=c('rf'))
feaimp_Sn_dfvis=vi2@varImportanceMean[["AUCtest"]]

feaimp_Sn_dfvis$variables[17]<-"optical_NDVIsd_hor"
feaimp_Sn_dfvis$variables[20]<-"radar_VHsd_hor"
feaimp_Sn_dfvis$variables[22]<-"radar_VVsd_hor"

feaimp_Sn_dfvis$color<-0
feaimp_Sn_dfvis$color[1:6]<-3
feaimp_Sn_dfvis$color[7:15]<-1
feaimp_Sn_dfvis$color[16:23]<-2

a1=ggplot(feaimp_GrW_dfvis, aes(x=reorder(variables,-color), y=AUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ylim(-0.01, 0.3)+
  scale_fill_manual(values = c("1" = "orange", "2" = "goldenrod4", "3" = "deeppink"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover"))+ggtitle("a. Great reed warbler")

b1=ggplot(feaimp_Sn_dfvis, aes(x=reorder(variables,-color), y=AUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ylim(0, 0.3)+
  scale_fill_manual(values = c("1" = "orange", "2" = "goldenrod4", "3" = "deeppink"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover"))+ggtitle("b. Savi's warbler")

p0=ggplot(feaimp_Sn_dfvis, aes(x=reorder(variables,-color), y=AUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = TRUE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "orange", "2" = "goldenrod4", "3" = "deeppink"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover"))+ggtitle("a. Great reed warbler")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

fig2b=grid.arrange(
  a1,b1,legend,
  ncol=3,
  nrow=1,
  widths = c(1,1,0.3)
)

# response curves

resp_GrW_sel=getResponseCurve(m_merged_GrW,id = c(21:40,81:100,141:160,201:220,261:280))
#resp_GrW_sel=getResponseCurve(m_merged_GrW,id = c(41:60,101:120,161:180,221:240,281:300))

sel_fea1_GrW1=resp_GrW_sel@response[["lidar_C_ppr"]]
sel_fea1_GrW1$meanresp=rowMeans(sel_fea1_GrW1[,c(2:101)])
sel_fea1_GrW1$sdresp=apply(subset(sel_fea1_GrW1, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_GrW2=resp_GrW_sel@response[["optical_NDVIsd_hor_50m"]]
sel_fea1_GrW2$meanresp=rowMeans(sel_fea1_GrW2[,c(2:101)])
sel_fea1_GrW2$sdresp=apply(subset(sel_fea1_GrW2, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_GrW3=resp_GrW_sel@response[["landcover_propswamp"]]
sel_fea1_GrW3$meanresp=rowMeans(sel_fea1_GrW3[,c(2:101)])
sel_fea1_GrW3$sdresp=apply(subset(sel_fea1_GrW3, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_GrW4=resp_GrW_sel@response[["landcover_propforest"]]
sel_fea1_GrW4$meanresp=rowMeans(sel_fea1_GrW4[,c(2:101)])
sel_fea1_GrW4$sdresp=apply(subset(sel_fea1_GrW4, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_GrW5=resp_GrW_sel@response[["lidar_HH_reedveg_prop"]]
sel_fea1_GrW5$meanresp=rowMeans(sel_fea1_GrW5[,c(2:101)])
sel_fea1_GrW5$sdresp=apply(subset(sel_fea1_GrW5, select = 2:101), 1, sd, na.rm=TRUE)

resp_Sn_sel=getResponseCurve(m_merged_Sn,id = c(21:40,81:100,141:160,201:220,261:280))
#resp_Sn_sel=getResponseCurve(m_merged_Sn,id = c(41:60,101:120,161:180,221:240,281:300))

sel_fea1_Sn1=resp_Sn_sel@response[["lidar_HH_reedveg_prop"]]
sel_fea1_Sn1$meanresp=rowMeans(sel_fea1_Sn1[,c(2:101)])
sel_fea1_Sn1$sdresp=apply(subset(sel_fea1_Sn1, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_Sn2=resp_Sn_sel@response[["landcover_propswamp"]]
sel_fea1_Sn2$meanresp=rowMeans(sel_fea1_GrW2[,c(2:101)])
sel_fea1_Sn2$sdresp=apply(subset(sel_fea1_Sn2, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_Sn3=resp_Sn_sel@response[["lidar_C_ppr"]]
sel_fea1_Sn3$meanresp=rowMeans(sel_fea1_Sn3[,c(2:101)])
sel_fea1_Sn3$sdresp=apply(subset(sel_fea1_Sn3, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_Sn4=resp_Sn_sel@response[["optical_NDVImed"]]
sel_fea1_Sn4$meanresp=rowMeans(sel_fea1_Sn4[,c(2:101)])
sel_fea1_Sn4$sdresp=apply(subset(sel_fea1_Sn4, select = 2:101), 1, sd, na.rm=TRUE)

sel_fea1_Sn5=resp_Sn_sel@response[["lidar_HH_sd"]]
sel_fea1_Sn5$meanresp=rowMeans(sel_fea1_Sn5[,c(2:101)])
sel_fea1_Sn5$sdresp=apply(subset(sel_fea1_Sn5, select = 2:101), 1, sd, na.rm=TRUE)

a=ggplot(sel_fea1_GrW1,aes(x=lidar_C_ppr,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_C_ppr")+ylim(0,1)+ggtitle("c.")
b=ggplot(sel_fea1_GrW2,aes(x=optical_NDVIsd_hor_50m,y=meanresp))+geom_line(color="goldenrod4",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="goldenrod4", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("loptical_NDVIsd_hor")+ylim(0,1)+ggtitle("e.")
c=ggplot(sel_fea1_GrW3,aes(x=landcover_propswamp,y=meanresp))+geom_line(color="deeppink",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="deeppink", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("landcover_propswamp")+ylim(0,1)+ggtitle("d.")
d=ggplot(sel_fea1_GrW4,aes(x=landcover_propforest,y=meanresp))+geom_line(color="deeppink",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="deeppink", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("landcover_propforest")+ylim(0,1)+ggtitle("f.")
e=ggplot(sel_fea1_GrW5,aes(x=lidar_HH_reedveg_prop,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_HH_reedveg_prop")+ylim(0,1)+ggtitle("g.")

f=ggplot(sel_fea1_Sn1,aes(x=lidar_HH_reedveg_prop,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_HH_reedveg_prop")+ylim(0,1)+ggtitle("h.")
g=ggplot(sel_fea1_Sn2,aes(x=landcover_propswamp,y=meanresp))+geom_line(color="deeppink",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="deeppink", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("landcover_propswamp")+ylim(0,1)+ggtitle("k.")
h=ggplot(sel_fea1_Sn3,aes(x=lidar_C_ppr,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_C_ppr")+ylim(0,1)+ggtitle("j.")
j=ggplot(sel_fea1_Sn4,aes(x=optical_NDVImed,y=meanresp))+geom_line(color="goldenrod4",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="goldenrod4", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("optical_NDVImed")+ylim(0,1)+ggtitle("l.")
k=ggplot(sel_fea1_Sn5,aes(x=lidar_HH_sd,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("optical_NDVImed")+ylim(0,1)+ggtitle("i.")

t1 <- textGrob("Savis's warbler",gp=gpar(fontsize=16, col="black", fontface="bold"))
t3 <- textGrob("Great reed warbler",gp=gpar(fontsize=16, col="black", fontface="bold"))

p0=ggplot(sel_fea1_Sn5,aes(x=lidar_HH_sd,y=meanresp))+geom_line(aes(color="goldenrod4"),size=2,show.legend=TRUE)+
  geom_line(aes(color="orange"),size=2)+geom_line(aes(color="deeppink"),size=2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_VV_p25")+ylim(0,1)+ggtitle("j.")+
  scale_color_identity(name = "Metrics type",breaks = c("goldenrod4", "orange", "deeppink"),labels = c("Sentinel", "LiDAR", "Land cover"),guide = "legend")+theme(legend.position="bottom")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

fig3b=grid.arrange(
  a,b,c,d,e,
  f,g,h,j,k,
  legend,
  ncol=6,
  nrow=3,
  layout_matrix=rbind(c(NA,1,2,3,4,5),c(NA,6,7,8,9,10), c(NA,11,11,11,11,11)),
  widths = c(0.1,0.1,0.1,0.1,0.1,0.1),
  heights = c(1,1,0.3)
)

fig3=grid.arrange(
  fig2b,fig3b,
  ncol=1,
  nrow=2
)

ggsave("fig3_maxent.png",plot = fig3,width = 18, height =16)
#ggsave("fig3_rf.png",plot = fig3,width = 18, height =16)