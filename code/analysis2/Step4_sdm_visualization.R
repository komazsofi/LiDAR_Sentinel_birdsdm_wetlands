library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)
library(sdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

feaimp_GrW=read.csv("GrW_feaimp_test.csv")
accuracy_GrW=read.csv("GrW_acc_test.csv")
m_merged_GrW=read.sdm("m_merged_GrW.sdm")

feaimp_Sn=read.csv("Sn_feaimp_test.csv")
accuracy_Sn=read.csv("Sn_acc_test.csv")

# Feature importance

vi <- getVarImp(m_merged_GrW, method=c('glm','maxent','rf'))
feaimp_GrW_dfvis=vi@varImportanceMean[["AUCtest"]]

feaimp_GrW_dfvis$color<-0
feaimp_GrW_dfvis$color[1:3]<-3
feaimp_GrW_dfvis$color[4:10]<-2
feaimp_GrW_dfvis$color[11:17]<-1

grouped_Sn <- group_by(feaimp_Sn, variables)
feaimp_Sn_dfvis<-summarise(grouped_Sn, mean=mean(AUCtest), max=max(AUCtest),min=min(AUCtest))

feaimp_Sn_dfvis$color<-0
feaimp_Sn_dfvis$color[1:3]<-3
feaimp_Sn_dfvis$color[4:10]<-2
feaimp_Sn_dfvis$color[11:17]<-1

a=ggplot(feaimp_GrW_dfvis, aes(x=variables, y=AUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "goldenrod4", "2" = "orange", "3" = "deeppink"),name="Metrics type",labels=c("Sentinel","LiDAR","Landcover"))+ggtitle("a. Great reed warbler")

b=ggplot(feaimp_Sn_dfvis, aes(x=variables, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "goldenrod4", "2" = "orange", "3" = "deeppink"),name="Metrics type",labels=c("Sentinel","LiDAR","Landcover"))+ggtitle("b. Savi's warbler")

p0=ggplot(feaimp_GrW_dfvis, aes(x=variables, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = TRUE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "goldenrod4", "2" = "orange", "3" = "deeppink"),name="Metrics type",labels=c("Sentinel","LiDAR","Landcover"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

fig2b=grid.arrange(
  a,b,legend,
  ncol=3,
  nrow=1,
  widths = c(1,1,0.3)
)

# Accuracy
accuracy_GrW2=accuracy_GrW
accuracy_GrW2$modeltype<-"ensemble"

accuracy_GrW3=rbind(accuracy_GrW,accuracy_GrW2)

accuracy_GrW3$RStype <- factor(accuracy_GrW3$RStype , levels=c("lidar", "sentinel", "landc", "lidsent","lidall"))
accuracy_GrW3$modeltype <- factor(accuracy_GrW3$modeltype , levels=c("glm","maxent","rf","ensemble"))

accuracy_Sn2=accuracy_Sn
accuracy_Sn2$modeltype<-"ensemble"

accuracy_Sn3=rbind(accuracy_Sn,accuracy_Sn2)

accuracy_Sn3$RStype <- factor(accuracy_Sn3$RStype , levels=c("lidar", "sentinel", "landc", "lidsent","lidall"))
accuracy_Sn3$modeltype <- factor(accuracy_Sn3$modeltype , levels=c("glm","maxent","rf","ensemble"))

c=ggplot(accuracy_GrW3, aes(x=RStype, y=AUC,fill=modeltype)) + geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+ylab("AUC")+xlab("Remote Sensing products")+ggtitle("a. Great reed wrabler")
d=ggplot(accuracy_Sn3, aes(x=RStype, y=AUC,fill=modeltype)) + geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+ylab("AUC")+xlab("Remote Sensing products")+ggtitle("b. Savi's wrabler")

p1=ggplot(accuracy_Sn3, aes(x=RStype, y=AUC,fill=modeltype)) + geom_boxplot()+theme_bw(base_size = 20)+ylab("AUC")+xlab("Remote Sensing products")+ggtitle("b. Savi's wrabler")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend2 <- get_legend(p1)

fig1b=grid.arrange(
  c,d,legend2,
  ncol=3,
  nrow=1,
  widths = c(1,1,0.3)
)

# report accuracy table

acc_grouped_GrW <- group_by(accuracy_GrW3, RStype,modeltype)
acc_GrW_table<-summarise(acc_grouped_GrW, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                         meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

acc_grouped_Sn<- group_by(accuracy_Sn3, RStype,modeltype)
acc_Sn_table<-summarise(acc_grouped_Sn, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                         meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

# response curves

resp_GrW_sel=getResponseCurve(m_merged_GrW)

sel_fea1_GrW1=resp_GrW_sel@response[["optical_NDVIsd_hor_100m"]]
sel_fea1_GrW1$meanresp=rowMeans(sel_fea1_GrW1[,c(2:301)])
sel_fea1_GrW1$sdresp=apply(subset(sel_fea1_GrW1, select = 2:301), 1, sd, na.rm=TRUE)

sel_fea1_GrW2=resp_GrW_sel@response[["lidar_VD_2_3"]]
sel_fea1_GrW2$meanresp=rowMeans(sel_fea1_GrW2[,c(2:301)])
sel_fea1_GrW2$sdresp=apply(subset(sel_fea1_GrW2, select = 2:301), 1, sd, na.rm=TRUE)

sel_fea1_GrW3=resp_GrW_sel@response[["lidar_HH_reedveg_prop"]]
sel_fea1_GrW3$meanresp=rowMeans(sel_fea1_GrW3[,c(2:301)])
sel_fea1_GrW3$sdresp=apply(subset(sel_fea1_GrW3, select = 2:301), 1, sd, na.rm=TRUE)

sel_fea1_GrW4=resp_GrW_sel@response[["lidar_C_ppr"]]
sel_fea1_GrW4$meanresp=rowMeans(sel_fea1_GrW4[,c(2:301)])
sel_fea1_GrW4$sdresp=apply(subset(sel_fea1_GrW4, select = 2:301), 1, sd, na.rm=TRUE)

sel_fea1_GrW5=resp_GrW_sel@response[["landcover_propswamp"]]
sel_fea1_GrW5$meanresp=rowMeans(sel_fea1_GrW5[,c(2:301)])
sel_fea1_GrW5$sdresp=apply(subset(sel_fea1_GrW5, select = 2:301), 1, sd, na.rm=TRUE)

a=ggplot(sel_fea1_GrW1,aes(x=optical_NDVIsd_hor_100m,y=meanresp))+geom_line(color="goldenrod4",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="goldenrod4", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("optical_NDVIsd_hor_100m")+ylim(0,1)+ggtitle("a.")
b=ggplot(sel_fea1_GrW2,aes(x=lidar_VD_2_3,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_VD_2_3")+ylim(0,1)+ggtitle("b.")
c=ggplot(sel_fea1_GrW3,aes(x=lidar_HH_reedveg_prop,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_HH_reedveg_prop")+ylim(0,1)+ggtitle("c.")
d=ggplot(sel_fea1_GrW4,aes(x=lidar_C_ppr,y=meanresp))+geom_line(color="orange",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="orange", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_C_ppr")+ylim(0,1)+ggtitle("d.")
e=ggplot(sel_fea1_GrW5,aes(x=landcover_propswamp,y=meanresp))+geom_line(color="deeppink",size=2)+
  geom_ribbon(aes(y = meanresp, ymin = meanresp - sdresp/2, ymax = meanresp + sdresp/2),color="deeppink", alpha = .2)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("landcover_propswamp")+ylim(0,1)+ggtitle("e.")

fig3b=grid.arrange(
  a,b,c,d,e,
  ncol=5,
  nrow=1,
  widths = c(0.5,0.5,0.5,0.5,0.5)
)