library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

feaimp_GrW=read.csv("GrW_feaimp_test.csv")
accuracy_GrW=read.csv("GrW_acc_test.csv")

feaimp_Sn=read.csv("Sn_feaimp_test.csv")
accuracy_Sn=read.csv("Sn_acc_test.csv")

# Feature importance

grouped_GrW <- group_by(feaimp_GrW, variables)
feaimp_GrW_dfvis<-summarise(grouped_GrW, mean=mean(AUCtest), max=max(AUCtest),min=min(AUCtest))

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

a=ggplot(feaimp_GrW_dfvis, aes(x=variables, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
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

fig2b=grid.arrange(
  c,d,legend2,
  ncol=3,
  nrow=1,
  widths = c(1,1,0.3)
)