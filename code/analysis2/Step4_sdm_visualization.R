library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

feaimp_GrW=read.csv("GrW_feaimp_test.csv")
accuracy_GrW=read.csv("GrW_acc_test.csv")

# Feature importance

grouped_GrW <- group_by(feaimp_GrW, variables)
feaimp_GrW_dfvis<-summarise(grouped_GrW, mean=mean(AUCtest), max=max(AUCtest),min=min(AUCtest))

feaimp_GrW_dfvis$color<-0
feaimp_GrW_dfvis$color[1:3]<-3
feaimp_GrW_dfvis$color[4:10]<-2
feaimp_GrW_dfvis$color[11:17]<-1

a=ggplot(feaimp_GrW_dfvis, aes(x=variables, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "goldenrod4", "2" = "orange", "3" = "deeppink"),name="Metrics type",labels=c("Sentinel","LiDAR","Landcover"))+ylim(0,0.3)+ggtitle("a. Great reed warbler")

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
  a,a,legend,
  ncol=3,
  nrow=1,
  widths = c(1,1,0.3)
)

# Accuracy

accuracy_GrW$RStype <- factor(accuracy_GrW$RStype , levels=c("lidar", "sentinel", "landc", "lidsent","lidall"))

ggplot(accuracy_GrW, aes(x=RStype, y=AUC)) + geom_boxplot()+theme_bw(base_size = 20)+ylab("AUC")+xlab("Remote Sensing products")+ggtitle("a. Great reed wrabler")