library(raster)
library(mlr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(mmpf)
library(patchwork)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import

presabs_Sn=read.csv("presabs_Sn.csv")
presabs_Ba=read.csv("presabs_Ba.csv")
presabs_GrW=read.csv("presabs_GrW.csv")

presabs_Sn$occurrence<-factor(presabs_Sn$occurrence)
presabs_Sn=presabs_Sn[,-1]
presabs_Ba$occurrence<-factor(presabs_Ba$occurrence)
presabs_Ba=presabs_Ba[,-1]
presabs_GrW$occurrence<-factor(presabs_GrW$occurrence)
presabs_GrW=presabs_GrW[,-1]

# start mlr

set.seed(11)

taskall_Sn = makeClassifTask(data = presabs_Sn[,c(1:18,19)], target = "occurrence",
                          positive = "1", coordinates = presabs_Sn[,c(20,21)])

task.over_Sn = oversample(taskall_Sn, rate = 2)

taskall_Ba = makeClassifTask(data = presabs_Ba[,c(1:18,19)], target = "occurrence",
                             positive = "1", coordinates = presabs_Ba[,c(20,21)])

task.over_Ba = oversample(taskall_Ba, rate = 2.5)

taskall_GrW = makeClassifTask(data = presabs_GrW[,c(1:18,19)], target = "occurrence",
                             positive = "1", coordinates = presabs_GrW[,c(20,21)])

task.over_GrW = oversample(taskall_GrW, rate = 4)

# fea importance

feaimp_Sn = generateFilterValuesData(task.over_Sn, method = "randomForestSRC_importance")
feaimp_Ba = generateFilterValuesData(task.over_Ba, method = "randomForestSRC_importance")
feaimp_GrW = generateFilterValuesData(task.over_GrW, method = "randomForestSRC_importance")

feaimp_Sn_df=feaimp_Sn[["data"]]
feaimp_Ba_df=feaimp_Ba[["data"]]
feaimp_GrW_df=feaimp_GrW[["data"]]

#run it 100 times

for (i in 1:99) {
  print(i)
  
  feaimp_Sn_2 = generateFilterValuesData(task.over_Sn, method = "randomForestSRC_importance")
  feaimp_Sn_2df=feaimp_Sn_2[["data"]]
  
  feaimp_Sn_df <- rbind(feaimp_Sn_df, feaimp_Sn_2df)
  
  feaimp_Ba_2 = generateFilterValuesData(task.over_Ba, method = "randomForestSRC_importance")
  feaimp_Ba_2df=feaimp_Ba_2[["data"]]
  
  feaimp_Ba_df <- rbind(feaimp_Ba_df, feaimp_Ba_2df)
  
  feaimp_GrW_2 = generateFilterValuesData(task.over_GrW, method = "randomForestSRC_importance")
  feaimp_GrW_2df=feaimp_GrW_2[["data"]]
  
  feaimp_GrW_df <- rbind(feaimp_GrW_df, feaimp_GrW_2df)
  
} 

grouped_Sn <- group_by(feaimp_Sn_df, name)
feaimp_Sn_dfvis<-summarise(grouped_Sn, mean=mean(value), max=max(value),min=min(value))

feaimp_Sn_dfvis$color<-0
feaimp_Sn_dfvis$color[1:4]<-1
feaimp_Sn_dfvis$color[5:11]<-2
feaimp_Sn_dfvis$color[12:18]<-3

grouped_Ba <- group_by(feaimp_Ba_df, name)
feaimp_Ba_dfvis<-summarise(grouped_Ba, mean=mean(value), max=max(value),min=min(value))

feaimp_Ba_dfvis$color<-0
feaimp_Ba_dfvis$color[1:4]<-1
feaimp_Ba_dfvis$color[5:11]<-2
feaimp_Ba_dfvis$color[12:18]<-3

grouped_GrW <- group_by(feaimp_GrW_df, name)
feaimp_GrW_dfvis<-summarise(grouped_GrW, mean=mean(value), max=max(value),min=min(value))

feaimp_GrW_dfvis$color<-0
feaimp_GrW_dfvis$color[1:4]<-1
feaimp_GrW_dfvis$color[5:11]<-2
feaimp_GrW_dfvis$color[12:18]<-3

#visualize
a=ggplot(feaimp_Sn_dfvis, aes(x=name, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4"),name="Metrics type",labels=c("Landcover","LiDAR","Sentinel"))+ylim(0,0.15)

b=ggplot(feaimp_Ba_dfvis, aes(x=name, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4"),name="Metrics type",labels=c("Landcover","LiDAR","Sentinel"))+ylim(0,0.15)

c=ggplot(feaimp_GrW_dfvis, aes(x=name, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4"),name="Metrics type",labels=c("Landcover","LiDAR","Sentinel"))+ylim(0,0.15)

p0=ggplot(feaimp_GrW_dfvis, aes(x=name, y=mean,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = TRUE)+
  geom_errorbar(aes(ymin=min, ymax=max), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4"),name="Metrics type",labels=c("Landcover","LiDAR","Sentinel"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

fig2=grid.arrange(
a,b,c,legend,
ncol=2,
nrow=2
)

ggsave("Fig2.png",plot = fig2,width = 15, height = 15)