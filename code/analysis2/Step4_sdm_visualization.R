library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)
library(sdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April_results/"
setwd(workingdirectory)

# Import
m_merged_GrW=read.sdm("merged_GrW_all2.sdm")
m_merged_Sn=read.sdm("merged_Sn_all2.sdm")

# Feature importance

met="glm"

#vi <- getVarImp(m_merged_GrW, method=c('glm','maxent','rf'))
vi <- getVarImp(m_merged_GrW, method=c(met))
feaimp_GrW_dfvis=vi@varImportanceMean[["AUCtest"]]

feaimp_GrW_dfvis$color<-0
feaimp_GrW_dfvis$color[1:6]<-1
feaimp_GrW_dfvis$color[7:15]<-2
feaimp_GrW_dfvis$color[16:22]<-3

#vi2 <- getVarImp(m_merged_Sn, method=c('glm','maxent','rf'))
vi2 <- getVarImp(m_merged_Sn, method=c(met))
feaimp_Sn_dfvis=vi2@varImportanceMean[["AUCtest"]]

feaimp_Sn_dfvis$color<-0
feaimp_Sn_dfvis$color[1:6]<-1
feaimp_Sn_dfvis$color[7:15]<-2
feaimp_Sn_dfvis$color[16:22]<-3

a1=ggplot(feaimp_GrW_dfvis, aes(x=reorder(variables,-color), y=AUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ylim(-0.01, 0.3)+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4"),name="Metrics type",labels=c("Landcover","LiDAR","Sentinel"))+ggtitle("a. Great reed warbler")

b1=ggplot(feaimp_Sn_dfvis, aes(x=reorder(variables,-color), y=AUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ylim(0, 0.3)+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4"),name="Metrics type",labels=c("Landcover","LiDAR","Sentinel"))+ggtitle("b. Savi's warbler")

p0=ggplot(feaimp_Sn_dfvis, aes(x=reorder(variables,-color), y=AUCtest,fill=as.factor(color))) + geom_bar(stat="identity", color="black", position=position_dodge(),show.legend = TRUE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+
  scale_fill_manual(values = c("1" = "deeppink", "2" = "orange", "3" = "goldenrod4"),name="Metrics type",labels=c("Land cover","LiDAR","Sentinel"))+ggtitle("a. Great reed warbler")

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

ggsave("fig3_ensemble.png",plot = fig2b,width = 16, height =10)