library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)
library(sdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both/"
setwd(workingdirectory)

# Import
GrW_lid=read.sdm("merged_GrW_lidar.sdm")
GrW_sent=read.sdm("merged_GrW_sentinel.sdm")
GrW_landc=read.sdm("merged_GrW_landc.sdm")

Sn_lid=read.sdm("merged_Sn_lidar.sdm")
Sn_sent=read.sdm("merged_Sn_sentinel.sdm")
Sn_landc=read.sdm("merged_Sn_landc.sdm")

# Feature importance

vi <- getVarImp(GrW_lid, method=c('glm','maxent','rf'))
feaimp_GrW_lid=vi@varImportanceMean[["AUCtest"]]

a1=ggplot(feaimp_GrW_lid, aes(x=variables, y=AUCtest)) + geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ggtitle("a.")

vi <- getVarImp(GrW_sent, method=c('glm','maxent','rf'))
feaimp_GrW_sent=vi@varImportanceMean[["AUCtest"]]

b1=ggplot(feaimp_GrW_sent, aes(x=variables, y=AUCtest)) + geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ggtitle("b.")

vi <- getVarImp(GrW_landc, method=c('glm','maxent','rf'))
feaimp_GrW_landc=vi@varImportanceMean[["AUCtest"]]

c1=ggplot(feaimp_GrW_landc, aes(x=variables, y=AUCtest)) + geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ggtitle("c.")

vi <- getVarImp(Sn_lid, method=c('glm','maxent','rf'))
feaimp_Sn_lid=vi@varImportanceMean[["AUCtest"]]

a2=ggplot(feaimp_Sn_lid, aes(x=variables, y=AUCtest)) + geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ggtitle("d.")

vi <- getVarImp(Sn_sent, method=c('glm','maxent','rf'))
feaimp_Sn_sent=vi@varImportanceMean[["AUCtest"]]

b2=ggplot(feaimp_Sn_sent, aes(x=variables, y=AUCtest)) + geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ggtitle("e.")

vi <- getVarImp(Sn_landc, method=c('glm','maxent','rf'))
feaimp_Sn_landc=vi@varImportanceMean[["AUCtest"]]

c2=ggplot(feaimp_Sn_landc, aes(x=variables, y=AUCtest)) + geom_bar(stat="identity", position=position_dodge(),show.legend = FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,position=position_dodge(.9))+
  coord_flip()+theme_bw(base_size = 20)+ylab("Feature Importance")+xlab("Metrics")+ggtitle("f.")

t1 <- textGrob("Savis's warbler",gp=gpar(fontsize=20, col="black", fontface="bold"))
t3 <- textGrob("Great reed warbler",gp=gpar(fontsize=20, col="black", fontface="bold"))

figs=grid.arrange(
  a1,b1,c1,a2,b2,c2,t1,t3,
  ncol=3,
  nrow=4,
  layout_matrix=rbind(c(8,8,8),c(1,2,3),c(7,7,7),c(4,5,6)),
  widths = c(1,1,1),
  heights = c(0.2,1,0.2,1)
)

ggsave("fig_A1.png",plot = figs,width = 18, height =12)