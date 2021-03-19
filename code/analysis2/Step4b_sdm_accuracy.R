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
GrW_lidsent=read.sdm("merged_GrW_lidsent.sdm")
GrW_all=read.sdm("merged_GrW_all.sdm")

GrW_lid_acc=getEvaluation(GrW_lid,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_sent_acc=getEvaluation(GrW_sent,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_landc_acc=getEvaluation(GrW_landc,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_lidsent_acc=getEvaluation(GrW_lidsent,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_all_acc=getEvaluation(GrW_all,stat=c('AUC','TSS','Deviance','Kappa'))

GrW_lid_acc$RStype<-"lidar"
GrW_sent_acc$RStype<-"sentinel"
GrW_landc_acc$RStype<-"landc"
GrW_lidsent_acc$RStype<-"lidsent"
GrW_all_acc$RStype<-"lidall"

GrW_sent_acc$modeltype<-NA
GrW_sent_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
GrW_sent_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
GrW_sent_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

GrW_lid_acc$modeltype<-NA
GrW_lid_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
GrW_lid_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
GrW_lid_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

GrW_landc_acc$modeltype<-NA
GrW_landc_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
GrW_landc_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
GrW_landc_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

GrW_lidsent_acc$modeltype<-NA
GrW_lidsent_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
GrW_lidsent_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
GrW_lidsent_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

GrW_all_acc$modeltype<-NA
GrW_all_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
GrW_all_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
GrW_all_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

GrW_merged=rbind(GrW_lid_acc,GrW_sent_acc,GrW_landc_acc,GrW_lidsent_acc,GrW_all_acc)

GrW_merged2=GrW_merged
GrW_merged2$modeltype<-"ensemble"

accuracy_GrW3=rbind(GrW_merged,GrW_merged2)

accuracy_GrW3$RStype <- factor(accuracy_GrW3$RStype , levels=c("lidar", "sentinel", "landc", "lidsent","lidall"))
accuracy_GrW3$modeltype <- factor(accuracy_GrW3$modeltype , levels=c("glm","maxent","rf","ensemble"))

Sn_lid=read.sdm("merged_Sn_lidar.sdm")
Sn_sent=read.sdm("merged_Sn_sentinel.sdm")
Sn_landc=read.sdm("merged_Sn_landc.sdm")
Sn_lidsent=read.sdm("merged_Sn_lidsent.sdm")
Sn_all=read.sdm("merged_Sn_all.sdm")

Sn_lid_acc=getEvaluation(Sn_lid,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_sent_acc=getEvaluation(Sn_sent,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_landc_acc=getEvaluation(Sn_landc,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_lidsent_acc=getEvaluation(Sn_lidsent,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_all_acc=getEvaluation(Sn_all,stat=c('AUC','TSS','Deviance','Kappa'))

Sn_lid_acc$RStype<-"lidar"
Sn_sent_acc$RStype<-"sentinel"
Sn_landc_acc$RStype<-"landc"
Sn_lidsent_acc$RStype<-"lidsent"
Sn_all_acc$RStype<-"lidall"

Sn_sent_acc$modeltype<-NA
Sn_sent_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
Sn_sent_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
Sn_sent_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

Sn_lid_acc$modeltype<-NA
Sn_lid_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
Sn_lid_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
Sn_lid_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

Sn_landc_acc$modeltype<-NA
Sn_landc_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
Sn_landc_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
Sn_landc_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

Sn_lidsent_acc$modeltype<-NA
Sn_lidsent_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
Sn_lidsent_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
Sn_lidsent_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

Sn_all_acc$modeltype<-NA
Sn_all_acc$modeltype[c(1:20,61:80,121:140,181:200,241:260)]<-"glm"
Sn_all_acc$modeltype[c(21:40,81:100,141:160,201:220,261:280)]<-"maxent"
Sn_all_acc$modeltype[c(41:60,101:120,161:180,221:240,281:300)]<-"rf"

Sn_merged=rbind(Sn_lid_acc,Sn_sent_acc,Sn_landc_acc,Sn_lidsent_acc,Sn_all_acc)

Sn_merged2=Sn_merged
Sn_merged2$modeltype<-"ensemble"

accuracy_Sn3=rbind(Sn_merged,Sn_merged2)

accuracy_Sn3$RStype <- factor(accuracy_Sn3$RStype , levels=c("lidar", "sentinel", "landc", "lidsent","lidall"))
accuracy_Sn3$modeltype <- factor(accuracy_Sn3$modeltype , levels=c("glm","maxent","rf","ensemble"))

# visualization

c=ggplot(accuracy_GrW3, aes(x=modeltype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+ylab("AUC")+ylim(0.2,1)+
  xlab("SDM")+ggtitle("a. Great reed warbler")+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))
d=ggplot(accuracy_Sn3, aes(x=modeltype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+
  ylab("AUC")+xlab("SDM")+ggtitle("b. Savi's warbler")+ylim(0.2,1)+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))

p1=ggplot(accuracy_GrW3, aes(x=modeltype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = TRUE)+theme_bw(base_size = 20)+ylab("AUC")+ylim(0.2,1)+
  xlab("SDM")+ggtitle("a. Great reed warbler")+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend2 <- get_legend(p1)

fig1b=grid.arrange(
  c,d,legend2,
  ncol=2,
  nrow=2,
  layout_matrix=rbind(c(1,3),c(2,3)),
  widths = c(1,0.5),
  heights = c(1,1)
)

ggsave("fig2v2.png",plot = fig1b,width = 10, height =10)

c2=ggplot(accuracy_GrW3, aes(x=modeltype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+ylab("TSS")+ylim(0,1)+
  xlab("SDM")+ggtitle("c. Great reed warbler")+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))
d2=ggplot(accuracy_Sn3, aes(x=modeltype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+
  ylab("TSS")+xlab("SDM")+ggtitle("d. Savi's warbler")+ylim(0,1)+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))

p2=ggplot(accuracy_GrW3, aes(x=modeltype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = TRUE)+theme_bw(base_size = 20)+ylab("TSS")+ylim(0,1)+
  xlab("SDM")+ggtitle("a. Great reed warbler")+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend3 <- get_legend(p2)

fig1c=grid.arrange(
  c2,d2,legend3,
  ncol=2,
  nrow=2,
  layout_matrix=rbind(c(1,3),c(2,3)),
  widths = c(1,0.5),
  heights = c(1,1)
)

ggsave("fig2tss.png",plot = fig1c,width = 10, height =10)

fig1d=grid.arrange(
  c,d,c2,d2,legend3,
  ncol=3,
  nrow=2,
  layout_matrix=rbind(c(1,2,5),c(3,4,5)),
  widths = c(1,1,0.5),
  heights = c(1,1)
)

ggsave("fig2_comb.png",plot = fig1d,width = 16, height =10)

# report accuracy table

acc_grouped_GrW <- group_by(accuracy_GrW3, RStype,modeltype)
acc_GrW_table<-summarise(acc_grouped_GrW, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                         meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

acc_grouped_Sn<- group_by(accuracy_Sn3, RStype,modeltype)
acc_Sn_table<-summarise(acc_grouped_Sn, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                        meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

write.csv(acc_GrW_table,"acc_GrW_table.csv")
write.csv(acc_Sn_table,"acc_Sn_table.csv")