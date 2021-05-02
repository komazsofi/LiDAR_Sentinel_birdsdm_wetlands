library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)
library(sdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April_results/"
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

GrW_merged$RStype <- factor(GrW_merged$RStype , levels=c("lidar", "sentinel", "landc", "lidsent","lidall"))
GrW_merged$modeltype <- factor(GrW_merged$modeltype , levels=c("glm","maxent","rf"))

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

Sn_merged$RStype <- factor(Sn_merged$RStype , levels=c("lidar", "sentinel", "landc","lidsent", "lidall"))
Sn_merged$modeltype <- factor(Sn_merged$modeltype , levels=c("glm","maxent","rf"))

# visualization

c=ggplot(GrW_merged, aes(x=modeltype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+ylab("AUC")+ylim(0.2,1)+
  xlab("SDM")+ggtitle("a. Great reed warbler")+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink","lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))
d=ggplot(Sn_merged, aes(x=modeltype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+
  ylab("AUC")+xlab("SDM")+ggtitle("b. Savi's warbler")+ylim(0.2,1)+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink","lidsent"="chocolate",  "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))

p1=ggplot(GrW_merged, aes(x=modeltype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = TRUE)+theme_bw(base_size = 20)+ylab("AUC")+ylim(0.2,1)+
  xlab("SDM")+ggtitle("a. Great reed warbler")+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink","lidsent"="chocolate",  "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))

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

ggsave("fig2v3.png",plot = fig1b,width = 10, height =10)

c2=ggplot(GrW_merged, aes(x=modeltype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+ylab("TSS")+ylim(0,1)+
  xlab("SDM")+ggtitle("c. Great reed warbler")+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))
d2=ggplot(Sn_merged, aes(x=modeltype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 20)+
  ylab("TSS")+xlab("SDM")+ggtitle("d. Savi's warbler")+ylim(0,1)+
  scale_fill_manual(values = c("lidar" = "orange", "sentinel" = "goldenrod4", "landc" = "deeppink", "lidsent"="chocolate", "lidall"="coral2"),name="Metrics type",labels=c("LiDAR","Sentinel","Landcover","LiDAR+Sentinel","All"))

p2=ggplot(GrW_merged, aes(x=modeltype, y=TSS,fill=RStype)) + 
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

# Only visualize RF



# report accuracy table

acc_grouped_GrW <- group_by(GrW_merged, RStype,modeltype)
acc_GrW_table<-summarise(acc_grouped_GrW, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                         meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

acc_grouped_Sn<- group_by(Sn_merged, RStype,modeltype)
acc_Sn_table<-summarise(acc_grouped_Sn, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                        meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

write.csv(acc_GrW_table,"acc_GrW_table.csv")
write.csv(acc_Sn_table,"acc_Sn_table.csv")

# ROC curves
png("GrW_lid.png", width = 6, height = 5, units = 'in',res=300)
roc(GrW_lid)
dev.off()

png("GrW_sent.png", width = 6, height = 5, units = 'in',res=300)
roc(GrW_sent)
dev.off()

png("GrW_landc.png", width = 6, height = 5, units = 'in',res=300)
roc(GrW_landc)
dev.off()

png("GrW_all.png", width = 6, height = 5, units = 'in',res=300)
roc(GrW_all)
dev.off()

png("Sn_lid.png", width = 6, height = 5, units = 'in',res=300)
roc(Sn_lid)
dev.off()

png("Sn_sent.png", width = 6, height = 5, units = 'in',res=300)
roc(Sn_sent)
dev.off()

png("Sn_landc.png", width = 6, height = 5, units = 'in',res=300)
roc(Sn_landc)
dev.off()

png("Sn_all.png", width = 6, height = 5, units = 'in',res=300)
roc(Sn_all)
dev.off()