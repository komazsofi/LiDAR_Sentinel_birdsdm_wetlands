library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)
library(sdm)

workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April_results/"
setwd(workingdirectory)

# Import

GrW_lid=read.sdm("merged_GrW_lidar2.sdm")
GrW_sent=read.sdm("merged_GrW_sentinel2.sdm")
GrW_landc=read.sdm("merged_GrW_landc2.sdm")
GrW_lidsent=read.sdm("merged_GrW_lidsent2.sdm")
GrW_all=read.sdm("merged_GrW_all2.sdm")

GrW_lid_acc=getEvaluation(GrW_lid,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_sent_acc=getEvaluation(GrW_sent,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_landc_acc=getEvaluation(GrW_landc,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_lidsent_acc=getEvaluation(GrW_lidsent,stat=c('AUC','TSS','Deviance','Kappa'))
GrW_all_acc=getEvaluation(GrW_all,stat=c('AUC','TSS','Deviance','Kappa'))

GrW_lid_acc$RStype<-"LiDAR"
GrW_sent_acc$RStype<-"Sentinel"
GrW_landc_acc$RStype<-"Land cover"
GrW_lidsent_acc$RStype<-"LiD+Sent"
GrW_all_acc$RStype<-"All"

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

GrW_merged$RStype <- factor(GrW_merged$RStype , levels=c("Land cover","LiDAR", "Sentinel","LiD+Sent","All"))
GrW_merged$modeltype <- factor(GrW_merged$modeltype , levels=c("glm","maxent","rf"))

Sn_lid=read.sdm("merged_Sn_lidar2.sdm")
Sn_sent=read.sdm("merged_Sn_sentinel2.sdm")
Sn_landc=read.sdm("merged_Sn_landc2.sdm")
Sn_lidsent=read.sdm("merged_Sn_lidsent2.sdm")
Sn_all=read.sdm("merged_Sn_all2.sdm")

Sn_lid_acc=getEvaluation(Sn_lid,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_sent_acc=getEvaluation(Sn_sent,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_landc_acc=getEvaluation(Sn_landc,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_lidsent_acc=getEvaluation(Sn_lidsent,stat=c('AUC','TSS','Deviance','Kappa'))
Sn_all_acc=getEvaluation(Sn_all,stat=c('AUC','TSS','Deviance','Kappa'))

Sn_lid_acc$RStype<-"LiDAR"
Sn_sent_acc$RStype<-"Sentinel"
Sn_landc_acc$RStype<-"Land cover"
Sn_lidsent_acc$RStype<-"LiD+Sent"
Sn_all_acc$RStype<-"All"

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

Sn_merged$RStype <- factor(Sn_merged$RStype , levels=c("Land cover","LiDAR", "Sentinel","LiD+Sent", "All"))
Sn_merged$modeltype <- factor(Sn_merged$modeltype , levels=c("glm","maxent","rf"))

# Only visualize RF

met="maxent"
met="glm"
met="rf"

aa=ggplot(GrW_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("AUC")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

bb=ggplot(GrW_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("TSS")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

cc=ggplot(GrW_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=Deviance,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Deviance")+ylim(0,2)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

dd=ggplot(GrW_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=Kappa,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Kappa")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

aa2=ggplot(Sn_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("AUC")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

bb2=ggplot(Sn_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("TSS")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

cc2=ggplot(Sn_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=Deviance,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Deviance")+ylim(0,2)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

dd2=ggplot(Sn_merged[GrW_merged$modeltype==met,], aes(x=RStype, y=Kappa,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Kappa")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

t1 <- textGrob("(a) Great reed warbler",gp=gpar(fontsize=25, col="black", fontface="bold"))
t2 <- textGrob("(b) Savis's warbler",gp=gpar(fontsize=25, col="black", fontface="bold"))

fig2=grid.arrange(
  t1,aa,bb,cc,dd,
  t2,aa2,bb2,cc2,dd2,
  ncol=4,
  nrow=4,
  layout_matrix=rbind(c(1,1,1,1),c(2,3,4,5), c(6,6,6,6),c(7,8,9,10)),
  widths = c(1,1,1,1),
  heights = c(0.3,1,0.3,1)
)

ggsave("fig2_maxent_vx.png",plot = fig2,width = 18, height =14)
ggsave("fig2_glm_vx.png",plot = fig2,width = 18, height =14)
ggsave("fig2_rf_vx.png",plot = fig2,width = 18, height =14)

# Ensemble

aa=ggplot(GrW_merged, aes(x=RStype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("AUC")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

bb=ggplot(GrW_merged, aes(x=RStype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("TSS")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

cc=ggplot(GrW_merged, aes(x=RStype, y=Deviance,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Deviance")+ylim(0,2)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

dd=ggplot(GrW_merged, aes(x=RStype, y=Kappa,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Kappa")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

aa2=ggplot(Sn_merged, aes(x=RStype, y=AUC,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("AUC")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

bb2=ggplot(Sn_merged, aes(x=RStype, y=TSS,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("TSS")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

cc2=ggplot(Sn_merged, aes(x=RStype, y=Deviance,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Deviance")+ylim(0,2)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

dd2=ggplot(Sn_merged, aes(x=RStype, y=Kappa,fill=RStype)) + 
  geom_boxplot(show.legend = FALSE)+theme_bw(base_size = 25)+ylab("Kappa")+ylim(0,1)+
  xlab("")+
  scale_fill_manual(values = c("LiDAR" = "orange", "Sentinel" = "goldenrod4", "Land cover" = "deeppink", "LiD+Sent"="chocolate", "All"="coral2"),name="Metrics type")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5))

t1 <- textGrob("(a) Great reed warbler",gp=gpar(fontsize=25, col="black", fontface="bold"))
t2 <- textGrob("(b) Savis's warbler",gp=gpar(fontsize=25, col="black", fontface="bold"))

fig2=grid.arrange(
  t1,aa,bb,cc,dd,
  t2,aa2,bb2,cc2,dd2,
  ncol=4,
  nrow=4,
  layout_matrix=rbind(c(1,1,1,1),c(2,3,4,5), c(6,6,6,6),c(7,8,9,10)),
  widths = c(1,1,1,1),
  heights = c(0.3,1,0.3,1)
)

ggsave("fig2_ensav_vx.png",plot = fig2,width = 18, height =14)

# report accuracy table

acc_grouped_GrW <- group_by(GrW_merged, RStype,modeltype)
acc_GrW_table<-summarise(acc_grouped_GrW, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                         meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

acc_grouped_Sn<- group_by(Sn_merged, RStype,modeltype)
acc_Sn_table<-summarise(acc_grouped_Sn, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                        meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

write.csv(acc_GrW_table,"acc_GrW_table.csv")
write.csv(acc_Sn_table,"acc_Sn_table.csv")

acc_grouped_GrW2 <- group_by(GrW_merged, RStype)
acc_GrW_table2<-summarise(acc_grouped_GrW2, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                         meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

acc_grouped_Sn2<- group_by(Sn_merged, RStype)
acc_Sn_table2<-summarise(acc_grouped_Sn2, meanAUC=mean(AUC),sdAUC=sd(AUC), meanDev=mean(Deviance),sdDev=sd(Deviance),
                        meanTSS=mean(TSS),sdTSS=sd(TSS),meanKappa=mean(Kappa),sdKappa=sd(Kappa))

write.csv(acc_GrW_table2,"acc_GrW_table_ens.csv")
write.csv(acc_Sn_table2,"acc_Sn_table_ens.csv")

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

png("GrW_lidsent.png", width = 6, height = 5, units = 'in',res=300)
roc(GrW_lidsent)
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

png("Sn_lidsent.png", width = 6, height = 5, units = 'in',res=300)
roc(Sn_lidsent)
dev.off()

png("Sn_all.png", width = 6, height = 5, units = 'in',res=300)
roc(Sn_all)
dev.off()