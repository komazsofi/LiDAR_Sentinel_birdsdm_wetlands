library(raster)
library(mlr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
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

presabs_Sn$class<-"yes"
presabs_Sn[presabs_Sn$occurrence==0,22]<-"no"
presabs_Ba$class<-"yes"
presabs_Ba[presabs_Ba$occurrence==0,22]<-"no"
presabs_GrW$class<-"yes"
presabs_GrW[presabs_GrW$occurrence==0,22]<-"no"

# start mlr

set.seed(11)

taskall_Sn = makeClassifTask(data = presabs_Sn[,c(1:18,22)], target = "class",
                             positive = "yes", coordinates = presabs_Sn[,c(20,21)])

task.over_Sn = oversample(taskall_Sn, rate = 2)

taskall_Ba = makeClassifTask(data = presabs_Ba[,c(1:18,22)], target = "class",
                             positive = "yes", coordinates = presabs_Ba[,c(20,21)])

task.over_Ba = oversample(taskall_Ba, rate = 2.5)

taskall_GrW = makeClassifTask(data = presabs_GrW[,c(1:18,22)], target = "class",
                              positive = "yes", coordinates = presabs_GrW[,c(20,21)])

task.over_GrW = oversample(taskall_GrW, rate = 4)

# fit model

lrnRF = makeLearner("classif.randomForest",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

model_Sn = mlr::train(lrnRF, task.over_Sn)
model_Ba = mlr::train(lrnRF, task.over_Ba)
model_GrW = mlr::train(lrnRF, task.over_GrW)

# partial dependence plot

pd_Sn = generatePartialDependenceData(model_Sn, task.over_Sn, c("lidar_HH_reedveg_prop","landcover_propswamp","lidar_C_ppr"),fun = mean)
#plotPartialDependence(pd_Sn)+theme_bw(base_size = 20)+ylab("Probability")+xlab("Metrics")+ylim(0,1)

pd_Sn_resp=pd_Sn[["data"]]

pd_Ba = generatePartialDependenceData(model_Ba, task.over_Ba, c("lidar_HH_reedveg_prop","optical_NDVIstd","lidar_VD_1_2"),fun = mean)
pd_Ba_resp=pd_Ba[["data"]]

pd_GrW = generatePartialDependenceData(model_GrW, task.over_Sn, c("optical_NDVIsd_hor_100m","lidar_VD_2_3","lidar_HH_reedveg_prop"),fun = mean)
pd_GrW_resp=pd_GrW[["data"]]

write.csv(pd_Sn_resp,"pd_Sn_resp.csv")
write.csv(pd_Ba_resp,"pd_Ba_resp.csv")
write.csv(pd_GrW_resp,"pd_GrW_resp.csv")

pd_Sn_resp=read.csv("pd_Sn_resp.csv")
pd_Ba_resp=read.csv("pd_Ba_resp.csv")
pd_GrW_resp=read.csv("pd_GrW_resp.csv")

# visualization

a=ggplot(pd_Sn_resp,aes(x=lidar_HH_reedveg_prop,y=yes))+geom_line(color="orange",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_HH_reedveg_prop")+ylim(0,1)
b=ggplot(pd_Sn_resp,aes(x=landcover_propswamp,y=yes))+geom_line(color="deeppink",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("landcover_propswamp")+ylim(0,1)
c=ggplot(pd_Sn_resp,aes(x=lidar_C_ppr,y=yes))+geom_line(color="orange",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_C_ppr")+ylim(0,1)

e=ggplot(pd_Ba_resp,aes(x=lidar_HH_reedveg_prop,y=yes))+geom_line(color="orange",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_HH_reedveg_prop")+ylim(0,1)
f=ggplot(pd_Ba_resp,aes(x=optical_NDVIstd,y=yes))+geom_line(color="goldenrod4",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("optical_NDVIstd")+ylim(0,1)
g=ggplot(pd_Ba_resp,aes(x=lidar_VD_1_2,y=yes))+geom_line(color="orange",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_VD_1_2")+ylim(0,1)

h=ggplot(pd_GrW_resp,aes(x=optical_NDVIsd_hor_100m,y=yes))+geom_line(color="goldenrod4",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("optical_NDVIsd_hor_100m")+ylim(0,1)
i=ggplot(pd_GrW_resp,aes(x=lidar_VD_2_3,y=yes))+geom_line(color="orange",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_VD_2_3")+ylim(0,1)
j=ggplot(pd_GrW_resp,aes(x=lidar_HH_reedveg_prop,y=yes))+geom_line(color="orange",size=4)+theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_HH_reedveg_prop")+ylim(0,1)

t1 <- textGrob("A. Savis's Warbler",gp=gpar(fontsize=16, col="black", fontface="bold"))
t2 <- textGrob("B. Bearded reedling",gp=gpar(fontsize=16, col="black", fontface="bold"))
t3 <- textGrob("C. Great reed warbler",gp=gpar(fontsize=16, col="black", fontface="bold"))

p0=ggplot(pd_Sn_resp,aes(x=lidar_HH_reedveg_prop,y=yes))+geom_line(aes(color="goldenrod4"),size=4)+
  geom_line(aes(color="orange"),size=4)+geom_line(aes(color="deeppink"),size=4)+
  theme_bw(base_size = 16)+ylab("Probability")+xlab("lidar_HH_reedveg_prop")+
  ylim(0,1)+scale_color_identity(name = "Metrics type",breaks = c("goldenrod4", "orange", "deeppink"),labels = c("Sentinel", "LiDAR", "Land cover"),guide = "legend")+theme(legend.position="bottom")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

fig3=grid.arrange(
  a,b,c,e,f,g,h,i,j,
  t1,t2,t3,legend,
  ncol=3,
  nrow=7,
  layout_matrix=rbind(c(10,10,10),c(1,2,3),c(11,11,11),c(4,5,6), c(12,12,12),c(7,8,9),c(13,13,13)),
  widths = c(1,1,1),
  heights = c(0.2,4,0.2,4,0.2,4,0.3)
)

ggsave("Fig3.png",plot = fig3,width = 10, height = 13)
