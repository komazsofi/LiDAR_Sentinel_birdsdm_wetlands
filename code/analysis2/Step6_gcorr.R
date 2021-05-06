library(ggplot2)
library(gridExtra)
library(grid)
library(GGally)

library(correlation)
library(see) 
library(ggraph) 


workingdirectory="D:/Koma/Sync_PhD/_Amsterdam/_PhD/Chapter4_Sentinel/3_Dataprocessing/dataprocess_forpaper_march/both_April_results/"
setwd(workingdirectory)

# Import

readRDS("vif_landcover2.rds")
readRDS("vif_lidar2.rds")
readRDS("vif_sentinel2.rds")
readRDS("vif_rasters2.rds")

presabs_GrW=read.csv("presabs_Sn_apr.csv")
presabs_Sn=read.csv("presabs_GrW_apr.csv")

presabs_GrW=presabs_GrW[,-1]
presabs_Sn=presabs_Sn[,-1]

# corrplot

a=ggcorr(presabs_GrW[,c(2:18)], c("pairwise", "spearman"), name = expression(italic("Spearman's r")), label=TRUE, label_alpha=TRUE, label_size=4, hjust=1, size=4, layout.exp=6)+ggtitle("a. Great reed warbler")
b=ggcorr(presabs_Sn[,c(2:18)], c("pairwise", "spearman"), name = expression(italic("Spearman's r")), label=TRUE, label_alpha=TRUE, label_size=4, hjust=1, size=4, layout.exp=6)+ggtitle("b. Savi's warbler")

fig=grid.arrange(
  a,b,
  ncol=1,
  nrow=2
)

ggsave("figcorr.png",plot = fig,width = 12, height =12)

# GGM

presabs_GrW[,c(2:18)] %>% 
  correlation(partial = TRUE) %>% 
  plot()