library(dplyr)

library(ggplot2)
library(gridExtra)
library(grid)
library(sdm)

#workingdirectory="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/"
workingdirectory="D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Revision/"
setwd(workingdirectory)

# Import

#GRW
GrW_lid_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_GrW_lidar2.sdm")
GrW_sent_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_GrW_sentinel2.sdm")
GrW_landc_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_GrW_landc2.sdm")
GrW_lidsent_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_GrW_lidsent2.sdm")
GrW_all_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_GrW_all2.sdm")

GrW_lid_n=read.sdm("merged_GrW_lidar2.sdm")
GrW_sent_n=read.sdm("merged_GrW_sentinel2.sdm")
GrW_landc_n=read.sdm("merged_GrW_landc2.sdm")
GrW_lidsent_n=read.sdm("merged_GrW_lidsent2.sdm")
GrW_all_n=read.sdm("merged_GrW_all2.sdm")

m_GrW_lid_o_n=GrW_lid_o+GrW_lid_n
m_GrW_sent_o_n=GrW_sent_o+GrW_sent_n
m_GrW_landc_o_n=GrW_landc_o+GrW_landc_n
m_GrW_lidsent_o_n=GrW_lidsent_o+GrW_lidsent_n
m_GrW_all_o_n=GrW_all_o+GrW_all_n

write.sdm(m_GrW_lid_o_n,"n_merged_GrW_lidar2",overwrite = TRUE)
write.sdm(m_GrW_sent_o_n,"n_merged_GrW_sentinel2",overwrite = TRUE)
write.sdm(m_GrW_landc_o_n,"n_merged_GrW_landc2",overwrite = TRUE)
write.sdm(m_GrW_lidsent_o_n,"n_merged_Grw_lidsent2",overwrite = TRUE)
write.sdm(m_GrW_all_o_n,"n_merged_GrW_all2",overwrite = TRUE)

#SW
SW_lid_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_Sn_lidar2.sdm")
SW_sent_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_Sn_sentinel2.sdm")
SW_landc_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_Sn_landc2.sdm")
SW_lidsent_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_Sn_lidsent2.sdm")
SW_all_o=read.sdm("D:/Koma/sync/_Amsterdam/ZsofiaKoma_PhD_cleaned/Chapter4/2_Dataprocess/Results/merged_Sn_all2.sdm")

SW_lid_n=read.sdm("merged_Sn_lidar2.sdm")
SW_sent_n=read.sdm("merged_Sn_sentinel2.sdm")
SW_landc_n=read.sdm("merged_Sn_landc2.sdm")
SW_lidsent_n=read.sdm("merged_Sn_lidsent2.sdm")
SW_all_n=read.sdm("merged_Sn_all2.sdm")

m_SW_lid_o_n=SW_lid_o+SW_lid_n
m_SW_sent_o_n=SW_sent_o+SW_sent_n
m_SW_landc_o_n=SW_landc_o+SW_landc_n
m_SW_lidsent_o_n=SW_lidsent_o+SW_lidsent_n
m_SW_all_o_n=SW_all_o+SW_all_n

write.sdm(m_SW_lid_o_n,"n_merged_Sn_lidar2",overwrite = TRUE)
write.sdm(m_SW_sent_o_n,"n_merged_Sn_sentinel2",overwrite = TRUE)
write.sdm(m_SW_landc_o_n,"n_merged_Sn_landc2",overwrite = TRUE)
write.sdm(m_SW_lidsent_o_n,"n_merged_Sn_lidsent2",overwrite = TRUE)
write.sdm(m_SW_all_o_n,"n_merged_Sn_all2",overwrite = TRUE)