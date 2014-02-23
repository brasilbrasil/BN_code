rm(list = ls()) #remove all cpast worksheet variables
wd="C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/1-Model parameterization/spatial data discretization/"
setwd(wd)

file_nm="all_spp_values_nobioregion.csv"     #get rid of header of first row
outname="landscape_metrics_nobioregion.csv"

quant_levels=function(x){
  y=quantile(x, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE)
  lowQ=y[2]
  highQ=y[4]
  quant_data=array("none", dim(raw_landscape_data2)[1])
  quant_data[x<lowQ]= "low"
  quant_data[x>highQ]= "high"
  quant_data[x<=highQ & x>=lowQ]="medium"
  quant_data
}

raw_landscape_data=read.csv(file_nm)
raw_landscape_data2=raw_landscape_data[raw_landscape_data$sqkm_area_CCE!='not present',]
raw_landscape_data2=raw_landscape_data2[raw_landscape_data2$sqkm_area_CCE!='NA',]
head(raw_landscape_data2)

#ensure data is in numeric, not strnig format
jnk=dim(raw_landscape_data2)[2]
for (i in 4:jnk){
  x=raw_landscape_data2[,i]
  if (class(x)!="numeric"){
    x=as.numeric(levels(x)[x])
  }
  raw_landscape_data2[,i]=x
}


indicator_mat <- data.frame(sp_name=raw_landscape_data2$sp_name,sp_code=raw_landscape_data2$sp_code)

#######CALCULATE METRICS
#Qsqkm_area_CCE
x=raw_landscape_data2$sqkm_area_CCE
sqkm_area_CCE=x
y=quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE)
lowQ=y[2]
highQ=y[4]
quant_data=array("none", dim(raw_landscape_data2)[1])
quant_data[x<lowQ]= "low"
quant_data[x>highQ]= "high"
quant_data[x<=highQ & x>=lowQ]="medium"
Qsqkm_area_CCE=quant_data
indicator_mat=cbind(indicator_mat, sqkm_area_CCE)
indicator_mat=cbind(indicator_mat, Qsqkm_area_CCE)

#Qsqkm_area_FCE
x=raw_landscape_data2$sqkm_area_FCE
sqkm_area_FCE=x
quant_data=array("none", dim(raw_landscape_data2)[1])
quant_data[x<lowQ]= "low"
quant_data[x>highQ]= "high"
quant_data[x<=highQ & x>=lowQ]="medium"
Qsqkm_area_FCE=quant_data
indicator_mat=cbind(indicator_mat, sqkm_area_FCE)
indicator_mat=cbind(indicator_mat, Qsqkm_area_FCE)

#Qprop_area_dif
prop_area_dif=raw_landscape_data2$area_dif/raw_landscape_data2$sqkm_area_CCE
Qprop_area_dif=quant_levels(prop_area_dif)
indicator_mat=cbind(indicator_mat, prop_area_dif)
indicator_mat=cbind(indicator_mat, Qprop_area_dif)

#Qprop_CE_dist
prop_CE_dist=raw_landscape_data2$FCE_distance/sqrt(raw_landscape_data2$sqkm_area_CCE)
Qprop_CE_dist=quant_levels(prop_CE_dist)
indicator_mat=cbind(indicator_mat, prop_CE_dist)
indicator_mat=cbind(indicator_mat, Qprop_CE_dist)

#Qzone_area
zone_areaMR=raw_landscape_data2$zone_areaMR
Qzone_areaMR=quant_levels(zone_areaMR)
indicator_mat=cbind(indicator_mat, zone_areaMR)
indicator_mat=cbind(indicator_mat, Qzone_areaMR)

zone_areaTL=raw_landscape_data2$zone_areaTL
Qzone_areaTL=quant_levels(zone_areaTL)
indicator_mat=cbind(indicator_mat, zone_areaTL)
indicator_mat=cbind(indicator_mat, Qzone_areaTL)

zone_areaMG=raw_landscape_data2$zone_areaMG
Qzone_areaMG=quant_levels(zone_areaMG)
indicator_mat=cbind(indicator_mat, zone_areaMG)
indicator_mat=cbind(indicator_mat, Qzone_areaMG)

#Amount of overlap
prop_CE_overlap=raw_landscape_data2$zone_areaTL/raw_landscape_data2$sqkm_area_CCE
Qprop_CE_overlap=quant_levels(prop_CE_overlap)
indicator_mat=cbind(indicator_mat, prop_CE_overlap)
indicator_mat=cbind(indicator_mat, Qprop_CE_overlap)

#Qprop_zone_lava_area
indicator_mat=cbind(indicator_mat, raw_landscape_data2$Sp_pioneer_status)

prop_MR_lava_area=raw_landscape_data2$zone_lavaflow_area_MR/raw_landscape_data2$zone_areaMR
Qprop_MR_lava_area=quant_levels(prop_MR_lava_area)
indicator_mat=cbind(indicator_mat, prop_MR_lava_area)
indicator_mat=cbind(indicator_mat, Qprop_MR_lava_area)

prop_TL_lava_area=raw_landscape_data2$zone_lavaflow_area_TL/raw_landscape_data2$zone_areaTL
Qprop_TL_lava_area=quant_levels(prop_TL_lava_area)
indicator_mat=cbind(indicator_mat, prop_TL_lava_area)
indicator_mat=cbind(indicator_mat, Qprop_TL_lava_area)

prop_MG_lava_area=raw_landscape_data2$zone_lavaflow_area_MG/raw_landscape_data2$zone_areaMG
Qprop_MG_lava_area=quant_levels(prop_MG_lava_area)
indicator_mat=cbind(indicator_mat, prop_MG_lava_area)
indicator_mat=cbind(indicator_mat, Qprop_MG_lava_area)

#Qzone_good_quality_habitat
MR_good=raw_landscape_data2$MR_good/raw_landscape_data2$zone_areaMR
QMR_good=quant_levels(MR_good)
indicator_mat=cbind(indicator_mat, MR_good)
indicator_mat=cbind(indicator_mat, QMR_good)

TL_good=raw_landscape_data2$TL_good/raw_landscape_data2$zone_areaTL
QTL_good=quant_levels(TL_good)
indicator_mat=cbind(indicator_mat, TL_good)
indicator_mat=cbind(indicator_mat, QTL_good)

MG_good=raw_landscape_data2$MG_good/raw_landscape_data2$zone_areaMG
QMG_good=quant_levels(MG_good)
indicator_mat=cbind(indicator_mat, MG_good)
indicator_mat=cbind(indicator_mat, QMG_good)

#Protected_area
MR_protected_area=raw_landscape_data2$MR_protected_area/raw_landscape_data2$zone_areaMR
QMR_protected_area=quant_levels(MR_protected_area)
indicator_mat=cbind(indicator_mat, MR_protected_area)
indicator_mat=cbind(indicator_mat, QMR_protected_area)

TL_protected_area=raw_landscape_data2$TL_protected_area/raw_landscape_data2$zone_areaTL
QTL_protected_area=quant_levels(TL_protected_area)
indicator_mat=cbind(indicator_mat, TL_protected_area)
indicator_mat=cbind(indicator_mat, QTL_protected_area)

MG_protected_area=raw_landscape_data2$MG_protected_area/raw_landscape_data2$zone_areaMG
QMG_protected_area=quant_levels(MG_protected_area)
indicator_mat=cbind(indicator_mat, MG_protected_area)
indicator_mat=cbind(indicator_mat, QMG_protected_area)

#Ung_free_Areas
MR_Ung_free_Areas=raw_landscape_data2$MR_Ung_free_Areas/raw_landscape_data2$zone_areaMR
QMR_Ung_free_Areas=quant_levels(MR_Ung_free_Areas)
indicator_mat=cbind(indicator_mat, MR_Ung_free_Areas)
indicator_mat=cbind(indicator_mat, QMR_Ung_free_Areas)

TL_Ung_free_Areas=raw_landscape_data2$TL_Ung_free_Areas/raw_landscape_data2$zone_areaTL
QTL_Ung_free_Areas=quant_levels(TL_Ung_free_Areas)
indicator_mat=cbind(indicator_mat, TL_Ung_free_Areas)
indicator_mat=cbind(indicator_mat, QTL_Ung_free_Areas)

MG_Ung_free_Areas=raw_landscape_data2$MG_Ung_free_Areas/raw_landscape_data2$zone_areaMG
QMG_Ung_free_Areas=quant_levels(MG_Ung_free_Areas)
indicator_mat=cbind(indicator_mat, MG_Ung_free_Areas)
indicator_mat=cbind(indicator_mat, QMG_Ung_free_Areas)

#zone_slope_mean
MR_zone_slope_mean=raw_landscape_data2$zone_slope_meanMR
QMR_zone_slope_mean=quant_levels(MR_zone_slope_mean)
indicator_mat=cbind(indicator_mat, MR_zone_slope_mean)
indicator_mat=cbind(indicator_mat, QMR_zone_slope_mean)

TL_zone_slope_mean=raw_landscape_data2$zone_slope_meanTL
QTL_zone_slope_mean=quant_levels(TL_zone_slope_mean)
indicator_mat=cbind(indicator_mat, TL_zone_slope_mean)
indicator_mat=cbind(indicator_mat, QTL_zone_slope_mean)

MG_zone_slope_mean=raw_landscape_data2$zone_slope_meanMG
QMG_zone_slope_mean=quant_levels(MG_zone_slope_mean)
indicator_mat=cbind(indicator_mat, MG_zone_slope_mean)
indicator_mat=cbind(indicator_mat, QMG_zone_slope_mean)

#zone_slope_std
MR_slope_std=raw_landscape_data2$zone_slope_stdMR
QMR_slope_std=quant_levels(MR_slope_std)
indicator_mat=cbind(indicator_mat, MR_slope_std)
indicator_mat=cbind(indicator_mat, QMR_slope_std)

TL_slope_std=raw_landscape_data2$zone_slope_stdTL
QTL_slope_std=quant_levels(TL_slope_std)
indicator_mat=cbind(indicator_mat, TL_slope_std)
indicator_mat=cbind(indicator_mat, QTL_slope_std)

MG_slope_std=raw_landscape_data2$zone_slope_stdMG
QMG_slope_std=quant_levels(MG_slope_std)
indicator_mat=cbind(indicator_mat, MG_slope_std)
indicator_mat=cbind(indicator_mat, QMG_slope_std)

#zone_aspect_std
MR_aspect_std=raw_landscape_data2$zone_aspect_stdMR
QMR_aspect_std=quant_levels(MR_aspect_std)
indicator_mat=cbind(indicator_mat, MR_aspect_std)
indicator_mat=cbind(indicator_mat, QMR_aspect_std)

TL_aspect_std=raw_landscape_data2$zone_aspect_stdTL
QTL_aspect_std=quant_levels(TL_aspect_std)
indicator_mat=cbind(indicator_mat, TL_aspect_std)
indicator_mat=cbind(indicator_mat, QTL_aspect_std)

MG_aspect_std=raw_landscape_data2$zone_aspect_stdMG
QMG_aspect_std=quant_levels(MG_aspect_std)
indicator_mat=cbind(indicator_mat, MG_aspect_std)
indicator_mat=cbind(indicator_mat, QMG_aspect_std)

#CO
indicator_mat=cbind(indicator_mat, raw_landscape_data2$pct_CO_in_TL)



write.table(indicator_mat,outname,sep=",",row.names=T, col.names=T)








