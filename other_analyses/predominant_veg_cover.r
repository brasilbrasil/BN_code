rm(list=ls())

## working directory
server=TRUE
if (server){
  wd <- "//10.0.0.5/data2$//BN vulnerability/Full Process/"
}else{
  wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/"}
setwd(wd)
cover_types=c('prop_Alpine_shrubland', 'prop_Decidious_shrubland', 'prop_Dry_forest', 'prop_Dry_grassland', 'prop_Dry_shrubland', 'prop_Evergreen_shrubland', 'prop_Mesic_forest', 'prop_Mesic_grassland', 'prop_Mesic_shrubland', 'prop_Perennial_grassland', 'prop_Wet_forest', 'prop_Wet_mesic_forest', 'prop_Wetland_coastal')
cover_names=c('Alpine_shrubland', 'Decidious_shrubland', 'Dry_forest', 'Dry_grassland', 'Dry_shrubland', 'Evergreen_shrubland', 'Mesic_forest', 'Mesic_grassland', 'Mesic_shrubland', 'Perennial_grassland', 'Wet_forest', 'Wet_mesic_forest', 'Wetland_coastal')
main_cover_types=c('main_Alpine_shrubland', 'main_Decidious_shrubland', 'main_Dry_forest', 'main_Dry_grassland', 'main_Dry_shrubland', 'main_Evergreen_shrubland', 'main_Mesic_forest', 'main_Mesic_grassland', 'main_Mesic_shrubland', 'main_Perennial_grassland', 'main_Wet_forest', 'main_Wet_mesic_forest', 'main_Wetland_coastal')
cover_ranks=c("cover1","cover2","cover3")

veg_cover_data=(read.csv("all_spp_values_veg_analysis_simple.csv",header=T, stringsAsFactors=F))
#veg_cover_data=(read.csv("all_spp_values_veg_analysis_simple.csv",header=T, stringsAsFactors=F, row.names=1))
new_data=as.data.frame(matrix(0,dim(veg_cover_data)[1],length(main_cover_types)+length(cover_ranks)+1))
names(new_data)=c("dominant_cover", cover_ranks, main_cover_types)

spp=veg_cover_data$sp_name

total_cover=c(216537.7411, 2.855661412, 1144.798039, 1210.851951, 50.12908027, 822.3068568, 61.56671135, 1202.571563, 15.18773547, 81.04683654, 2861.289378, 2888.269712, 1289.20141, 37.67243981)

sp=spp[1]

for (sp in spp){
  sp_veg_cover_data=veg_cover_data[veg_cover_data$sp_name==sp,]
  prop_sp_veg_cover_data=as.matrix(as.double(sp_veg_cover_data[,cover_types]))
  jnk=order(prop_sp_veg_cover_data, decreasing = TRUE)
  dominant_cover='na'
  dominant_index=which(prop_sp_veg_cover_data>0.5)
  if (length(dominant_index)>0){
    dominant_cover=cover_names[dominant_index]
  }
  
  main_covers=cover_names[jnk]
  main_covers=main_covers[1:3]
  #jnk[jnk>3]=0
  prop_sp_veg_cover_data=jnk
  new_data[veg_cover_data$sp_name==sp,"dominant_cover"]=dominant_cover
  new_data[veg_cover_data$sp_name==sp,cover_ranks]=main_covers
  new_data[veg_cover_data$sp_name==sp,main_cover_types[jnk[1:3]]]=c(1,2,3)
}

results=cbind(veg_cover_data,new_data)
write.csv(results, "dominant_veg_classes.csv", row.names=FALSE)