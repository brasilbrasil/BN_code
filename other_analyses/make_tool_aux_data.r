csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)
variables.file="variables.csv"
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)

Abs_q5levels <- function(x, q=c(0.2, 0.4, 0.6, 0.8), lv=c("Very_small", "Small", "Medium", "Large", "Very_large")) {
  quant_data <- rep(lv[3], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < rep(q[2], length(x))] <- lv[2]
  quant_data[x < rep(q[1], length(x))] <- lv[1]
  quant_data[x > rep(q[3], length(x))] <- lv[4]
  quant_data[x > rep(q[4], length(x))] <- lv[5]    
  return(quant_data)}


header_cols=c('spp', 'sp_code', 'DIVISION', 'FAMILY', 'GENUS', 'Status.simplified', 'Native.Status', 'Coastal', 'Ha', 'Ma', 'Ka', 'Oa', 'Mo', 'La', 'Ke', 'Ni', 'dominant_cover', 'cover1', 'cover2', 'cover3', 'CCE_Area', 'transformed', 'Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
header_cols_names=c('spp', 'sp_code', 'Division', 'Family', 'Genus', 'Consv_stat', 'Native_stat', 'Coastal', 'Ha_pres', 'Ma_pres', 'Ka_pres', 'Oa_pres', 'Mo_pres', 'La_pres', 'Ke_pres', 'Ni_pres', 'Cover_D', 'Cover_1', 'Cover_2', 'Cover_3', 'CCE_Area', 'Vulnerability', 'Tolerate', 'Migrate', 'Micro_refugia', 'Eff_Mrf_A', 'Eff_Tol_A', 'Eff_Mig_A', 'Habqual_Mrg', 'Habqual_Tol', 'Habqual_Mig', 'Dispersion')
vlist=vlist[which(vlist$tool_order!="NA"),]
vlist=vlist[order(vlist$tool_order),]
node_names=vlist$Tool_name_short
node_cols=vlist$Node
jnkk=node_cols
jnkk1=c()
for (jnk in jnkk){
  jnk=paste0("node_",jnk)
  jnkk1=c(jnkk1, jnk)
}
node_cols=jnkk1

aux_data0=all_combined[,header_cols]

vars=c('Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
for (var in vars){
  jnk=aux_data0[,var]
  #jnk_med=median(jnk)
  #cat("median for ",var, " is ",jnk_med, "\n")   
  #jnk <- ifelse(jnk > jnk_med, "H.", "L.")
  jnk=Abs_q5levels(jnk)
  aux_data0[,var]=jnk
  #jnk=dim(data_resps_discr)[2]
  #names(data_resps_discr)[jnk]=paste0(var, "_cat")
}
names(aux_data0)=c(header_cols_names)

##
aux_data1=all_combined[,node_cols]
names(aux_data1)=c(node_names)

##average_condition
hab_area=c('Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area')
hab_qual=c('Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual')
#headers=c('spp',  'sp_code',  'shpcode', 'DIVISION', 'FAMILY', 'GENUS','SPECIES')

##qual, area and distr
#data_resps=all_combined[,headers]
jnk=rowMeans(all_combined[,hab_qual])
jnk1=rowMeans(all_combined[,hab_area])
data_resps=cbind(Quality=jnk,Area=jnk1)
data_resps=as.data.frame(data_resps)
vars=c("Quality","Area")
data_resps_discr=data_resps
var=vars[1]

for (var in vars){
  jnk=data_resps[,var]
  jnk_med=median(jnk)
  jnk=Abs_q5levels(jnk)
  data_resps_discr[,var]=jnk
  jnk=which(names(data_resps_discr)==var)
  names(data_resps_discr)[jnk]=paste0(var, "_avg")
}

#node_cols %in% names(all_combined) 
aux_data=cbind(aux_data0, data_resps_discr, aux_data1)
#names(aux_data)=c(header_cols_names, node_names)
isl_col=c('Ha_pres', 'Ma_pres', 'Ka_pres', 'Oa_pres', 'Mo_pres', 'La_pres', 'Ke_pres', 'Ni_pres')
jnk=aux_data[, isl_col]
jnk[jnk>0]=1
aux_data[, isl_col]=jnk
write.csv(aux_data, paste("results/",  project_name, "_tool_aux_data.csv", sep=""), row.names=TRUE)
