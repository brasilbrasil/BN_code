server=TRUE
if (server){
  wd <- "//10.0.0.5/data2$//BN vulnerability/Full Process/"
}else{
  wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/"}
setwd(wd)

cat.data.file <- "qrld.csv"
data.file <- "rld.csv"
aux_data_file="//10.0.0.5/data2$//VA data/CAO/spp_habitat_requirements.csv"
veg_file="dominant_veg_classes.csv"
all_vulnerabilities_file="results/selected_spp_vulnerability_scores.csv"
all_combined_file="results/all_combined.csv"

data=read.csv(data.file, stringsAsFactors=FALSE)
cat_data=read.csv(cat.data.file, stringsAsFactors=FALSE)
jnkk=names(cat_data)
jnkk1=c()
for (jnk in jnkk){
  jnk=paste("node_",jnk, sep="")
  jnkk1=c(jnkk1, jnk)
}
names(cat_data)=jnkk1
aux_data=read.csv(aux_data_file, stringsAsFactors=FALSE)
vul_data=read.csv(all_vulnerabilities_file, stringsAsFactors=FALSE)
veg_data=read.csv(veg_file, stringsAsFactors=FALSE)


all_species=vul_data$X

#sp=all_species[1]
#sp="Vigna owahuensis"
i=1
for (sp in all_species){
  jnk0=c()
  jnk=which(aux_data$spp==sp)
  jnk=aux_data[jnk,]
  jnk0=jnk
  
  jnk=which(veg_data$sp_name==sp)
  jnk=veg_data[jnk,]
  jnk0=cbind(jnk0, jnk)
  
  jnk=which(data$sp_name==sp)
  jnk=data[jnk,]
  jnk0=cbind(jnk0, jnk)
  
  jnk=which(cat_data$node_sp_name==sp)
  jnk=cat_data[jnk,]
  jnk0=cbind(jnk0, jnk)
  
  jnk=which(vul_data$X==sp)
  jnk=vul_data[jnk,]
  jnk0=cbind(jnk0, jnk)
  
  if (i==1){
    all_combined=jnk0    
  }else{
    all_combined=rbind(all_combined,jnk0)
  }
  i=i+1
}
write.csv(all_combined, all_combined_file, row.names=TRUE)