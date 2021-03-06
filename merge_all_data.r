sp_to_remove=c("Gahnia lanaiensis")
cat.data.file <- paste(project_name, "_qrld.csv",sep="")
uncat.data.file <- paste(project_name, "_rld.csv",sep="")
aux_data_file=paste0(code_loc,"spp_habitat_requirements_new_taxonomy.csv")

veg_file="dominant_veg_classes.csv"
all_vulnerabilities_file=paste("results/",project_name,"_spp_vulnerability_scores.csv", sep="")
all_combined_file=paste("results/",project_name,"_all_combined.csv", sep="")

data=read.csv(uncat.data.file, stringsAsFactors=FALSE)
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
  jnk=which(aux_data$spp_old==sp)
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

#island endemics
islands=c("Ha", "Ma", "Ka", "Oa", "Mo", "La", "Ke", "Ni")
jnk=all_combined[,islands]
jnk[jnk>0]=1
jnk=rowSums(jnk)
jnk0=matrix("No",dim(all_combined)[1],1)
jnk0[jnk==1]="Yes"
all_combined=cbind(all_combined, island_endemic=jnk0)

jnk=all_combined$transformed
jnk0=rank(-jnk)
jnk1=max(jnk0)
rel_rank_vul=jnk0/jnk1
rank_cat_vuln=ceiling(rel_rank_vul*10)*10

all_combined=cbind(all_combined, rel_rank_vul, rank_cat_vuln)

# island=islands[1]
# i=1
# for (island in islands){
#   jnk=temp_data[,i]
#   jnk[jnk>0]=island
#   jnk[jnk<=0]="na"
#   temp_data[,i]=jnk
#   i=i+1  
# }

if (length(sp_to_remove)>0){
  jnk=all_combined$spp %in% sp_to_remove
  jnk=!jnk
  all_combined=all_combined[jnk,]
}
write.csv(all_combined, all_combined_file, row.names=TRUE)