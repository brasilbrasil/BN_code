server=FALSE
if (server){
  wd <- "//10.0.0.5/data2$//PICCC_analysis/BN_vulnerability/Full Process template/"
  code_loc="Y:/code/BN_code/"
}else{
  wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/BN vulnerability/Full Process template/" #C:\Users\lfortini\Dropbox\USGS\Science\0-ongoing\VAs\HI spp VA\BN vulnerability\Full Process template
  code_loc="C:/Users/lfortini/Dropbox/code/BN_code/"
}
setwd(wd)

runs=c("unknwnfacs_eqwgts_priors_thirddispersion","unknwnfacs_eqwgts_priors_thirddispersion_max_habqual", "unknwnfacs_eqwgts_priors_thirddispersion_min_habqual")  #_min_habqual
run_type=c("standard", "max_hab_qual", "min_hab_qual")
vars=c("transformed")

var=vars[1]
for (var in vars){
  i=1
  run=runs[1]
  for (run in runs){
    all_combined_file=paste("results/",run,"_all_combined.csv", sep="")
    all_combined=read.csv(all_combined_file, stringsAsFactors=FALSE)
    if (run==runs[1]){
      temp_var=as.data.frame(all_combined[,c("spp", "sp_code", "FAMILY")])
      names(temp_var)=c("Species", "code", "FAMILY")
    }
    
    jnk=as.data.frame(all_combined[,var])
    names(jnk)=run_type[i]
    
    temp_var=cbind(temp_var, jnk) 
    i=i+1
  }
  temp_var=cbind(temp_var, dmaxHabqual=temp_var[,"standard"]-temp_var[,"max_hab_qual"])
  temp_var=cbind(temp_var, dminHabqual=temp_var[,"min_hab_qual"]-temp_var[,"standard"])
  temp_var=cbind(temp_var, propdminHabqual=temp_var[,"dminHabqual"]/temp_var[,"standard"])
  temp_var=cbind(temp_var, propdmaxHabqual=temp_var[,"dmaxHabqual"]/temp_var[,"standard"])
  assign(var,temp_var)
}
write.csv(temp_var, paste0("results/min_max_hab_qual_table.csv"), row.names=FALSE)
