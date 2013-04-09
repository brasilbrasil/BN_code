csv_data=uncat.data.file
variables.file="variables.csv"

## load un-categorized data
all_rld <- read.csv(csv_data, stringsAsFactors=FALSE)
#all_combined=all_combined[,-first_and_last_discr_var]

## load the list of variables and corresponding nodes
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)
ttemp_vars=unique(vlist$graph_name)
ttemp_zones=unique(vlist$graph_legend)
res_table=as.data.frame(matrix(0, length(ttemp_vars), length(ttemp_zones)))
names(res_table)=ttemp_zones
rownames(res_table)=ttemp_vars
mean_res_table=res_table
sd_res_table=res_table

ttemp_var=ttemp_vars[1]
for (ttemp_var in ttemp_vars){
  if (vlist$table_vars[min(which(vlist$graph_name==ttemp_var))]){
    sub_ttemp_vars=vlist$Variable[vlist$graph_name==ttemp_var]
    sub_ttemp_var=sub_ttemp_vars[1]
    for (sub_ttemp_var in sub_ttemp_vars){      
      tpzone=vlist$graph_legend[which(vlist$Variable==sub_ttemp_var)]
      tpmean=mean(all_rld[,sub_ttemp_var],na.rm=TRUE)
      tpsd=sd(all_rld[,sub_ttemp_var],na.rm=TRUE)
      indC=which(names(mean_res_table)==tpzone)
      indR=which(ttemp_vars==ttemp_var)
      mean_res_table[indR,indC]=tpmean
      sd_res_table[indR,indC]=tpsd  
    }            
  }
}
#mean_res_table=mean_res_table[vlist$table_vars==TRUE,]
#sd_res_table=sd_res_table[vlist$table_vars==TRUE,]
write.csv(mean_res_table, paste(project_name, "_factor_table_means.csv",sep=""), row.names=TRUE)
write.csv(sd_res_table, paste(project_name, "_factor_table_sds.csv",sep=""), row.names=TRUE)


