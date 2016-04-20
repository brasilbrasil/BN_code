csv_data=uncat.data.file
variables.file="variables.csv"
factor_order_table=c('Overlap between current and future suitable range', 'Projected change in CE area', 'Envelope overlap', 'Winkout', 'Proximity to max height of islands', 'Fraction of occurence points in tol zone', 'Number of future compatible bioregions', 'Area lost to SLR ', 'Lava flow area ', 'Ugly habitat ', 'Effective zone area ', 'Pioneer effective zone area ', 'Total area ', 'Area under Protective designation ', 'Fragmentation ', 'Native cover', 'Ungulate exclusion areas ', 'Average invasive suitability', 'Average ppt gradient', 'Aspect variability', 'Average slope', 'Slope variability', 'Distance between current and future suitable range', 'Persistence in invaded landscape', 'Pioneer species')


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
  if (vlist$table_vars[min(which(vlist$graph_name==ttemp_var))]){ #should the variable should be included in table?
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
table_vars=vlist$graph_name[vlist$table_vars==TRUE]
jnk=rownames(mean_res_table)
jnk=jnk %in% table_vars
mean_res_table=mean_res_table[jnk,]
sd_res_table=sd_res_table[jnk,]
names(mean_res_table)=c("Mean migration zone","Mean micro refugia zone", "Mean tolerate zone", "Mean all zones")
names(sd_res_table)=c("S.d. migration zone","S.d. micro refugia zone", "S.d. tolerate zone", "S.d. all zones")
mean_res_table=mean_res_table[,c(2,3,1,4)]
sd_res_table=sd_res_table[,c(2,3,1,4)]
all=cbind(mean_res_table, sd_res_table)
all=all[factor_order_table,c(1,5,2,6,3,7,4,8)]
write.csv(all, paste("tables/", project_name, "_factor_table.csv",sep=""), row.names=TRUE)

