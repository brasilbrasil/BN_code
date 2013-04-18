#first_and_last_discr_var=c(85:169)
csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
variables.file="variables.csv"
dir.create("tables/",showWarnings=FALSE)

require(xtable)
## load un-categorized data
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)
#all_combined=all_combined[,-first_and_last_discr_var]

## load the list of variables and corresponding nodes
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)

#signif function
frame_rounding=function(frame){
  ii <- sapply(frame, is.factor)
  frame[ii] <- lapply(frame[ii], as.character)  
  for (i in 1:dim(frame)[1]){
    for (j in 1:dim(frame)[2]){
      temp=frame[i,j]
      if (suppressWarnings(!is.na(as.numeric(temp)))){
        frame[i,j]=signif(as.numeric(temp),3)
        #frame[i,j]=round(as.numeric(temp),3)  
      }
    }
  }
  return(frame)
}  

#MOST/LEAST VULNERABLE SPP
name="most_least_vuln_spp"
rows=10
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
n=dim(most_vuln_spp)[1]
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,],c("...","...","..."),most_vuln_spp[(n-rows):(n),])
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)
# fileConn<-file("tables/HTML_table.html")
# writeLines(print(xtable(most_vuln_spp_selection), type="html"), fileConn)
# close(fileConn)
# fileConn<-file("tables/HTML_table.txt")
# writeLines(print(xtable(most_vuln_spp_selection), type="latex"), fileConn)
# close(fileConn)

#MOST VULNERABLE SPP WITH FCES
name="most_least_vuln_spp_w_FCEs"
rows=20
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "winkout")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$winkout==0,c("spp","FAMILY","transformed")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
n=dim(most_vuln_spp)[1]
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,])
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#print(xtable(most_vuln_spp_selection), type="html")
#print(xtable(most_vuln_spp_selection), floating=FALSE)

#all Winkout spp 
name="winkout_spp"
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "winkout", "Status.simplified")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$Status.simplified!="Extinct",]
most_vuln_spp=most_vuln_spp[most_vuln_spp$winkout==1,c("spp","FAMILY","transformed")]
#most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
most_vuln_spp_selection=most_vuln_spp
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#all no-overlap spp 
name="no_overlap_spp"
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "winkout","CE_overlap", "Status.simplified")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$Status.simplified!="Extinct",]
most_vuln_spp=most_vuln_spp[most_vuln_spp$winkout==0,]
most_vuln_spp=most_vuln_spp[most_vuln_spp$CE_overlap==0,c("spp","FAMILY","transformed")]
#most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
most_vuln_spp_selection=most_vuln_spp
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)


#MOST VULNERABLE NON-LISTED SPP
name="most_vuln_not_listed_spp"
rows=20
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "Status.simplified")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$Status.simplified=="Apparently Secure",c("spp","FAMILY","transformed")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
n=dim(most_vuln_spp)[1]
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,])
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

library(reshape2)
library(plyr)

min_n=5
#least/most vulnerable families
name="most_least_vuln_families"
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed")]
n=dim(most_vuln_spp)[1]
most_vuln_spp=cbind(most_vuln_spp,Temp=rep(1,n,1), Count=rep(1,n,1))
group_n=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="Count", sum)
group_mean=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="transformed", mean)
group_sd=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="transformed", sd)
family_table=cbind(group_n, group_mean[,2],group_sd[,2])
names(family_table)=c("Family","n","Mean","S.d.")
family_table[is.na(family_table)]="-"
family_table=family_table[order(family_table$Mean),]
family_table=family_table[family_table$n>=min_n,]
rows=10
n=dim(family_table)[1]
if (rows*2<n){
  family_table_selection=rbind(family_table[1:rows,],c("...","...","...", "..."),family_table[(n-rows):(n),])  
}else{
  family_table_selection=family_table
}
family_table_selection=frame_rounding(family_table_selection)
write.csv(family_table_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#least/most vulnerable genera
name="most_least_vuln_genera"
most_vuln_spp=all_combined[,c("spp","GENUS","transformed")]
n=dim(most_vuln_spp)[1]
most_vuln_spp=cbind(most_vuln_spp,Temp=rep(1,n,1), Count=rep(1,n,1))
group_n=dcast(most_vuln_spp,  GENUS  ~  Temp,  value.var="Count", sum)
group_mean=dcast(most_vuln_spp,  GENUS  ~  Temp,  value.var="transformed", mean)
group_sd=dcast(most_vuln_spp,  GENUS  ~  Temp,  value.var="transformed", sd)
genus_table=cbind(group_n, group_mean[,2],group_sd[,2])
names(genus_table)=c("Genus","n","Mean","S.d.")
genus_table[is.na(genus_table)]="-"
genus_table=genus_table[order(genus_table$Mean),]
genus_table=genus_table[genus_table$n>=min_n,]
rows=10
n=dim(genus_table)[1]
if (rows*2<n){
  genus_table_selection=rbind(genus_table[1:rows,],c("...","...","...", "..."),genus_table[(n-rows):(n),])  
}else{
  genus_table_selection=genus_table
}
genus_table_selection=frame_rounding(genus_table_selection)
write.csv(genus_table_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#Example species
name="example_species_values"
rows=10
example_spp=c(664, 502, 134)
most_vuln_spp=all_combined[all_combined$sp_code %in% example_spp,]
nm_most_vuln_spp=most_vuln_spp$spp

top=c("FAMILY", "transformed", "Micro_refugia","Resist","Migrate")
header=c("Factor", "Zone", "Category", "Type")#, nm_most_vuln_spp)
side=as.data.frame(matrix(c(""),length(top),length(header)))
names(side)=header
nm=c("Family", "Vulnerability", "Micro refugia","Tolerate","Migrate")
side[,"Factor"]=nm

top1=as.data.frame(t(most_vuln_spp[,top]))
names(top1)=nm_most_vuln_spp
top=cbind(side,top1)

to_show0=vlist[vlist$ET_var==1,c("Variable", "ET_type", "ET_order", "graph_name", "graph_legend", "ET_zone", "ET_name")]
to_show1=vlist[vlist$ET_node==1,c("Node", "ET_type", "ET_order", "graph_name", "graph_legend",  "ET_zone", "ET_name")]
to_show1$Node=paste0("node_", to_show1$Node)
to_show0=cbind(to_show0,Value=rep("Value",dim(to_show0)[1]))
to_show1=cbind(to_show1,Value=rep("State",dim(to_show1)[1]))
names(to_show1)[1]="Variable"
to_show=rbind(to_show0, to_show1)
to_show=to_show[order(to_show$ET_order),]
names(to_show)=c("Variable0","Category","ET_order", "Factor0", "Zone0", "Zone","Factor", "Type")
example_spp=cbind(to_show,t(most_vuln_spp[,to_show$Variable]))

cols=dim(example_spp)[2]
names(example_spp)[(cols-length(nm_most_vuln_spp)+1):cols]=nm_most_vuln_spp
example_spp=example_spp[,c("Factor", "Zone", "Category", "Type", nm_most_vuln_spp)]
example_spp=rbind(top, example_spp)
example_spp=frame_rounding(example_spp)
write.csv(example_spp, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)


#spp most benefitting from hab qual increases
name="spp_w_largest_decrease_in_vuln_with_hab_qual_increase"
rows=20
csv_data="results/min_max_hab_qual_table.csv"
min_max_hab_qual_table=read.csv(csv_data, stringsAsFactors=FALSE)
most_vuln_spp=min_max_hab_qual_table[,c("Species","FAMILY","standard", "propdmaxHabqual")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$standard, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "Decrease in vulnerability")
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,])
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#spp most harmed by  hab qual decreases
name="spp_w_largest_increase_in_vuln_with_hab_qual_decrease"
rows=20
csv_data="results/min_max_hab_qual_table.csv"
min_max_hab_qual_table=read.csv(csv_data, stringsAsFactors=FALSE)
most_vuln_spp=min_max_hab_qual_table[,c("Species","FAMILY","standard", "propdminHabqual")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$standard, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "Increase in vulnerability")
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,])
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)


#families most benefitting from hab qual increases
name="families_w_largest_decrease_in_vuln_with_hab_qual_increase"
rows=20
csv_data="results/min_max_hab_qual_table.csv"
min_max_hab_qual_table=read.csv(csv_data, stringsAsFactors=FALSE)
most_vuln_spp=min_max_hab_qual_table[,c("Species","FAMILY","standard", "propdmaxHabqual")]
n=dim(most_vuln_spp)[1]
most_vuln_spp=cbind(most_vuln_spp,Temp=rep(1,n,1), Count=rep(1,n,1))
group_n=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="Count", sum)
group_mean=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="standard", mean)
group_sd=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="propdmaxHabqual", mean)
family_table=cbind(group_n, group_mean[,2],group_sd[,2])
names(family_table)=c("Family","n","Mean","Change")
family_table[is.na(family_table)]="-"
family_table=family_table[order(family_table$Change,decreasing=TRUE),]
family_table=family_table[family_table$n>=min_n,]
rows=20
family_table_selection=family_table[1:rows,] 
family_table_selection=frame_rounding(family_table_selection)
write.csv(family_table_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#families most harmed by hab qual decreases
name="families_w_largest_increase_in_vuln_with_hab_qual_decrease"
rows=20
csv_data="results/min_max_hab_qual_table.csv"
min_max_hab_qual_table=read.csv(csv_data, stringsAsFactors=FALSE)
most_vuln_spp=min_max_hab_qual_table[,c("Species","FAMILY","standard", "propdminHabqual")]
n=dim(most_vuln_spp)[1]
most_vuln_spp=cbind(most_vuln_spp,Temp=rep(1,n,1), Count=rep(1,n,1))
group_n=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="Count", sum)
group_mean=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="standard", mean)
group_sd=dcast(most_vuln_spp,  FAMILY  ~  Temp,  value.var="propdminHabqual", mean)
family_table=cbind(group_n, group_mean[,2],group_sd[,2])
names(family_table)=c("Family","n","Mean","Change")
family_table[is.na(family_table)]="-"
family_table=family_table[order(family_table$Change,decreasing=TRUE),]
family_table=family_table[family_table$n>=min_n,]
rows=20
family_table_selection=family_table[1:rows,] 
family_table_selection=frame_rounding(family_table_selection)
write.csv(family_table_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)


####make factor means table
csv_data=uncat.data.file
variables.file="variables.csv"
factor_order_table=c('Overlap between current and future suitable range', 'Projected change in CE area', 'Proximity to max height of islands', 'Fraction of occurence points in tol zone', 'Number of future compatible bioregions', 'Area lost to SLR ', 'Lava flow area ', 'Ugly habitat ', 'Effective zone area ', 'Pioneer effective zone area ', 'Total area ', 'Area under Protective designation ', 'Fragmentation ', 'Native cover', 'Ungulate exclusion areas ', 'Average invasive suitability', 'Average ppt gradient', 'Aspect variability', 'Average slope', 'Slope variability', 'Distance between current and future suitable range', 'Envelope overlap', 'Winkout', 'Persistence in invaded landscape', 'Pioneer species')

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
temp_index=c()
fct=factor_order_table[1]
for (fct in factor_order_table){
  jnk0=which(vlist$graph_name==fct)
  if (length(jnk0>1)) jnk0=jnk0[1]
  jnk=vlist$ET_name[jnk0]
  temp_index=c(temp_index,jnk)
}
#factor_order_table=vlist$ET_name[vlist$graph_name %in% factor_order_table]
all=all[factor_order_table,c(1,5,2,6,3,7,4,8)]
all[c('Envelope overlap', 'Winkout', 'Persistence in invaded landscape', 'Pioneer species'),"S.d. all zones"]=""

rownames(all)=temp_index
all=frame_rounding(all)
all[is.na(all)]=""
all[all==0]=""
write.csv(all, paste("tables/", project_name, "_factor_table.csv",sep=""), row.names=TRUE)
