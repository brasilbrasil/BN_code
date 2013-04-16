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

#MOST/LEAST VULNERABLE SPP
name="most_least_vuln_spp"
rows=10
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
n=dim(most_vuln_spp)[1]
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,],c("...","...","..."),most_vuln_spp[(n-rows):(n),])
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
write.csv(genus_table_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#Example species
name="example_species_values"
rows=10
example_spp=c(664, 502, 134)
most_vuln_spp=all_combined[all_combined$sp_code %in% example_spp,]
nm_most_vuln_spp=most_vuln_spp$spp
to_show0=vlist[vlist$ET_var==1,c("Variable", "ET_type", "ET_order", "graph_name", "graph_legend")]
to_show1=vlist[vlist$ET_var==1,c("Node", "ET_type", "ET_order", "graph_name", "graph_legend")]
to_show1$Node=paste0("node_", to_show1$Node)
to_show0=cbind(to_show0,Value=rep("Value",dim(to_show0)[1]))
to_show1=cbind(to_show1,Value=rep("State",dim(to_show1)[1]))
names(to_show1)[1]="Variable"
to_show=rbind(to_show0, to_show1)
to_show=to_show[order(to_show$ET_order),]
names(to_show)=c("Variable","Category","ET_order", "Factor", "Zone", "Type")
example_spp=cbind(to_show,t(most_vuln_spp[,to_show$Variable]))

cols=dim(example_spp)[2]
names(example_spp)[(cols-length(nm_most_vuln_spp)+1):cols]=nm_most_vuln_spp
example_spp=example_spp[,c("Factor", "Zone", "Category", "Type", nm_most_vuln_spp)]
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
write.csv(family_table_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)
