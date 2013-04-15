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
write.csv(most_vuln_spp_selection, paste0("tables/", name,".csv"), row.names=FALSE)
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
write.csv(most_vuln_spp_selection, paste0("tables/", name,".csv"), row.names=FALSE)

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
write.csv(most_vuln_spp_selection, paste0("tables/", name,".csv"), row.names=FALSE)

#all no-overlap spp 
name="no_overlap_spp"
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "winkout","CE_overlap", "Status.simplified")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$Status.simplified!="Extinct",]
most_vuln_spp=most_vuln_spp[most_vuln_spp$winkout==0,]
most_vuln_spp=most_vuln_spp[most_vuln_spp$CE_overlap==0,c("spp","FAMILY","transformed")]
#most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
most_vuln_spp_selection=most_vuln_spp
write.csv(most_vuln_spp_selection, paste0("tables/", name,".csv"), row.names=FALSE)


#MOST VULNERABLE NON-LISTED SPP
name="most_vuln_not_listed_spp"
rows=20
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "Status.simplified")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$Status.simplified=="Apparently Secure",c("spp","FAMILY","transformed")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability")
n=dim(most_vuln_spp)[1]
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,])
write.csv(most_vuln_spp_selection, paste0("tables/", name,".csv"), row.names=FALSE)

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
write.csv(family_table_selection, paste0("tables/", name,".csv"), row.names=FALSE)

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
write.csv(genus_table_selection, paste0("tables/", name,".csv"), row.names=FALSE)


