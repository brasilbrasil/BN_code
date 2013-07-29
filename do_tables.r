#first_and_last_discr_var=c(85:169)
csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
variables.file="variables.csv"
dir.create("tables/",showWarnings=FALSE)

require(xtable)
library(reshape2)
library(plyr)
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
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "Status.simplified")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "Conservation status")
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
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "winkout", "Status.simplified")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$winkout==0,c("spp","FAMILY","transformed", "Status.simplified")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "Conservation status")
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
most_vuln_spp=most_vuln_spp[most_vuln_spp$winkout==1,c("spp","FAMILY","transformed", "Status.simplified")]
#most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "Conservation status")
most_vuln_spp_selection=most_vuln_spp
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#all no-overlap spp 
name="no_overlap_spp"
most_vuln_spp=all_combined[,c("spp","FAMILY","transformed", "winkout","CE_overlap", "Status.simplified")]
most_vuln_spp=most_vuln_spp[most_vuln_spp$Status.simplified!="Extinct",]
most_vuln_spp=most_vuln_spp[most_vuln_spp$winkout==0,]
most_vuln_spp=most_vuln_spp[most_vuln_spp$CE_overlap==0,c("spp","FAMILY","transformed", "Status.simplified")]
#most_vuln_spp=most_vuln_spp[order(most_vuln_spp$transformed, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "Conservation status")
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
family_table=family_table[order(family_table$Mean, decreasing = TRUE),]
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
genus_table=genus_table[order(genus_table$Mean, decreasing = TRUE),]
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


#
csv_data="results/min_max_hab_qual_table.csv"
min_max_table=read.csv(csv_data, stringsAsFactors=FALSE)
n=dim(min_max_table)[1]
min_max_table=cbind(min_max_table,Temp=rep(1,n,1), Count=rep(1,n,1))
median_standard_vul=median(min_max_table$standard)
cat("median vulnerability score is", "\n")
median_standard_vul
high_standard=sum(min_max_table[min_max_table$standard>median_standard_vul, "Count"])
high_maxQual=sum(min_max_table[min_max_table$max_hab_qual>median_standard_vul, "Count"])
high_minQual=sum(min_max_table[min_max_table$min_hab_qual>median_standard_vul, "Count"])


#spp most benefitting from hab qual increases
name="spp_w_largest_decrease_in_vuln_with_hab_qual_increase"
rows=20
csv_data="results/min_max_hab_qual_table.csv"
min_max_hab_qual_table=read.csv(csv_data, stringsAsFactors=FALSE)
median_standard_vul=median(min_max_hab_qual_table$standard)
most_vuln_spp=min_max_hab_qual_table[,c("Species","FAMILY","standard", "propdmaxHabqual")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$propdmaxHabqual, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "% decrease in vulnerability")
most_vuln_spp_selection=rbind(most_vuln_spp[1:rows,])
most_vuln_spp_selection=frame_rounding(most_vuln_spp_selection)
write.csv(most_vuln_spp_selection, paste0("tables/", project_name, "_", name,".csv"), row.names=FALSE)

#spp most harmed by  hab qual decreases
name="spp_w_largest_increase_in_vuln_with_hab_qual_decrease"
rows=20
csv_data="results/min_max_hab_qual_table.csv"
min_max_hab_qual_table=read.csv(csv_data, stringsAsFactors=FALSE)
most_vuln_spp=min_max_hab_qual_table[,c("Species","FAMILY","standard", "propdminHabqual")]
most_vuln_spp=most_vuln_spp[order(most_vuln_spp$propdminHabqual, decreasing = TRUE),]
names(most_vuln_spp)=c("Species","Family", "Vulnerability", "% increase in vulnerability")
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
names(family_table)=c("Family","n","Mean","% Change")
family_table[is.na(family_table)]="-"
family_table=family_table[order(family_table$"% Change",decreasing=TRUE),]
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
names(family_table)=c("Family","n","Mean","% Change")
family_table[is.na(family_table)]="-"
family_table=family_table[order(family_table$"% Change",decreasing=TRUE),]
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
all=all[factor_order_table,c(1,5,2,6,3,7,4,8)]
all[c('Envelope overlap', 'Winkout', 'Persistence in invaded landscape', 'Pioneer species'),"S.d. all zones"]=""
rownames(all)=temp_index
all=frame_rounding(all)
all[is.na(all)]=""
all[all==0]=""
write.csv(all, paste("tables/", project_name, "_factor_table.csv",sep=""), row.names=TRUE)


##vulnerability of listed endemic dicots vs all else
most_vuln_spp=all_combined#[,c("spp","GENUS","transformed")]
most_vuln_spp=cbind(most_vuln_spp,Temp=rep(1,dim(most_vuln_spp)[1]), 
                    Count=rep(1,dim(most_vuln_spp)[1]),selected=rep(0,dim(most_vuln_spp)[1]))
most_vuln_spp[most_vuln_spp$Status.simplified!="Apparently Secure" & 
                most_vuln_spp$Native.Status=="Endemic" &
                most_vuln_spp$DIVISION=="Dicot", "selected"]=1
group_n=dcast(most_vuln_spp,  selected  ~  Temp,  value.var="Count", sum)
group_mean=dcast(most_vuln_spp,  selected  ~  Temp,  value.var="transformed", mean)
group_sd=dcast(most_vuln_spp,  selected  ~  Temp,  value.var="transformed", sd)

##vulnerability of listed single island endemic dicots vs all else
most_vuln_spp=all_combined#[,c("spp","GENUS","transformed")]
island=c("Ha","Ma", "Ka", "Oa", "Mo", "La", "Ke", "Ni")
island_vals=all_combined[,island]
island_vals[island_vals>0]=1
island=rowSums(island_vals)
island=island==1
most_vuln_spp=cbind(most_vuln_spp,Temp=rep(1,dim(most_vuln_spp)[1]), 
                    Count=rep(1,dim(most_vuln_spp)[1]),selected=rep(0,dim(most_vuln_spp)[1]))
most_vuln_spp[most_vuln_spp$Status.simplified!="Apparently Secure" & 
                island &
                most_vuln_spp$DIVISION=="Dicot", "selected"]=1
group_n=dcast(most_vuln_spp,  selected  ~  Temp,  value.var="Count", sum)
group_mean=dcast(most_vuln_spp,  selected  ~  Temp,  value.var="transformed", mean)
group_sd=dcast(most_vuln_spp,  selected  ~  Temp,  value.var="transformed", sd)

tmp=cbind(all_combined, count=matrix(1, dim(all_combined)[1],1),group=matrix(0, dim(all_combined)[1],1))
#tmp[tmp$DIVISION=="Dicot","group"]=1
#tmp[tmp$Status.simplified!="Apparently Secure","group"]=1
#tmp[tmp$Native.Status=="Endemic","group"]=1

tmp[tmp$DIVISION=="Dicot" & tmp$Status.simplified!="Apparently Secure" & tmp$Native.Status=="Endemic","group"]=1
sum(tmp$group)
cat("vulnerability for species with characteristics associated with vuln is", "\n")
aggregate(transformed ~ group, data=tmp, FUN=median)

cat("area lost by slr for coastal species is", "\n")
aggregate(MRzone_slr ~ Coastal, data=tmp, FUN=mean)


# most_vuln_spp$Status.simplified!="Apparently Secure"
# most_vuln_spp$Native.Status=="Endemic"
# most_vuln_spp$DIVISION=="Dicot"
# most_vuln_spp$Pioneer==0
# most_vuln_spp$Alien_hab_comp==0

####MAKE TOOL AUX DATA
csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)
variables.file="variables.csv"
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)
vlist0=vlist
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
auxcols=colnames(aux_data)
auxcols_desc=rep("",length(auxcols))

#auxcol = auxcols[5]
#auxcol = auxcols[40]
#auxcol = auxcols[33]
i=1
for (auxcol in auxcols){
  if (auxcol %in% header_cols_names){
    jnk=which(header_cols_names==auxcol)
    auxcol=header_cols[jnk]
    jnkvar=which(vlist0$Variable==auxcol)
  }else{
    jnk=which(node_names==auxcol)
    jnkvar=vlist$Variable[jnk]
    jnkvar=which(vlist0$Variable==jnkvar)  
  }
  if (length(jnkvar)>0){
    jnk=vlist0$ET_name[jnkvar]
    jnk1=vlist0$ET_zone[jnkvar]
    if (jnk1!=""){
      jnk=paste(jnk,"in",jnk1, "zone")
    }
    auxcols_desc[i]=jnk
  }else{
    #auxcols_desc[]=vlist0$ET_name[jnkvar]
  }
  i=i+1
}
all_names=cbind(auxcols, auxcols_desc)
write.csv(all_names, paste("results/",  project_name, "_tool_aux_data_col_description.csv", sep=""), row.names=FALSE)
#node_names=vlist$Tool_name_short

###APPENDIX ALL SPP VULNERABILITIES!
csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)
variables.file="variables.csv"
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)

# Abs_q5levels <- function(x, q=c(0.2, 0.4, 0.6, 0.8), lv=c("Very_small", "Small", "Medium", "Large", "Very_large")) {
#   quant_data <- rep(lv[3], length(x))
#   quant_data[is.na(x)] <- "none"
#   quant_data[x < rep(q[2], length(x))] <- lv[2]
#   quant_data[x < rep(q[1], length(x))] <- lv[1]
#   quant_data[x > rep(q[3], length(x))] <- lv[4]
#   quant_data[x > rep(q[4], length(x))] <- lv[5]    
#   return(quant_data)}

#all no duplicates
# header_cols=c('spp', 'DIVISION', 'FAMILY', 'Status.simplified', 'Native.Status', 'Coastal', 'cover1',  'CCE_Area', 'transformed', 'Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
# header_cols_names=c('spp', 'Division', 'Family', 'Consv_stat', 'Native_stat', 'Coastal', 'Cover_1',  'CCE_Area', 'Vulnerability', 'Tolerate', 'Migrate', 'Micro_refugia', 'Eff_Mrf_A', 'Eff_Tol_A', 'Eff_Mig_A', 'Habqual_Mrg', 'Habqual_Tol', 'Habqual_Mig', 'Dispersion')
#shorter with duplicates 
#header_cols=c('spp', 'transformed', 'transformed','Resist','Resist', 'Migrate','Migrate', 'Micro_refugia','Micro_refugia', 'Effective_MRF_area','Effective_MRF_area', 'Effective_Tol_zone_area','Effective_Tol_zone_area', 'Effective_Mig_area', 'Effective_Mig_area','Habitat_qual_MRF','Habitat_qual_MRF', 'Tol_Zone_Habitat_qual','Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion', 'Dispersion')
#header_cols_names=c('Species', 'Vulnerability', 'Rank_Vulnerability','Tolerate','Q_Tolerate', 'Migrate','Q_Migrate', 'Micro_refugia','Q_Micro_refugia', 'Eff_Mrf_A','Q_Eff_Mrf_A', 'Eff_Tol_A', 'Q_Eff_Tol_A','Q_Eff_Mig_A','Q_Eff_Mig_A', 'Habqual_Mrf', 'Q_Habqual_Mrf','Habqual_Tol','Q_Habqual_Tol', 'Habqual_Mig', 'Q_Habqual_Mig','Dispersion', 'Q_Dispersion')

header_cols=c('spp', 'transformed', 'Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
header_cols_names=c('Species', 'Vulnerability', 'Tolerate', 'Migrate', 'Micro_refugia', 'Eff_Mrf_A', 'Eff_Tol_A', 'Eff_Mig_A', 'Habqual_Mrg', 'Habqual_Tol', 'Habqual_Mig', 'Dispersion')

# vlist=vlist[which(vlist$tool_order!="NA"),]
# vlist=vlist[order(vlist$tool_order),]
# node_names=vlist$Tool_name_short
# node_cols=vlist$Node
# jnkk=node_cols
# jnkk1=c()
# for (jnk in jnkk){
#   jnk=paste0("node_",jnk)
#   jnkk1=c(jnkk1, jnk)
# }
# node_cols=jnkk1

aux_data0=all_combined[,header_cols]


#vars=c('Resist.1', 'Migrate.1', 'Micro_refugia.1', 'Effective_MRF_area.1', 'Effective_Tol_zone_area.1', 'Effective_Mig_area.1', 'Habitat_qual_MRF.1', 'Tol_Zone_Habitat_qual.1', 'Mig_Zone_Habitat_qual.1', 'Dispersion.1')
# for (var in vars){
#   jnk=aux_data0[,var]
#   #jnk_med=median(jnk)
#   #cat("median for ",var, " is ",jnk_med, "\n")   
#   #jnk <- ifelse(jnk > jnk_med, "H.", "L.")
#   jnk=Abs_q5levels(jnk)
#   aux_data0[,var]=jnk
#   #jnk=dim(data_resps_discr)[2]
#   #names(data_resps_discr)[jnk]=paste0(var, "_cat")
# }

vars=c('transformed','Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
for (var in vars){
jnk=aux_data0[,var]
jnk=rank(jnk)
jnk1=max(jnk)
jnk=jnk/jnk1
aux_data0[,var]=jnk
}
names(aux_data0)=c(header_cols_names)

# ##aux data
# aux_data1=all_combined[,node_cols]
# names(aux_data1)=c(node_names)
# 
# ##average_condition
# hab_area=c('Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area')
# hab_qual=c('Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual')
# #headers=c('spp',  'sp_code',  'shpcode', 'DIVISION', 'FAMILY', 'GENUS','SPECIES')
# 
# ##qual, area and distr
# #data_resps=all_combined[,headers]
# jnk=rowMeans(all_combined[,hab_qual])
# jnk1=rowMeans(all_combined[,hab_area])
# data_resps=cbind(Quality=jnk,Area=jnk1)
# data_resps=as.data.frame(data_resps)
# vars=c("Quality","Area")
# data_resps_discr=data_resps
# var=vars[1]
# 
# for (var in vars){
#   jnk=data_resps[,var]
#   jnk_med=median(jnk)
#   jnk=Abs_q5levels(jnk)
#   data_resps_discr[,var]=jnk
#   jnk=which(names(data_resps_discr)==var)
#   names(data_resps_discr)[jnk]=paste0(var, "_avg")
# }
# 
# #node_cols %in% names(all_combined) 
# aux_data=cbind(aux_data0, data_resps_discr, aux_data1)
# #names(aux_data)=c(header_cols_names, node_names)
# isl_col=c('Ha_pres', 'Ma_pres', 'Ka_pres', 'Oa_pres', 'Mo_pres', 'La_pres', 'Ke_pres', 'Ni_pres')
# jnk=aux_data[, isl_col]
# jnk[jnk>0]=1
# aux_data[, isl_col]=jnk
#write.csv(aux_data, paste("tables/",  project_name, "_appendix10_all_spp_results.csv", sep=""), row.names=TRUE)
aux_data0=frame_rounding(aux_data0)
write.csv(aux_data0, paste("tables/",  project_name, "_appendix10_all_spp_results.csv", sep=""), row.names=FALSE)


###APPENDIX ALL SPP VULNERABILITIES!
csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)
variables.file="variables.csv"
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)
header_cols=c('spp', 'transformed', 'Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
header_cols_names=c('Species', 'Vulnerability', 'Tolerate', 'Migrate', 'Micro_refugia', 'Eff_Mrf_A', 'Eff_Tol_A', 'Eff_Mig_A', 'Habqual_Mrg', 'Habqual_Tol', 'Habqual_Mig', 'Dispersion')
aux_data0=all_combined[,header_cols]

#vars=c('transformed','Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
vars=c('transformed')
for (var in vars){
  jnk=aux_data0[,var]
  jnk=rank(jnk)
  jnk1=max(jnk)
  jnk=jnk/jnk1
  aux_data0[,var]=jnk
}
names(aux_data0)=c(header_cols_names)

aux_data0=frame_rounding(aux_data0)
write.csv(aux_data0, paste("tables/",  project_name, "_appendix10_all_spp_results2.csv", sep=""), row.names=FALSE)


