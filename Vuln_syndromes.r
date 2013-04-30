csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)

all_dep=c('Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion', 'transformed')
resps=c('Resist', 'Migrate', 'Micro_refugia')
all_factor_groups=c('Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
hab_area=c('Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area')
hab_qual=c('Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual')
headers=c('spp',  'sp_code',	'shpcode', 'DIVISION', 'FAMILY', 'GENUS','SPECIES')

##qual, area and distr
data_resps=all_combined[,headers]
jnk=rowMeans(all_combined[,hab_qual])
jnk1=rowMeans(all_combined[,hab_area])
data_resps=cbind(data_resps,Quality=jnk,Area=jnk1,Distribution=all_combined[,"Dispersion"],
                 Vulnerability=all_combined[,"transformed"])
vars=c("Quality","Area", "Distribution", "Vulnerability")
data_resps_discr=data_resps
var=vars[1]
for (var in vars){
  jnk=data_resps[,var]
  jnk_med=median(jnk)
  cat("median for ",var, " is ",jnk_med, "\n")   
  jnk <- ifelse(jnk > jnk_med, "H.", "L.")
  data_resps_discr=cbind(data_resps_discr, jnk)
  jnk=dim(data_resps_discr)[2]
  names(data_resps_discr)[jnk]=paste0(var, "_cat")
}
jnk=dim(data_resps_discr)[1]
data_resps_discr=cbind(data_resps_discr,Temp=matrix(1,jnk,1), Count=matrix(1,jnk,1))
jnk=data_resps_discr[,c("Vulnerability_cat", "Quality_cat","Area_cat", "Distribution_cat")]
merged=apply(jnk,1,paste,collapse="")
data_resps_discr=cbind(data_resps_discr, Merged=merged)
library(reshape2)
library(plyr)
group_n=dcast(data_resps_discr,  Merged ~  Temp,  value.var="Count", sum)
names(group_n)=c("Syndrome", "Count")
foo <- data.frame(do.call('rbind', strsplit(as.character(group_n$Syndrome),'.',fixed=TRUE)))
names(foo)=c("Vulnerability", "Quality","Area", "Distribution")
levels(foo)
group_n=cbind(foo, group_n$Count)
jnk=dim(group_n)[2]
names(group_n)[jnk]="Species count"
group_n <- data.frame(lapply(group_n, as.character), stringsAsFactors=FALSE)
group_n[group_n=="H"]='High'
group_n[group_n=="L"]='Low'
write.csv(group_n, paste("tables/",  project_name, "_vulnerability_syndromes.csv", sep=""), row.names=TRUE)

# #these here calculate the binary relationship among variables- show how area and distribution is related
# group_n=dcast(data_resps_discr,  Quality_cat ~  Vulnerability_cat,  value.var="Count", sum)
# group_n
# group_n=dcast(data_resps_discr,  Area_cat ~  Vulnerability_cat,  value.var="Count", sum)
# group_n
# group_n=dcast(data_resps_discr,  Area_cat ~  Distribution_cat,  value.var="Count", sum)
# group_n
# group_n=acast(data_resps_discr,  Quality_cat  ~ Area_cat ~  Vulnerability_cat,  value.var="Count", sum)


###CLUSTER CREATION! ((this did not work so well))
#responses only
data_resps=all_combined[,resps]
d <- dist(data_resps, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 

resps_groups3 <- cutree(fit, k=3) # cut tree into 5 clusters
#resps_groups7 <- cutree(fit, k=7) # cut tree into 5 clusters
jpeg_name=paste("graphs/",project_name,"_clusterplots_","resps_cluster",".jpg", sep = "")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(fit) # display dendogram
  rect.hclust(fit, k=3, border="red")
dev.off()  

#dep=all_dep[1]
for (dep in all_dep){
  jpeg_name=paste("graphs/",project_name,"_clusterplots_","resps_5groups_vs_",dep,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(as.factor(resps_groups3),all_combined[,dep])
  dev.off()  
}
all_factor_dep=c('node_Winkout','node_CE_overlap')
for (dep in all_factor_dep){
  jpeg_name=paste("graphs/",project_name,"_clusterplots_","resps_5groups_vs_",dep,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(as.factor(resps_groups3),as.factor(all_combined[,dep]))
  dev.off()  
}

#all factor groups
data_all_factor_groups=all_combined[,all_factor_groups]
d <- dist(data_all_factor_groups, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
resps_all_factor5 <- cutree(fit, k=5) # cut tree into 5 clusters
resps_all_factor3 <- cutree(fit, k=3) # cut tree into 5 clusters

jpeg_name=paste("graphs/",project_name,"_clusterplots_","all_factors_cluster",".jpg", sep = "")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(fit) # display dendogram
rect.hclust(fit, k=3, border="red")
dev.off()  

#dep=all_dep[1]
for (dep in all_dep){
jpeg_name=paste("graphs/",project_name,"_clusterplots_","all_factor_5groups_vs_",dep,".jpg", sep = "")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(as.factor(resps_all_factor3),all_combined[,dep])
dev.off()  
}
all_factor_dep=c('node_Winkout','node_CE_overlap')
for (dep in all_factor_dep){
  jpeg_name=paste("graphs/",project_name,"_clusterplots_","all_factor_5groups_vs_",dep,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(as.factor(resps_all_factor3),as.factor(all_combined[,dep]))
  dev.off()  
}

#all hab qual
data_hab_qual=all_combined[,hab_qual]
d <- dist(data_hab_qual, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
hab_qual_cluster4 <- cutree(fit, k=4) # cut tree into 5 clusters
#hab_qual_cluster7 <- cutree(fit, k=7) # cut tree into 5 clusters

jpeg_name=paste("graphs/",project_name,"_clusterplots_","hab_qual_cluster",".jpg", sep = "")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(fit) # display dendogram
rect.hclust(fit, k=4, border="red")
dev.off()  


#dep=all_dep[1]
for (dep in all_dep){
  jpeg_name=paste("graphs/",project_name,"_clusterplots_","hab_qual_4groups_vs_",dep,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(as.factor(hab_qual_cluster4),all_combined[,dep])
  dev.off()  
}
all_factor_dep=c('node_Winkout','node_CE_overlap')
for (dep in all_factor_dep){
  jpeg_name=paste("graphs/",project_name,"_clusterplots_","hab_qual_5groups_vs_",dep,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(as.factor(hab_qual_cluster4),as.factor(all_combined[,dep]))
  dev.off()  
}

#####
clustered=cbind(all_combined,resps_groups3, resps_all_factor3, resps_all_factor5, hab_qual_cluster4)
write.csv(clustered, paste("results/",  project_name, "_cluster_results.csv", sep=""), row.names=TRUE)


#regression tree
library(rpart)
data_rpart=all_combined[,c(all_factor_groups,"transformed", "node_Winkout","node_CE_overlap")]

fit <- rpart(transformed ~ node_CE_overlap + node_Winkout + Effective_MRF_area + Effective_Tol_zone_area + Effective_Mig_area + Habitat_qual_MRF + Tol_Zone_Habitat_qual + Mig_Zone_Habitat_qual + Dispersion,
             method="anova", data=data_rpart)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for vulnerability")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for vulnerability")


#Test for significance
# library(pvclust)
# #THIS TAKES AGES
# data_all_factor_groups_C <- pvclust(t(data_all_factor_groups), nboot=1000)
# plot(data_all_factor_groups_C, cex=0.8, cex.pv=0.7)
# pvrect(data_all_factor_groups_C, alpha=0.8)
#msplot(data_all_factor_groups_C)
#print(data_all_factor_groups_C)
#seplot(data_all_factor_groups_C)

#THIS TAKES AGES
# data_resps_C <- pvclust(t(data_resps), nboot=1000)
# plot(data_resps_C, cex=0.8, cex.pv=0.7)
# pvrect(data_resps_C, alpha=0.8)
