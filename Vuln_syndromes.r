csv_data=paste("results/",project_name,"_","all_combined.csv",sep="")
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)

all_dep=c('Resist', 'Migrate', 'Micro_refugia', 'Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
resps=c('Resist', 'Migrate', 'Micro_refugia')
all_factor_groups=c('Effective_MRF_area', 'Effective_Tol_zone_area', 'Effective_Mig_area', 'Habitat_qual_MRF', 'Tol_Zone_Habitat_qual', 'Mig_Zone_Habitat_qual', 'Dispersion')
headers=c('spp',  'sp_code',	'shpcode', 'DIVISION', 'FAMILY', 'GENUS','SPECIES')

data_resps=all_combined[,resps]
d <- dist(data_resps, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
resps_groups5 <- cutree(fit, k=5) # cut tree into 5 clusters
resps_groups7 <- cutree(fit, k=7) # cut tree into 5 clusters
rect.hclust(fit, k=7, border="red")

plot(as.factor(resps_groups5),all_combined[,'Resist'])
plot(as.factor(resps_groups5),all_combined[,'Migrate'])
plot(as.factor(resps_groups5),all_combined[,'Micro_refugia'])
plot(as.factor(resps_groups5),all_combined[,'transformed'])

data_all_factor_groups=all_combined[,all_factor_groups]
d <- dist(data_all_factor_groups, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
resps_all_factor5 <- cutree(fit, k=5) # cut tree into 5 clusters
resps_all_factor7 <- cutree(fit, k=7) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red")

dep=all_dep[1]
for (dep in all_dep){
jpeg_name=paste("graphs/",project_name,"_clusterplots_","all_factor_5groups_vs_",dep,".jpg", sep = "")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
  plot(as.factor(resps_all_factor5),all_combined[,dep])
dev.off()  
}

plot(as.factor(resps_all_factor5),as.factor(all_combined[,'node_Winkout']))
plot(as.factor(resps_all_factor5),as.factor(all_combined[,'node_CE_overlap']))

clustered=cbind(all_combined,resps_groups5, resps_groups7, resps_all_factor5, resps_all_factor7)
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
library(pvclust)
#THIS TAKES AGES
data_all_factor_groups_C <- pvclust(t(data_all_factor_groups), nboot=1000)
plot(data_all_factor_groups_C, cex=0.8, cex.pv=0.7)
pvrect(data_all_factor_groups_C, alpha=0.8)
#msplot(data_all_factor_groups_C)
#print(data_all_factor_groups_C)
#seplot(data_all_factor_groups_C)

#THIS TAKES AGES
data_resps_C <- pvclust(t(data_resps), nboot=1000)
plot(data_resps_C, cex=0.8, cex.pv=0.7)
pvrect(data_resps_C, alpha=0.8)
