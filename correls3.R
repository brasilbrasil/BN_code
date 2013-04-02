## working directory
server=TRUE
if (server){
  wd <- "//10.0.0.5/data2$//BN vulnerability/Full Process/"
}else{
  wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/"}
setwd(wd)

cor_order="original" #"AOE"
n_dep_vars=11 #must be at end of var list

#first_and_last_discr_var=c(85:169)
csv_data="results/all_combined.csv"
variables.file="variables.csv"
require(XML)
library(ggplot2)
library(corrplot)
library(ecodist)
## load un-categorized data
all_combined <- read.csv(csv_data, stringsAsFactors=FALSE)
#all_combined=all_combined[,-first_and_last_discr_var]

## load the list of variables and corresponding nodes
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)

## remove information for nodes without variables
vlist0 <- vlist[vlist$correl==TRUE,]
vlist0 <- vlist0[order(vlist0$type),]
vlist=vlist0$Variable

## simplify all_combined to contain only variables associated with nodes
## (and sp_name and sp_code)
all_combined <- all_combined[, c("sp_name", "sp_code", vlist)]
jnkx=names(all_combined)
jnkxx=c("sp_name", "sp_code")
for (lo in jnkx){
  jnk=which(vlist0$Variable==lo)
  jnkxx=c(jnkxx,vlist0$node_names[jnk])
}
names(all_combined)=jnkxx

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}


##############CORELS BY TYPE#################
uniq_var_types=unique(vlist0$type)
if (length(which(uniq_var_types==""))>0){
  uniq_var_types=uniq_var_types[-which(uniq_var_types=="")]  
}
uniq_var_type = uniq_var_types[4]
for (uniq_var_type in uniq_var_types){
  sub_vars=vlist0$node_names[vlist0$type==uniq_var_type]
  sub_jnk=all_combined[,sub_vars]
  if (any(!is.finite(sub_jnk$"Distance between current and future suitable range"))){
    sub_jnk$"Distance between current and future suitable range"[!is.finite(sub_jnk$"Distance between current and future suitable range")]=max(sub_jnk$"Distance between current and future suitable range")    
  }
  #plot(sub_jnk$"Ugly habitat (MRF zone)", sub_jnk$"Current envelope area")
  #cor(sub_jnk$"Ugly habitat (MRF zone)", sub_jnk$"Current envelope area", use="complete.obs")
  #jnk_p <- cor.mtest(sub_jnk,0.95)
  jnk=cor(sub_jnk, use="complete.obs")
  jpeg_name=paste("correls/correls_", uniq_var_type,".jpg", sep="")
  jpeg(jpeg_name,
       width = 8, height = 8, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
#  corrplot(jnk, order=cor_order, method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=0.6, tl.srt=45, 
#           p.mat = jnk_p[[1]], sig.level=0.01, insig = "blank", addCoef.col="black", addCoefasPercent = TRUE)
  corrplot(jnk, order=cor_order, method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=0.6, tl.srt=45, 
           addCoef.col="black", addCoefasPercent = TRUE,
           p.mat = 1-abs(jnk), sig.level=0.50, insig = "blank")  
  dev.off()    
  write.csv(jnk, paste("correls/correls_", uniq_var_type,".csv", sep=""), row.names=TRUE)
}

##############CORELS BY TYPE2#################
uniq_var_types=unique(vlist0$type2)
uniq_var_types=uniq_var_types[-which(uniq_var_types=="")]
uniq_var_type = uniq_var_types[1]
for (uniq_var_type in uniq_var_types){
  sub_vars=vlist0$node_names[vlist0$type2==uniq_var_type]
  sub_jnk=all_combined[,sub_vars]
  if (any(!is.finite(sub_jnk$"Distance between current and future suitable range"))){
    sub_jnk$"Distance between current and future suitable range"[!is.finite(sub_jnk$"Distance between current and future suitable range")]=max(sub_jnk$"Distance between current and future suitable range")    
  }  
  jnk=cor(sub_jnk, use="complete.obs")
  jpeg_name=paste("correls/correls_", uniq_var_type,".jpg", sep="")
  jpeg(jpeg_name,
       width = 8, height = 8, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  corrplot(jnk, order=cor_order, method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=0.6, tl.srt=45, 
           addCoef.col="black", addCoefasPercent = TRUE,
           p.mat = 1-abs(jnk), sig.level=0.50, insig = "blank")    
  dev.off()    
  write.csv(jnk, paste("correls/correls_", uniq_var_type,".csv", sep=""), row.names=TRUE)
}

##############CORELS LIMITED#################

jnk0_ind=all_combined[,3:(dim(all_combined)[2]-n_dep_vars)]
jnk00=all_combined[,(dim(all_combined)[2]-(n_dep_vars-1)):dim(all_combined)[2]]
jnk=cor(jnk0_ind,jnk00, use="complete.obs")

#label="correls_responses_and_vuln"
#jpeg_name=paste("correls/", label,".jpg", sep="")
#jpeg(jpeg_name,
#     width = 3, height = 8, units = "in",
#     pointsize = 12, quality = 90, bg = "white", res = 300)
#corrplot(jnk, order="original", method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=0.4, tl.srt=45)
#dev.off()    
avg=rowMeans(jnk[,1:3])
#jnk=cbind(jnk,avg)
jnk=jnk[order(jnk[,n_dep_vars], decreasing = TRUE),]

write.csv(jnk, "correls/response_correl_matrix.csv", row.names=TRUE)

##############CORELS INDEPENDENTS#################
jnk=cor(jnk0_ind, use="complete.obs")
label="correls_independents"
jpeg_name=paste("correls/", label,".jpg", sep="")
jpeg(jpeg_name,
     width = 8, height = 8, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
corrplot(jnk, order=cor_order, method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         p.mat = 1-abs(jnk), sig.level=0.50, insig = "blank")  
dev.off()    
write.csv(jnk, "correls/correl_independents_matrix.csv", row.names=TRUE)

# ##############CORELS DEPENDENTS#################
# jnk=cor(jnk00, use="complete.obs")
# label="correls_dependents"
# jpeg_name=paste("correls/", label,".jpg", sep="")
# jpeg(jpeg_name,
#      width = 8, height = 8, units = "in",
#      pointsize = 12, quality = 90, bg = "white", res = 300)
# corrplot(jnk, order="AOE", method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=1, tl.srt=45)
# dev.off()    
# write.csv(jnk, "correls/correl_dependents_matrix.csv", row.names=TRUE)

##############CORELS DEPENDENTS no responses#################
jnk=cor(jnk00[,-c(8:10)], use="complete.obs")
label="correls_dependents_no_resp"
jpeg_name=paste("correls/", label,".jpg", sep="")
jpeg(jpeg_name,
     width = 8, height = 8, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
corrplot(jnk, order="original", method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=1, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         p.mat = 1-abs(jnk), sig.level=0.50, insig = "blank")  
dev.off()    
write.csv(jnk, "correls/correl_independents_matrix_no_resp.csv", row.names=TRUE)

##############CORELS#################
jnk0=all_combined[,3:dim(all_combined)[2]]
jnk=cor(jnk0, use="complete.obs")
label="correls"
jpeg_name=paste("correls/", label,".jpg", sep="")
jpeg(jpeg_name,
     width = 8, height = 8, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
corrplot(jnk, order=cor_order, method="circle", tl.pos="lt", type="upper", tl.col="black", tl.cex=0.4, tl.srt=45)
dev.off()    

write.csv(jnk, "correls/correl_matrix.csv", row.names=TRUE)


# #############PCA#####################
jnk00=jnk0_ind[complete.cases(jnk0_ind),]
#jnk00=jnk0[,1:(dim(jnk0)[2]-4)]

pc <- princomp(jnk00)
print(summary(pc))

label="pca_biplot"
jpeg_name=paste("correls/", label,".jpg", sep="")
jpeg(jpeg_name,
     width = 8, height = 8, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
biplot(pc)
dev.off()    

label="pca_screeplot"
jpeg_name=paste("correls/", label,".jpg", sep="")
jpeg(jpeg_name,
     width = 4, height = 4, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
screeplot(pc, npcs = 3, main="")
dev.off()



# #############NMDS#####################
dist=distance(jnk00, "sorensen")

iris.nmds <- nmds(dist, mindim=2, maxdim=2, nits=3)
iris.nmin <- nmds.min(iris.nmds)

# Plot NMDS result with symbols denoting species
label="NMDS_results"
jpeg_name=paste("correls/", label,".jpg", sep="")
jpeg(jpeg_name,
     width = 8, height = 8, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(iris.nmin)
dev.off()

# # Fit vectors for the main variables to the NMDS configuration
# iris.vf <- vf(iris.nmin, jnk00, nperm=10)
# label="NMDS_w_vars"
# jpeg_name=paste("correls/", label,".jpg", sep="")
# jpeg(jpeg_name,
#      width = 8, height = 8, units = "in",
#      pointsize = 12, quality = 90, bg = "white", res = 300)
# plot(iris.vf, col="blue")
# dev.off()    


##PCA wrt vulnerability
#pc <- princomp(jnk000~jnk00)

# #############3D plot###############
#with(foo, plot3d(mr, tol, mg))
# #with(foo, spheres3d(mr, tol, mg, color=rangecol, radius=0.01))
# with(foo, spheres3d(mr, tol, mg, color=elevcol, radius=0.01))
# with(foo, text3d(mr, tol, mg, spp))
# 
