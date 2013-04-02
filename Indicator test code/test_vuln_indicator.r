rm(list=ls())
## working directory
wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/"
setwd(wd)
csv_out_data="results/selected_spp_vulnerability_scores.csv"
vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)
rand=FALSE


repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)}

rand_text=""
if (rand){
  vul_data=rbind(vul_data, vul_data, vul_data, vul_data)#tripling n for smoother graph!
  n_spp=dim(vul_data)[1]
  for (i in 2:4){
    vul_data[,i]=runif(n_spp)  
  }
  rand_text="rand_"
}

vul_data[,6]=((1-(vul_data[,3]+vul_data[,4])/2)+(1-vul_data[,2]))/2
vul_data[,7]=acos(1-vul_data[,5])/(0.5*pi)
names(vul_data)[2]="Tolerate"
names(vul_data)[5]="Multiplicative_vulnerability"
names(vul_data)[6]="Additive_vulnerability"
names(vul_data)[7]="Transformed_multiplicative"

for (i in 2:dim(vul_data)[2]){
  jnk=names(vul_data)[i]
  jpeg_name=paste("graphs/vuln_graphs/",jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  x=vul_data[,i]
  hist(x, main="", breaks = 35)
  dev.off()  
}
jnk="additive vs multiplicative model"
jpeg_name=paste("graphs/vuln_graphs/", jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(vul_data$Multiplicative_vulnerability, vul_data$Additive_vulnerability)
dev.off()    

jnk="transformed vs untrans model"
jpeg_name=paste("graphs/vuln_graphs/", jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(vul_data$Multiplicative, vul_data$Transformed_multiplicative)
dev.off()    

jnk="additive vs transformed model"
jpeg_name=paste("graphs/vuln_graphs/", jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(vul_data$Transformed_multiplicative, vul_data$Additive_vulnerability)
dev.off()    

repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)}

##random distribution!!!
vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)
#vul_data=repmat(vul_data, 5, 1)
vul_data[,5]=((1-(vul_data[,3]+vul_data[,4])/2)*(1-vul_data[,2]))
vul_data[,6]=((1-(vul_data[,3]+vul_data[,4])/2)+(1-vul_data[,2]))/2
vul_data[,7]=acos(1-vul_data[,5])/(0.5*pi)
names(vul_data)[5]="Multiplicative vulnerability"
names(vul_data)[6]="Additive vulnerability"
names(vul_data)[7]="Transformed multiplicative"


for (i in 5:dim(vul_data)[2]){
  jnk=names(vul_data)[i]
  jpeg_name=paste("graphs/vuln_graphs/","rand_", jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  x=vul_data[,i]
  hist(x, main="", breaks = 35)
  dev.off()    
}

jnk="additive vs multiplicative model"
jpeg_name=paste("graphs/vuln_graphs/","rand_", jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(vul_data$Multiplicative, vul_data$Additive)
dev.off()    
