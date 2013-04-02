rm(list=ls())
## working directory
wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/indicator test/"
setwd(wd)
csv_out_data="selected_spp_vulnerability_scores.csv"
vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)
rand=TRUE


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

vul_data[,5]=((1-(vul_data[,3]+vul_data[,4])/2)*(1-vul_data[,2]))
vul_data[,6]=((1-(vul_data[,3]+vul_data[,4])/2)+(1-vul_data[,2]))/2
vul_data[,7]=acos(1-vul_data[,5])/(0.5*pi)
names(vul_data)[2]="a_Tolerate"
names(vul_data)[3]="a_Migrate"
names(vul_data)[4]="a_Micro_refugia"
names(vul_data)[5]="b_Multiplicative_vulnerability"
names(vul_data)[6]="b_Additive_vulnerability"
names(vul_data)[7]="b_Transformed_multiplicative"

for (i in 2:dim(vul_data)[2]){
  jnk=names(vul_data)[i]
  jpeg_name=paste("vuln_graphs/",rand_text, jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  x=vul_data[,i]
  hist(x, main="", breaks = 35)
  dev.off()  
}
jnk="c_additive vs multiplicative model"
jpeg_name=paste("vuln_graphs/", rand_text, jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(vul_data$b_Multiplicative_vulnerability, vul_data$b_Additive_vulnerability)
dev.off()    

jnk="c_transformed vs untrans model"
jpeg_name=paste("vuln_graphs/", rand_text, jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(vul_data$b_Multiplicative_vulnerability, vul_data$b_Transformed_multiplicative)
dev.off()    

jnk="c_additive vs transformed model"
jpeg_name=paste("vuln_graphs/", rand_text, jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(vul_data$b_Transformed_multiplicative, vul_data$b_Additive_vulnerability)
dev.off()    

