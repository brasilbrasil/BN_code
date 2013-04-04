rm(list=ls())
## working directory
server=FALSE
if (server){
  wd <- "//10.0.0.5/data2$//BN vulnerability/Full Process/indicator test/"
}else{
  wd <- "C:/Users/lfortini/Google Drive/BN vulnerability/Full Process template/indicator test/"
}
setwd(wd)
csv_out_data="model_vs_expert.csv"
vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)

for (i in 5:7){
  jnk=names(vul_data)[i]
  jpeg_name=paste("vuln_graphs/ranked_expert_vs_",jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  
  plot(rank(vul_data[,i]), rank(vul_data[,8]), xlab=jnk, ylab="Ranked expert score")
  
  dev.off()  
}

for (i in 5:7){
  jnk=names(vul_data)[i]
  jpeg_name=paste("vuln_graphs/Experts_vs_",jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  
  plot(vul_data[,i], vul_data[,8], xlab=jnk, ylab="Expert score")
  
  dev.off()  
}

r=cor(rank(vul_data[,7]), rank(vul_data[,8]))
rsq=r^2 
