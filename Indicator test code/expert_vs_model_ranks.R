rm(list=ls())
## working directory
wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/indicator test/"
setwd(wd)
csv_out_data="model_vs_expert.csv"
vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)

for (i in 5:8){
  jnk=names(vul_data)[i]
  jpeg_name=paste("vuln_graphs/ranked_expert_vs_",jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  
  plot(rank(vul_data[,i]), rank(vul_data[,9]), xlab=jnk, ylab="Ranked expert score")
  
  dev.off()  
}

r=cor(rank(vul_data[,7]), rank(vul_data[,8]))
rsq=r^2 