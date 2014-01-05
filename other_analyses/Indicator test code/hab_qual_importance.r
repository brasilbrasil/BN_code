rm(list=ls())
## working directory
wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/indicator test/"
setwd(wd)
csv_out_data="Vuln_scores_no_hab_qual.csv"
vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)



jnk="f_multiplicative vs expert model"
jpeg_name=paste("vuln_graphs/", jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(rank(vul_data$Expert), rank(vul_data$b_Transformed_multiplicative))
dev.off()    

jnk="f_multiplicative vs expert model no hab qual"
jpeg_name=paste("vuln_graphs/", jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(rank(vul_data$Expert), rank(vul_data$no_hab_trans))
dev.off()    

jnk="f_rank trans model w&wout hab qual"
jpeg_name=paste("vuln_graphs/", jnk,".jpg", sep="")
jpeg(jpeg_name,
     width = 5, height = 5, units = "in",
     pointsize = 12, quality = 90, bg = "white", res = 300)
plot(rank(vul_data$no_hab_trans), rank(vul_data$b_Transformed_multiplicative))
dev.off()    

