csv_out_data="model_vs_expert_blank.csv"
exp_vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)
spp_to_compare=exp_vul_data$X
csv_out_data=paste("results/",project_name,"_spp_vulnerability_scores.csv", sep="")
model_vul_data <- read.csv(csv_out_data, stringsAsFactors=FALSE)
metrics_to_compare=c("X", "transformed", "vulnerability")
vul_data=model_vul_data[model_vul_data$X %in% spp_to_compare,metrics_to_compare]
vul_data=cbind(vul_data[order(vul_data$X),],Expert=exp_vul_data[order(exp_vul_data$X),"Expert"])

for (i in 2:3){
  jnk=names(vul_data)[i]
  jpeg_name=paste("expert_comparison/",project_name,"_ranked_expert_vs_",jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  
  plot(rank(vul_data[,i]), rank(vul_data[,4]), xlab="Model vulnerability rank", ylab="Expert vulnerability rank")
  
  dev.off()  
}

for (i in 2:3){
  jnk=names(vul_data)[i]
  jpeg_name=paste("expert_comparison/",project_name,"_Experts_vs_",jnk,".jpg", sep="")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  
  plot(vul_data[,i], vul_data[,4], xlab="Model vulnerability", ylab="Mean expert vulnerability")
  
  dev.off()  
}

##To calculate r values  
r=cor(rank(vul_data[,2]), rank(vul_data[,4]))
#rsq=r^2 
