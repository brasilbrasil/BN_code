server=TRUE
if (server){
  wd <- "//10.0.0.5/data2$//BN vulnerability/Full Process/"
}else{
  wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/Conceptual model/2- implementation (mine)/Full Process/"}
setwd(wd)

all_combined_file="min_max_comparison/all_combined_comparison.csv"
data=read.csv(all_combined_file, stringsAsFactors=FALSE)
data=cbind(data, count=matrix(1,dim(data)[1],1))
comparison_cols=c("transformed", "Mx_ql_transformed", "Mn_ql_transformed", "Actual_max", "Min_actual", "min_max")
data_summary=summary(data[,comparison_cols])
write.csv(data_summary, "min_max_comparison/comparison_summary.csv", row.names=TRUE)
median_vuln=median(data$transformed)
jnk=data$count[data$transformed>=median_vuln]
n_actual_vuln=length(jnk)

jnk=data$count[data$Mx_ql_transformed>=median_vuln]
n_min_vuln=length(jnk)

jnk=data$count[data$Mn_ql_transformed>=median_vuln]
n_max_vuln=length(jnk)

