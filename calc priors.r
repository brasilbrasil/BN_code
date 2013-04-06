#must run process spp data script first
library(reshape2)
library(plyr)

qrld <- read.csv(cat.data.file, stringsAsFactors=FALSE)
temp_qrld=cbind(qrld,count=matrix(1,nrow=dim(qrld)[1],ncol=1))
vars=names(temp_qrld)
vars=vars[3:length(vars)]
vars=vars[1:(length(vars)-1)]

n_max_states=7
all_counts=as.data.frame(matrix(0,nrow=length(vars), ncol=(1+n_max_states*3)))
all_counts[,1]=vars
i=1
var=vars[33]
for (var in vars){
  temp_index=c(which(names(temp_qrld)==var), which(names(temp_qrld)=="count"))
  temp_qrld0=temp_qrld[,temp_index]
  jnk0=dcast(temp_qrld0,  get(var)  ~  "count",  value.var="count", sum)
  names(jnk0)[1]="state"
  no_NA_ct=sum(jnk0[jnk0$state!="none",2])
  all_counts[i,(1:dim(jnk0)[1])+1]=jnk0[,1]
  all_counts[i,(1:dim(jnk0)[1])+(1+n_max_states)]=jnk0[,2]
  all_counts[i,(1:dim(jnk0)[1])+(1+n_max_states*2)]=jnk0[,2]/no_NA_ct
  i=i+1
}

all_counts=all_counts[all_counts[,"V16"]!=Inf,] #get rid of internal BN model vars
write.csv(all_counts, paste(project_name, "_all_priors.csv",sep=""), row.names=TRUE)
