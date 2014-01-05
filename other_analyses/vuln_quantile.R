rm(list = ls()) #remove all past worksheet variables
wd="C:/Users/lfortini/Dropbox/current work/HI spp VA/writing/Plant/HIplantVATool/data/"
setwd(wd)
tmp=read.csv('tool_aux_data.csv',header=T, stringsAsFactors=F)
vuln=tmp$Vulnerability
Qvuln=data.frame(Vuln_quant=rank(vuln,vuln))
Qvuln=(Qvuln/max(Qvuln))*100

frame_rounding=function(frame){
  ii <- sapply(frame, is.factor)
  frame[ii] <- lapply(frame[ii], as.character)  
  for (i in 1:dim(frame)[1]){
    for (j in 1:dim(frame)[2]){
      temp=frame[i,j]
      if (suppressWarnings(!is.na(as.numeric(temp)))){
        frame[i,j]=signif(as.numeric(temp),3)
        #frame[i,j]=round(as.numeric(temp),3)  
      }
    }
  }
  return(frame)
}  


Qvuln=frame_rounding(Qvuln)
tmp=cbind(tmp,Qvuln)
write.csv(tmp, 'tool_aux_data2.csv', row.names=FALSE)

