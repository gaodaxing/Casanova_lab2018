library(data.table)
library(dplyr)
library(clipr)
library(psych)
t=read.clipboard.tab()
result=list()
t=data.frame(t)
t1=t[,c(2,3,10)]
t1[,3]=as.numeric(as.character(t1[,3]))
undetermined=35##****undetermined****
for(i in 1:nrow(t1)){
  if(is.na(t1[i,3]))t1[i,3]=undetermined
}
t1=t1[order(t1[[1]]),]
t1=t1[order(sapply(as.character(t1[[1]]),nchar)),]
t1=t1[order(t1[[2]]),]
replicates=1
t1$group=rep(1:replicates,nrow(t1)/replicates)##****replicates****
t2=dcast(t1,...~Target.Name,value.var="Cт")###t2 reshape
j=grep("GAPDH|gapdh|GUS|GAS",colnames(t2))
if(j!=3){
  a=t2[[j]]
  t2[[j]]=t2[[3]]
  t2[[3]]=a
  b=colnames(t2)[j]
  colnames(t2)[j]=colnames(t2)[3]
  colnames(t2)[3]=b
}
for(i in 1:(ncol(t2)-3)){
  t2[[i+3]]=t2[[3]]-t2[[i+3]]
  t2[[i+3]]=2^t2[[i+3]]
  mn=mean(t2[1:3,i+3])
  t2[[i+3]]=t2[[i+3]]/mn
  t3=t2[,c(1,2,i+3)]
  t4=dcast(t3,...~group,value.var = colnames(t2)[i+3])
  t4=t4[order(sapply(as.character(t4[[1]]),nchar)),]
  result[[i]]=t4
  names(result)[i]=colnames(t2)[i+3]
}
result=as.data.frame(result)
if(replicates==1){
  for(i in 1:(ncol(result)/2)){
    result[[i*2]]=result[[i*2]]/result[1,i*2]
  }
}
write_clip(result)