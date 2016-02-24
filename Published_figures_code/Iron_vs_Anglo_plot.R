#!/bin/Rscript

require(visreg)
require(scales)

#normalize function
normalize <- function(x){
normalized = (x-min(x))/(max(x)-min(x))
return(normalized)
}


a<-read.table("matrix_w_anglo_iron")
#a<-read.table("ibs.txt", h=T)

#apply normalize function to each column (each sample)
b<-sapply(a[2:11], normalize)
c<-data.frame(b)
rownames(c)<-a$pop

j<-read.table("t", h=F)

colnames(j)<-c("pop", "rank","colour")
c$pop<-rownames(c) 

#merge index with pop data
d<-merge(c,j)



svg(file="Iron_vs_Anglo.svg")
par(mfrow=c(2,2))

#LM
fit<-lm(NO3423~M1489, data=d)
visreg(fit,points=list(cex=1,pch=c(d$colour)+14, col=as.vector(d$colour)), xlab="Iron Age", ylab="Anglo-Saxon")
#text(d$NO3423 ~ d$M1489, col=alpha(as.vector(d$colour),1), labels=substring(d$pop,1,3), pos=2)
legend( x="bottomright", legend=c("N.W.Europe","E.Europe","S.Europe","W.Asia","Middle East","N.Africa"), col=c("#4daf4a","#984ea3","#377eb8","#a65628","orange","red"), pch=c(16,17,15,18,19,20), merge=FALSE , cex=0.7)
visreg(fit,points=list(cex=1,pch=c(d$colour)+14, col=as.vector(d$colour)), xlim=c(0.8,1),ylim=c(0.8,1),xlab="Iron Age", ylab="Anglo-Saxon")
text(d$NO3423 ~ d$M1489, col=alpha(as.vector(d$colour),1), labels=substring(d$pop,1,3), pos=2)

dev.off()
