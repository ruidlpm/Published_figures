pdf(file="F4_chromopainter.pdf", paper="a4")

d<-read.table("results.txt", h=T)

#normalize function
normalize <- function(x){
normalized = (x-min(x))/(max(x)-min(x))
return(normalized)
}

#apply normalize function to each column (each sample)
b<-sapply(d[2:10], normalize)
b<-as.data.frame(b)
b$clusters<-d$clusters


require(visreg)
require(scales)

par(mfrow=c(2,2))

barplot_order<-c(1,5,6,2,3,13,4,12,10,7,11,9,8)

a<-read.table("results_clustering_prop.txt", h=T)
require(RColorBrewer)

cols<-brewer.pal(10,"Spectral")
j<-as.data.frame(cbind(levels(a$pop),cols))
colnames(j)[1]<-"pop"
c<-merge(j, a)

barplot(as.matrix(c[3:15][barplot_order]), col=as.vector(c$cols), las=2, names.arg=gsub("_"," ",paste(colnames(a)[2:length(colnames(a))],paste("(n=",popsize,")", sep="")))[barplot_order])

# clusters order   1  2  3  4  5  6  7  8  9 10 11 12 13
b$colour<-c("black",colors()[473],colors()[573],"#F781BF","#999999","peachpuff","navyblue","#984EA3","#4DAF4A" ,"steelblue","#FF7F00",colors()[142],"#E41A1C")
b$k<-1:length(b$clusters)
popsize<-c(9,2,2,2,3,8,14,30,47,70,3,202,606)

fit<-lm(M1489 ~ Roman,data=b)
visreg(fit, xlab="Roman", ylab="Iron Age", ylim=c(0,1))
points(b$M1489 ~ b$Roman, bg=alpha(b$colour, 0.6), cex=2, pch=21)
text(b$M1489 ~ b$Roman, labels=b$k, cex=0.8)
mtext("r=0.74", cex=0.7)

barplot(b$Roman[c(barplot_order)], las=2, col="grey",names.arg=gsub("clust","cluster",b$clusters[c(barplot_order)]))


fit<-lm(NO3423 ~ Roman,data=b)
visreg(fit, xlab="Roman", ylab="Anglo-Saxon")
points(b$NO3423 ~ b$Roman, bg=alpha(b$colour, 0.6), cex=2, pch=21)
text(b$NO3423 ~ b$Roman, labels=b$k, cex=0.8)
mtext("r=0.06",cex=0.7)

dev.off()
