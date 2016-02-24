pdf(file="fineSTRUCTURE_barplot.pdf")

a<-read.table("testin", h=T)
require(RColorBrewer)

cols<-brewer.pal(10,"Spectral")
j<-as.data.frame(cbind(levels(a$pop),cols))
colnames(j)[1]<-"pop"
c<-merge(j, a)

barplot(as.matrix(c[3:15]), col=as.vector(c$cols), las=2)
dev.off()
