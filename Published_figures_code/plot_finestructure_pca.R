require("RColorBrewer")
require("scales")

#read in pca just to get the dims
pca<-read.csv("output.csv")
pca1<-subset(pca, pca$Label!="Eigenvalue")
pca<-pca1

#make list of pca files for each cluster in the pwd
ff <- list.files(pattern=".clust.inds_per_cluster.pca")

#get number of clusters
k<-NULL
for (i in 1:length(ff)){
k[i]<-as.numeric(unlist(strsplit(ff[i], '.clust'))[[1]])
}

#read tables
myfilelist <- lapply(ff, read.table)

#get popids
get_popid<-function(x){
	popid<-(gsub("_WTC.*$", "", x$V1))
	x<-as.data.frame(cbind(x[1],x[2],x[3],popid))
}

#when popid is North_West, East Anglia etc etc, replace with England
rep_ids<-function(h){
	popid2<-NULL
	for (subpop in 1:length(h$popid)){
		if (h$popid[subpop]!="Wales" & h$popid[subpop]!="Scotland") {
			popid2[subpop]<-"England"
		}
		else {
			popid2[subpop]<-as.character(h$popid[subpop])
		}
	}
	cbind(h,popid2)

}

myfilelist<-lapply(myfilelist,get_popid)
myfilelist<-lapply(myfilelist,rep_ids)

#plot PCA
pdf(file="fineSTRUCTURE_PCA.pdf", paper="a4")

plot(pca$Component.1, pca$Component.2, type="n", main="fineSTRUCTURE PCA", xlab="PC1", ylab="PC2")# asp=1)

counter <- 0
plot_pops<-function(x, colour){
	counter <<- counter + 1
	xEng<-subset(x, x$popid2=="England")
	xWa<-subset(x, x$popid2=="Wales")
	xSct<-subset(x, x$popid2=="Scotland")

	points(xEng$V2,xEng$V3, col=alpha(colour[counter], 0.8),  pch=pts[1], cex=0.7)
	points(xWa$V2,xWa$V3, col=alpha(colour[counter], 0.8),  pch=pts[2], cex=0.7)
	points(xSct$V2,xSct$V3, col=alpha(colour[counter], 0.8),  pch=pts[3], cex=0.7)

	points(median(x$V2),median(x$V3), bg=alpha(colour[counter], 0.6), cex=3, pch=21)
	text(median(x$V2),median(x$V3), label=k[counter])
}

# clusters order  10 11 12 13 1  2  3  4  5  6  7  8  9
cols<-c("steelblue","#FF7F00",colors()[142],"#E41A1C","black",colors()[473],colors()[573],"#F781BF","#999999","peachpuff","navyblue","#984EA3","#4DAF4A" )
pts<-c(3,6,8)

#call  plot pops function
lapply(myfilelist, plot_pops,cols)

#legend according to major pop groups
legend( x="topright", legend=c("English","Welsh","Scottish"),title="Population labels", col="black", pch=pts, cex=1)

dev.off()
