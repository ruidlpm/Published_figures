#!/bin/Rscript
#makes Supplementary Fig. 13, Roman paper

svg(file="new_corrplot_SM.svg")
require(corrplot)
require(scales)

ranks<-read.table("corplot_newlab", header=T)
x<-ranks

#make vectors of point types
j1<-14+c(x$group)
j2<-as.vector(x$group)


#change panel.pts function in corrplot package to plot points by col
 panel.pts2 <-function (x, y, corr = NULL, col.regions, cor.method, ...)
 {
     if (!is.null(corr)) 
         return()
     plot.xy(xy.coords(x, y), type = "p", col=c(j2), pch=j1)

     box(col = "lightgray")

}

#plot corrgram
corrgram(x,type="data",order="PCA", lower.panel=panel.ellipse,cor.method="spearman",
  upper.panel=panel.pts2, text.panel=panel.conf, 
  diag.panel=panel.minmax, pch=20, main="Parwise correlation of IBS ranks - Roman samples")

dev.off()
