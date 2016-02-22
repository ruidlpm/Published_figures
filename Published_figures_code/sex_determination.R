#!/bin/Rscript

pdf("sex_determination_plot_final.pdf",width=3, height=4)
require(ggplot2)
d <- read.table("input_XY.txt", header=T)
shapes1=c(23,23,23,23,23,23,23,23,23,22,22,22,22,22,22,22,22,22)
col1=c("red", "yellow", "yellow","yellow","yellow","yellow","yellow","yellow","white","red", "yellow", "yellow","yellow","yellow","yellow","yellow","yellow","white")
yaxiscol=c("black","black","black","black","black","black","black","black","black","steelblue","steelblue","steelblue","steelblue","steelblue","steelblue","steelblue","steelblue","steelblue")
##d is a data frame with 4 columns
errorbarcol=yaxiscol
# d$x gives variable names
# d$y gives center point
# d$ylo gives lower limits
# d$yhi gives upper limits
 forestplot <- function(d, xlab="Ry", ylab="Sample"){
    require(ggplot2)
    p <- ggplot(d, aes(x=factor(x, levels=unique(x)), y=y, ymin=ylo, ymax=yhi), ylim=c(0,0.12)) + 

		geom_pointrange(colour="black",size=0.1) + 
		geom_rect(aes(ymin=-Inf,ymax=0.016,xmin=-Inf,xmax=Inf),fill="lightgray", alpha=0.15) +
		geom_rect(aes(ymin=0.075,ymax=Inf,xmin=-Inf,xmax=Inf),fill="lightgray", alpha=0.15) +
		coord_flip() +
		theme_bw() +
		ylab(xlab) +
		geom_pointrange(colour="black",size=0.1) + 
		geom_point(fill=col1, shape=shapes1, cex=3) +
		xlab(ylab) + #switch because of the coord_flip() above
		theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
		theme(axis.text.y=element_text(colour=yaxiscol)) +
		theme(text = element_text(size=9))



    return(p)

}

forestplot(d)

dev.off()


