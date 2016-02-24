pdf(file="SM_Deamination_roman_paper.pdf")

par(mfrow=c(2,2))


#read mapdamage output
#5 prime end
d5 <- read.table("input_deaminCtoT.txt", header=T)

#3 prime end
d3 <- read.table("input_deaminGtoA.txt", header=T)


#make colours
cols=c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","red", "black")

#left-hand plot
#plot empty
plot(d5$X3DT16, type="n", ylim=c(0,0.25), xlab="Distance from 5' end", ylab="C->T changes",  yaxt="n")

#add axis
axis(2, at=c(0.00, 0.05, 0.1, 0.15, 0.2, 0.25))

#draw lines for each column/sample
for (i in 1:length(d5)) {
	lines(d5[i], col=cols[i], lwd=2)
}


#right-hand plot
plot(d3$X3DT16, type="n", ylim=c(0,0.25), xlim=c(25, 1), xlab="Distance from 3' end", ylab="G->A changes",  yaxt="n")

#add axis
axis(4, at=c(0.00, 0.05, 0.1, 0.15, 0.2, 0.25))

#draw lines for each column/sample
for (i in 1:length(d3)) {
	lines(d3[i], col=cols[i], lwd=2)
}

#add legend
legend(x="topright" ,legend=c("Roman","Iron Age", "Anglo-Saxon"), col=c("yellow", "red", "black"), lty=1, lwd=2, cex=0.7)
dev.off()
