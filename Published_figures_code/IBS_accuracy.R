require(scales)

#read in
a<-read.table("plink.mibs.id")
b<-read.table("plink.mibs")

#label cols as inds
colnames(b) <- a$V2
b$Recipient <- a$V1
j<-aggregate(. ~ Recipient, data=b,FUN=median)


#rank ibs values
j1<-sapply(j,rank)
j1<-as.data.frame(j1)
rownames(j1) <-j$Recipient

#transpose
bb<-as.data.frame(t(j1))

#exlude recipient line
res1<-subset(bb, rownames(bb)!="Recipient")

#add pop names as column
res1$pop<-a$V1


#iterate through indivs. 
#get the value(rank) at which the population of the indiv. (res1$pop[indiv]) is identified (==colnames(res1))][indiv,] is true)
score<-NULL
for (indiv in 1:length(res1$pop)){
	score[indiv]<-res1[which(res1$pop[indiv]==colnames(res1))][indiv,]
}

#paste scores into table
res1$score<-score

#read index with colours
index <-read.table("index2", comment.char="", h=T)

#merge
gg1<-merge(res1, index)

#order by colour, i.e. population group
gg1<-as.data.frame(gg1[with(gg1, order(colour)), ])

#write output
write.table(gg1,"Roman_IBS_accuracy_table.txt")



#pdf showing accuracy across populations
pdf(file="accuracy.pdf", width=10, height=6)
plot(gg1$score, col=alpha(as.vector(gg1$colour), 0.6), pch=20, ylim=c(0,46), cex=1, main="IBS scoreuracy", xlab="Inds.", ylab="Rank")
#plot median ranks at which IBS is identified
for (i in levels(gg1$pop)){
points(median(which(gg1$pop==i)),median(gg1$score[which(gg1$pop==i)]), cex=4, pch=21, bg=as.vector(alpha(gg1$colour[which(gg1$pop==i)],0.6))[1])
#add pop label short
text(median(which(gg1$pop==i)),median(gg1$score[which(gg1$pop==i)]), label=substring(i,1,3), cex=0.8)
}
dev.off()


#make seprate boxplots for each popualtion group
pdf(file="boxplots.pdf")

EEurope <- subset(gg1,gg1$colour=="#984ea3", ylim=c(0,45))
SEurope <- subset(gg1,gg1$colour=="#377eb8", ylim=c(0,45))
NWEurope <- subset(gg1,gg1$colour=="#4daf4a", ylim=c(0,45))
WAsia <- subset(gg1,gg1$colour=="#a65628", ylim=c(0,45))
MEast <- subset(gg1,gg1$colour=="orange", ylim=c(0,45))
NAfrica <- subset(gg1,gg1$colour=="red", ylim=c(0,45))
BIsles<-subset(NWEurope, NWEurope$pop=="English" | NWEurope$pop=="Ireland" | NWEurope$pop=="Scottish" | NWEurope$pop=="Welsh" | NWEurope$pop=="Orcadian")


par(mfrow=c(3,4))
#note: multiplying by -1 so that i can plot it in an inverse order
#+1 is so that that y axis=1 matches first hit (45). 
#example: first hit is 45, so -45+1=44.
#because im labelling the axis as c(seq.int( 45,0, -5))), so that 45 is 0, tophits would be assigned to 0, but this is wrong.
#so, bydoing (score*-1)+1 the top hit is then correctly assigned to 1. example (-45+1)=44  and this corresponds to 1 in the y axis.

boxplot((BIsles$score*-1)+1 ~ as.vector(BIsles$pop), las=2, col="darkgreen", ylim=c(0,-45), main="British Isles", yaxt='n')
axis(2, c(seq.int(0,-45 ,-5)),  c(seq.int( 45,0, -5)))
mtext("(0.474 / 0.974", cex=0.6)


boxplot((NWEurope$score*-1)+1 ~ as.vector(NWEurope$pop), las=2, col="#4daf4a", ylim=c(0,-45), main="NW Europe", yaxt='n')
axis(2, c(seq.int(0,-45 ,-5)),  c(seq.int( 45,0, -5)))
mtext("(0.315 / 0.87)", cex=0.6)


boxplot((SEurope$score*-1)+1 ~ as.vector(SEurope$pop), las=2, col="#377eb8", ylim=c(0,-45), main="S Europe", yaxt='n')
axis(2, c(seq.int(0,-45 ,-5)),  c(seq.int( 45,0, -5)))
mtext("(0.340 / 0.991)", cex=0.6)


boxplot((EEurope$score*-1)+1 ~ as.vector(EEurope$pop), las=2, col="#984ea3", ylim=c(0,-45), main="E Europe", yaxt='n')
axis(2, c(seq.int(0,-45 ,-5)),  c(seq.int( 45,0, -5)))
mtext("(0.262 / 0.869)", cex=0.6)


boxplot((WAsia$score*-1)+1 ~ as.vector(WAsia$pop), las=2, col="#a65628", ylim=c(0,-45), main="W Asia", yaxt='n')
axis(2, c(seq.int(0,-45 ,-5)),  c(seq.int( 45,0, -5)))
mtext("(0.556 / 0.827)", cex=0.6)


boxplot((MEast$score*-1)+1 ~ as.vector(MEast$pop), las=2, col="orange", ylim=c(0,-45), main="Middle East", yaxt='n')
axis(2, c(seq.int(0,-45 ,-5)),  c(seq.int( 45,0, -5)))
mtext("(0.088 / 0.394)", cex=0.6)


boxplot((NAfrica$score*-1)+1 ~ as.vector(NAfrica$pop), las=2, col="red", ylim=c(0,-45), main="N Africa", yaxt='n')
axis(2, c(seq.int(0,-45 ,-5)),  c(seq.int( 45,0, -5)))
mtext("(0.439 / 0.5)", cex=0.6)


dev.off()
