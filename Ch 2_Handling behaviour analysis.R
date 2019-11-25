#==== Setup & Packages ====

#WD set automatically to project file

#Packages
install.packages("TraMineR")
library("TraMineR") #For sequence visualisation
install.packages("pavo")
library(pavo) #For sequence pattern analysis
library(lme4)#For sequence comparison analysis
library(RColorBrewer) #For colours on plots
library(car) #For visualising blind scorer comparison

#==== 1: TraMineR Sequence Visualisation ====
#KATYDID data
katydid.data<-read.csv("katy_behaviour sequences.csv")
head(katydid.data)
View(katydid.data)

#reading the data into TraMineR format (STS) 
seq.k<-seqdef(data=katydid.data, var=6:71, informat="STS", id="auto") 
summary(seq.k)
#Var=X:Y tells it which columns the behaviour states are in and from that it builds the alphabet
#'id=auto' names the row names automatically
#Format is STS = State Sequence, basically, tells TraMineR to read the sequences as strings of behaviours

#palette set up
#we have 6 behaviours so the palette has to be 6 strong
cpal(seq.k)
#that's a nasty palette let's do this instead
cpal(seq.k) <- c(Dismember="#2dbd1b", Eat="black", Finish="grey", Peck="#4472c4", Reorient="#c40000", Thrash="#ffbf00")
cpal(seq.k)
#The legend colours for all the images
seqlegend(seq.k, ncol=1)

#SEQUENCE PLOTTING

#plot of ALL sequence
seqIplot(seq.k, border=NA, with.legend=FALSE)
#this is exported to a folder to be broken into individual sequences for PAVO

#plot of the most common sequences
seqplot(seq.k, type="f", border=NA, with.legend=FALSE, main="Sequence Frequency",)
#bar thickness indicades the frequency of that sequence
#Thick bar = this happened a lot, thin bar = didn't occur a lot
#pecking it a few times and then leaving it (FINISH) were the most common


#DIVIDING BY FAMILIARITY
#there is a way to do this in TraMineR
#unfortunately this does strip the sequences of their ID information
#so we can't see which birds and treatments they are
#and thusly we can't divide it by familiarity, not easily
#I'm using the same dataset just sorted by sequence length, purely for visual purposes
sorted.data<-read.csv('katy_behaviour sequences_SORTED.csv')
names(sorted.data)[names(sorted.data) == "X1"] <- "sum"

#naive bird sequences
naive.data<-subset(sorted.data, sorted.data$treatment=="n")

seq.kn<-seqdef(data=naive.data, var=7:72, informat="STS", id="auto") 
summary(seq.kn)

cpal(seq.kn) <- c(Dismember="#2dbd1b", Eat="black", Finish="grey", Peck="#4472c4", Reorient="#c40000", Thrash="#ffbf00")
cpal(seq.kn)

seqIplot(seq.kn, border=TRUE, with.legend=FALSE, main="Katydid handling by naive birds", xlab="Number of behaviours", ylab="Individual Sequences (n=23)", axes=F, cex.axis=0.5)
axis(1, at=c(1:66)-1, cex.axis=0.7)

#familiar bird sequences
familiar.data<-subset(sorted.data, sorted.data$treatment=="t")

seq.kf<-seqdef(data=familiar.data, var=7:72, informat="STS", id="auto") 
summary(seq.kf)

cpal(seq.kf) <- c(Dismember="#2dbd1b", Eat="black", Finish="grey", Peck="#4472c4", Reorient="#c40000", Thrash="#ffbf00")
cpal(seq.kf)

seqIplot(seq.kf, border=TRUE, with.legend=FALSE, main="Katydid handling by familiar birds", xlab="Number of behaviours", ylab="Individual Sequences (n=16)", axes=F, cex.axis=0.5)
axis(1, at=c(1:67)-1, cex.axis=0.5)

data
cricket.data<-read.csv("cricket_behaviour sequences first.csv")
sortedC.data<-read.csv("cricket_behaviour sequences first_SORTED.csv")

#CRICKET/Grasshopper data
head(cricket.data)
View(cricket.data)
#reading the data into TraMineR format (STS) and setting it up 
seq.c<-seqdef(data=cricket.data, var=7:18, informat="STS", id="auto") 
summary(seq.c)

#changing colours
#the order is eat, finish, peck ,reorient, thrash
cpal(seq.c) <- c(Eat="black", Finish="grey", Peck="#4472c4", Reorient="#c40000", Thrash="#ffbf00")
seqlegend(seq.c)

#plotting full sequences for pavo analysis
seqIplot(seq.c, border=NA, legend=FALSE) #title and legend suppressed to export at a good size with not distortion

#plotting ordered sequence for figures
seq.cS<-seqdef(data=sortedC.data, var=8:19, informat="STS", id="auto") 
summary(seq.cS)

cpal(seq.cS) <- c(Eat="black", Finish="grey", Peck="#4472c4", Reorient="#c40000", Thrash="#ffbf00")
seqlegend(seq.cS)

seqIplot(seq.cS, border=TRUE, with.legend=FALSE, main="Cricket handling", xlab="Number of behaviours", ylab="Individual Sequences (n=26)", axes=F)
axis(1, at=c(1:13)-1, cex.axis=0.5)
#no need to divide in to naive or familiar because all birds are considered familiar with cricket/grasshopper

#PLASTECINE BALL data
ball.data<-read.csv("ball_behaviour sequences first.csv")
sortedB.data<-read.csv("ball_behaviour sequences SORTED.csv")

#code to TraMineR sequence
seq.b<-seqdef(data=ball.data, var=6:11, informat="STS", id="auto")
summary(seq.b)

#checking legend and tweaking colours to be consistent
cpal(seq.b) <- c(Finish="grey", Peck="#4472c4", Reorient="#c40000")
seqlegend(seq.b)

#plotting 
seqIplot(seq.b, border=NA, with.legend=FALSE)


seq.bS<-seqdef(data=sortedB.data, var=6:11, informat="STS", id="auto") 
summary(seq.bS)

cpal(seq.bS) <- c(Finish="grey", Peck="#4472c4", Reorient="#c40000")
seqlegend(seq.bS)

seqIplot(seq.bS, border=TRUE, with.legend=FALSE, main="Plastecine ball handling", xlab="Number of behaviours", ylab="Individual Sequences (n=26)", axes=F, cex.axis=0.9)
axis(1, at=c(1:13)-1, cex.axis=0.5)

#==== 2: PAVO Sequence pattern analysis ====
#Loading the sequence images
katy<-getimg("/sequences/katydid")
cricket<-getimg("/sequences/cricket")
ball<-getimg("/sequences/balls")

#Now classifying colours
#When selecting colour classes, all classes will be selected in the same set order:
# 1- peck (blue) / 2- thrash (yellow) / 3- dismember (green) / 4- reorient (red)
#if a behaviour isn't present it's skipped to the next one in the order
katydid_class_INT <- classify(katy, interactive = TRUE, plotnew = TRUE)

cricket_class_INT <- classify(cricket, interactive = TRUE, plotnew = TRUE)

ball_class_INT <- classify(ball, interactive = TRUE, plotnew = TRUE)


#writing pdf output - THIS MAY TAKE A WHILE!
pdf("katydid_class_INT_realcol.pdf")
summary(katydid_class_INT, plot=TRUE)
dev.off()#<- remember to call this to stop writing to pdf

pdf("cricket_class_INT_realcol.pdf")
summary(cricket_class_INT, plot=TRUE)
dev.off()

pdf("ball_class_INT_realcol.pdf")
summary(ball_class_INT, plot=TRUE)
dev.off() 

#writing output CSV
katydid_adjacent_INT <- adjacent(katydid_class_INT, xpts = 250, xscale = 300) #xpts tells it how many points to sample along across the image, the images are all 500px so it's every second pixel
write.csv(katydid_adjacent_INT, 'output_katydid.csv')

cricket_adjacent_INT <- adjacent(cricket_class_INT, xpts = 250, xscale = 300)
write.csv(cricket_adjacent_INT, 'output_cricket.csv')


ball_adjacent_INT <- adjacent(ball_class_INT, xpts = 250, xscale = 300)
write.csv(ball_adjacent_INT, 'output_ball.csv')

#==== 3:Model making & regressions ====
#logreg of transitions csv generated above
#treatment and consume columns have been added to datasheet but that's all
#otherwise csv is as is generated above

#reading in csv
data.katydid<-read.csv('output_katydid.csv')
data.cricket<-read.csv('output_cricket.csv')
data.ball<-read.csv('output_ball.csv')

#consume values need to be read as numerics
data.katydid$consume <- as.numeric(data.katydid$consume)
data.cricket$consume <- as.numeric(data.cricket$consume)
data.ball$consume <- as.numeric(data.ball$consume)


#KATYDID
colnames(data.katydid)

katy1 <- glmer(consume ~ treatment + (1|bird_ID), data=data.katydid, family=binomial(link="logit"))
summary(katy1)
#that's the raw difference in likliehood of consumption by treatment

#Consume vs JC (how many behaviours shown)
katyJC <- glmer(consume ~ Jc + (1|bird_ID), data=data.katydid, family=binomial(link="logit"))
summary(katyJC)

katyJCT <- glmer(consume ~ Jc * treatment + (1|bird_ID), data=data.katydid, family=binomial(link="logit"))
summary(katyJCT)


#Consume vs JT (How many times a bird switches behaviours)
katyJT <- glmer(consume ~ Jt + (1|bird_ID), data=data.katydid, family="binomial")
summary(katyJT)

katyJTT <- glmer(consume ~ Jt * treatment + (1|bird_ID), data=data.katydid, family="binomial")
summary(katyJTT)

#Consume vs M (Overall complexity as a measure of JT + JC)
katyM <- glmer(consume ~ m + (1|bird_ID), data=data.katydid, family="binomial")
summary(katyM)

#CRICKET
#Consume vs JC (how many behaviours shown)
cricketJC <- glmer(consume ~ Jc + (1|bird_ID), data=data.cricket, family=binomial(link="logit"))
summary(cricketJC)

#Consume vs JT (How many times a bird switches behaviours)
cricketJT <- glmer(consume ~ Jt + (1|bird_ID), data=data.cricket, family="binomial")
summary(cricketJT)

#Consume vs M (Overall complexity as a measure of JT + JC)
cricketM <- glmer(consume ~ m + (1|bird_ID), data=data.cricket, family="binomial")
summary(cricketM)
#the estimate for complexity is the best fit but because there was only 1 bird that didn't eat it, it's blowing it out, so these aren't very good to look at

#instead, let's do a simple descriptive look at M to get the range and median
summary(data.cricket$m)
IQR(data.cricket$m)
sd(data.cricket$m)
plot(data.cricket$treatment, data.cricket$m,ylim=c(0,0.030), ylab="Overall sequence complexity (m)", xlab="Behaviours (k)", col=brewer.pal(n = 3, name = "Set1"))

#PLASTECINE BALL
#there's only 3 responses and the high similarity confounds any attempt to analyse it

#==== 5:Plotting comparisons ====
#I would like to plot the complexity scores per treatment/per consume
#so I'm subsetting by bird familiarity group
#and using the sorted data from the TraMineR sequences so I can use 'sum'
#plotting average sequence length per consume/treatment
names(sorted.data)[names(sorted.data) == "X1"] <- "sum"
sorted.data$consumeYN<-as.factor(sorted.data$consumeYN)

#testing for differences in sequence length (sum)
wilcox.test(sum ~ treatment, data=sorted.data)
#p=0.1295, there is no significant difference in sequence length between familiar/experienced birds

wilcox.test(sum ~ consumeYN, data=sorted.data)
#p=<0.001 the shorter the sequence, the less likely to consume for both groups

#what about between treatment groups when they consumed/did not consume katydids?
#subsetting
data.consumeS <- subset(sorted.data, sorted.data$consume=="1")
data.finishS <- subset(sorted.data, sorted.data$consume=="0")

wilcox.test(sum ~ treatment, data=data.consumeS)
#p=0.757, there is no difference in sequence length of birds that consumed the katydids either familiar or naive

wilcox.test(sum ~ treatment, data=data.finishS)
#p=0.9 there is no difference in sequence lenth in birds that did not consume the katydid (famil/naive)

#need to subset the katydid pattern metrics output so we can plot M against Consumption
kconsume <- subset(data.katydid, data.katydid$consume=="1")
kfinish <- subset(data.katydid, data.katydid$consume=="0")

par(mfrow=c(2,2))

plot(data.consumeS$treatment, data.consumeS$sum, ylim=c(0,70), ylab="Behaviour sequence length", xlab="Familiarity", main="Consume", col=brewer.pal(n = 3, name = "Set1"))

plot(data.finishS$treatment, data.finishS$sum, ylim=c(0,70), ylab="Behaviour sequence length", xlab="Familiarity", main="No Consume", col=brewer.pal(n = 3, name = "Set1"))


plot(kconsume$treatment, kconsume$m,ylim=c(0,0.030), ylab="Overall sequence complexity (m)", xlab="Behaviours (k)", col=brewer.pal(n = 3, name = "Set1"))

plot(kfinish$treatment, kfinish$m, ylim=c(0,0.030), ylab="Overall sequence complexity (m)", xlab="Behaviours (k)", col=brewer.pal(n = 3, name = "Set1"))

#==== 6:Blind Scorer Comparison ====
corr<-read.csv('ab cor.csv')

#testing for the strength and significance of correlation
obs.corr<-cor.test(corr$A, corr$B,  method = "spearman")
obs.corr
#R2=0.08, pval <0.001, no significant difference between A and B scores, they match eachother 
library("car")
scatterplot(B ~ A, data = corr, grid=FALSE, smooth=FALSE, main='Correlation of Behaviours Scored by Observer per Bird', ylab='Blind scorer', xlab='Initial scorer')
