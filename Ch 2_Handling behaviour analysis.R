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

#naive bird sequences
naive.data<-subset(sorted.data, sorted.data$treatment=="n")
summary(naive.data$sum)
summary(sorted.data$sum)

seq.kn<-seqdef(data=naive.data, var=7:72, informat="STS", id="auto") 
summary(seq.kn)

cpal(seq.kn) <- c(Dismember="#2dbd1b", Eat="black", Finish="grey", Peck="#4472c4", Reorient="#c40000", Thrash="#ffbf00")
cpal(seq.kn)

seqIplot(seq.kn, border=TRUE, with.legend=FALSE, main="Katydid handling by naive birds", xlab="Number of behaviours", ylab="Individual Sequences (n=23)", axes=F, cex.axis=0.5)
axis(1, at=c(1:66)-1, cex.axis=0.7)

#familiar bird sequences
familiar.data<-subset(sorted.data, sorted.data$treatment=="t")
summary(familiar.data$sum)

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

complexF<-subset(data.katydid, data.katydid$treatment =="t")
complexN<-subset(data.katydid, data.katydid$treatment =="n")
summary(complexF$m)
summary(complexN$m)
summary(data.katydid$m)

range(complexF$m)
range(complexN$m)

#consume values need to be read as numerics
data.katydid$consume <- as.numeric(data.katydid$consume)

#KATYDID
katy1 <- glm(consume ~ treatment, data=data.katydid, family=binomial(link="logit"))
summary(katy1)
#that's the raw difference in likliehood of consumption by treatment - familiar birds more likely to consume, this we know from Ch 1


#testing for difference in the number of behaviours between naive and familiar birds?

katysum1 <- glm(sum ~ treatment, data=sorted.data, family=gaussian)
summary(katysum1)
#there is no significant differnce in number of behaviours between treatments, but most sequences are longer (significant positive intercept)


#Does the sequence length impact the liklihood of consumption?
katysum2 <- glm(consumeYN ~ sum, data=sorted.data, family=binomial(link=logit))
summary(katysum2)
#longer sequences = more likely to end in consumption

#How does complexity vary between familia and naive birds?
katym1 <- glm(m ~ treatment, data=data.katydid, family=gaussian)
summary(katym1)
#familiar birds had significantly more complex handling


#Does m influence the likelihood of consumption?
katym2 <- glm(consume ~ m, data=data.katydid, family=binomial(link=logit))
summary(katym2)
#more complex sequences are most likely to lead ot consumption - so increased complexity = increased likilhood of consumption



#CRICKET
#because there was only 1 bird that didn't eat it, it's blowing it out, so these aren't very good to look at

#instead, let's do a simple descriptive look at M to get the range and median
summary(data.cricket$m)
IQR(data.cricket$m)
sd(data.cricket$m)

#PLASTECINE BALL
#there's only 3 responses and the high similarity confounds any attempt to analyse it

#==== 5:Plotting comparisons ====
#I would like to plot the complexity scores per treatment/per consume
#so I'm subsetting by bird familiarity group
#and using the sorted data from the TraMineR sequences so I can use 'sum'

#plotting average sequence length per treatment
plot(sorted.data$treatment, sorted.data$sum,ylim=c(0,70), ylab="Behaviours (n)", xlab="Experience", col=brewer.pal(n = 3, name = "Set1"))


#plotting the complexity per experience
plot(data.katydid$treatment, data.katydid$m,ylim=c(0,0.03), ylab="Complexity (m)", xlab="Experience", col=brewer.pal(n = 3, name = "Set1"))


#plotting the likelihood of consumption against the handling complexity
range(data.katydid$m)
xcomplexity <-seq (0, 0.030, 0.01)
ycomplexity <- predict(katym2, list(m=xcomplexity),type="response")

with(data.katydid,plot(m, consume, ylim=c(0,1), pch = 16, xlab = "Handling Complexity (m)", ylab = "Likelihood of Consumption"))

lines(xcomplexity, ycomplexity, col = "red", lwd = 2)

#generating a range for the complexity fits
m<-seq(0,0.030,length=500)

#predictions using the glm
glm.prd<-predict(katym2,data.frame(independent=m),se=TRUE,type="response")

#plotting upper and lower confidence intervals
lines(m,glm.prd$fit+1.96*glm.prd$se,lty=2,col="red",lwd=2)
lines(m,glm.prd$fit-1.96*glm.prd$se,lty=2,col="red",lwd=2)

#plotting correlation of number of behaviours with likelihood of consumption
range(sorted.data$sum)
xsum <-seq (0, 66, 1)
ysum <- predict(katysum2, list(sum=xsum),type="response")

with(sorted.data,plot(sum, consumeYN, ylim=c(0,1), pch = 16, xlab = "Sequence length (sum n behaviours)", ylab = "Likelihood of Consumption"))

lines(xsum, ysum, col = "red", lwd = 2)

#generating a range for the complexity fits
sum<-seq(0,66,length=500)

#predictions using the glm
glm.prd2<-predict(katysum2,data.frame(independent=sum),se=TRUE,type="response")

#plotting upper and lower confidence intervals
lines(sum,glm.prd2$fit+1.96*glm.prd2$se,lty=2,col="red",lwd=2)
lines(sum,glm.prd2$fit-1.96*glm.prd2$se,lty=2,col="red",lwd=2)




#==== 6:Blind Scorer Comparison ====
corr<-read.csv('ab cor.csv')

#testing for the strength and significance of correlation
obs.corr<-cor.test(corr$A, corr$B,  method = "spearman")
obs.corr
#R2=0.08, pval <0.001, no significant difference between A and B scores, they match eachother 
library("car")
scatterplot(B ~ A, data = corr, grid=FALSE, smooth=FALSE, main='Correlation of Behaviours Scored by Observer per Bird', ylab='Blind scorer', xlab='Initial scorer')
