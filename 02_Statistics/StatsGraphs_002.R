#This Codes helps present the accuracies, does statistics on them and graphs.
options(scipen=100)
options(digits=2)

library(easypackages)
libraries("tidyverse","doBy","pastecs","Hmisc")
acc=read.csv(file = "../Data/Acc.csv")
attach(acc)
AccSumm <- summary(Accuracy)
AccStatDesc <- format(stat.desc(Accuracy, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95), scientific = FALSE)
latex(AccStatDesc, file= "Accuracy Descprition 2.tex")
latex(AccSumm, file= "Accuracy Summary.tex")



#creating boxplots in pdf and jpeg
pdf(file="../Results/AccuracyBoxplot.pdf")
boxplot(Accuracy, xlab="Accuracy")
dev.off()
jpeg(file="../Results/AccuracyBoxplot.jpg")
boxplot(Accuracy, xlab="Accuracy")
dev.off()

#creating histograms in pdf and jpeg
pdf(file="../Results/hist.pdf")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
dev.off()
jpeg(file="../Results/hist.jpg")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
dev.off()


#creating histograms in pdf and jpeg with normal curve
pdf(file="../Results/hist with normal curve.pdf")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
curve(dnorm(x, mean=mean(Accuracy), sd=sd(Accuracy)), add=TRUE, col="darkblue", lwd=2)
dev.off()
jpeg(file="../Results/hist with normal curve.jpg")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
curve(dnorm(x, mean=mean(Accuracy), sd=sd(Accuracy)), add=TRUE, col="darkblue", lwd=2)
dev.off()

#creating histograms in pdf and jpeg with fitting curve
pdf(file="../Results/hist with fitting curve.pdf")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
lines(density(Accuracy), col="blue", lwd=2)
dev.off()
jpeg(file="../Results/hist with fitting curve.jpg")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
lines(density(Accuracy), col="blue", lwd=2)
dev.off()


#creating histograms in pdf and jpeg with Both curve
pdf(file="../Results/hist with Both curve.pdf")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
lines(density(Accuracy), col="blue", lwd=2)
curve(dnorm(x, mean=mean(Accuracy), sd=sd(Accuracy)), add=TRUE, col="darkblue", lwd=2)
dev.off()
jpeg(file="../Results/hist with Both curve.jpg")
hist(Accuracy, xlab="Accuracy",freq=FALSE, xlim=c(min(Accuracy)-.0131,max(Accuracy)+.0131),  ylim=c(0, 30), breaks = 10)
lines(density(Accuracy), col="blue", lwd=2)
curve(dnorm(x, mean=mean(Accuracy), sd=sd(Accuracy)), add=TRUE, col="darkblue", lwd=2)
dev.off()

write.csv(AccSumm, file="Summary.csv")
write.csv(AccStatDesc, file = "../Results/Statistical_Description.csv")


