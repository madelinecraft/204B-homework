getwd()
setwd("~/Desktop")
getwd()
mydata<-read.csv("happy.csv")
head(mydata)
summary(mydata)
sapply(mydata,sd)
table(mydata$lifegreat)
table(mydata$trauma)

install.packages('MASS')
library(MASS)
cor(mydata)
cor.test(mydata$happy, mydata$excited)$p.value
cor.test(mydata$happy, mydata$electric)$p.value
cor.test(mydata$happy, mydata$jubilant)$p.value

cor.test(mydata$excited, mydata$electric)$p.value
cor.test(mydata$excited, mydata$jubilant)$p.value

cor.test(mydata$electric, mydata$jubilant)$p.value

install.packages('ltm')
library(ltm)
mydata$lifegreat=as.factor(mydata$lifegreat)
mydata$trauma=as.factor(mydata$trauma)
biserial.cor(mydata$happy,mydata$lifegreat)
biserial.cor(mydata$excited,mydata$lifegreat)
biserial.cor(mydata$electric,mydata$lifegreat)
biserial.cor(mydata$jubilant,mydata$lifegreat)


biserial.cor(mydata$happy,mydata$trauma)
biserial.cor(mydata$excited,mydata$trauma)
biserial.cor(mydata$electric,mydata$trauma)
biserial.cor(mydata$jubilant,mydata$trauma)

table(mydata$lifegreat, mydata$trauma)
matrix=matrix(c(29,72,76,23),nrow=2,ncol=2,byrow=TRUE)
install.packages('psych')
library(psych)
phi(matrix,digits=3)