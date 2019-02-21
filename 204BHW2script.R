getwd()
setwd("~/Desktop/204B")
getwd()
mydata<-read.csv("scatterplot.csv")
head(mydata)
summary(mydata)
sapply(mydata,sd)
##########
par(mfrow=c(1,3))
hist(mydata$SES,xlab="Socioeconomic Status (SES)", main="Histogram of SES")
hist(mydata$PEI,xlab="Perceptions of Educational Importance (PEI)", main="Histogram of PEI")
hist(mydata$LGA,xlab="Life-Goals Achievement", main="Histogram of LGA")
###########
par(mfrow=c(1,3))
plot(mydata$PEI, mydata$LGA, ylab="Life-Goal Achievement (LGA)", xlab="Perceptions of Educational Importance (PEI)", main="Relationship Between LGA and PEI")

lines(abline(lm(LGA ~ PEI, data = mydata)))

plot(mydata$SES, mydata$LGA, ylab="Life-Goal Achievement (LGA)", xlab="Socioeconomic Status (SES)", main="Relationship Between LGA and SES")

lines(abline(lm(mydata$LGA~mydata$SES)))

plot(mydata$PEI, mydata$SES, xlab="Perceptions of Educational Importance (PEI)", ylab="Socioeconomic Status (SES)", main="Relationship Between PEI and SES")
lines(abline(lm(mydata$PEI~mydata$SES)))
##########
mod1<-lm(LGA~PEI,data=mydata)
mod2<-lm(LGA~SES,data=mydata)
mod3<-lm(LGA~PEI+SES,data=mydata)
coef(summary(mod1))
coef(summary(mod2))
coef(summary(mod3))

y1<-mydata$LGA
x1<-mydata$PEI
x2<-mydata$SES

par(mfrow = c(1,2))
plot(x2,resid(lm(y1 ~ x1 + I(x1^2) + I(x1^3))))
lines(lowess(x2,resid(lm(y1 ~ x1 + I(x1^2) + I(x1^3))),f = .4), lwd = 3, col = "red")
plot(x2,resid(lm(y1 ~ x1 + I(x1^2))))
lines(lowess(x2,resid(lm(y1 ~ x1 + I(x1^2))),f = .4), lwd = 3, col = "red")

par(mfrow=c(1,2))
plot(x1,resid(lm(y1 ~ x2 + I(x2^2) + I(x2^3))))
lines(lowess(x1,resid(lm(y1 ~ x2 + I(x2^2) + I(x2^3))),f = .4), lwd = 3, col = "red")
plot(x1,resid(lm(y1 ~ x2 + I(x2^2))))
lines(lowess(x1,resid(lm(y1 ~ x2 + I(x1^2))),f = .4), lwd = 3, col = "red")