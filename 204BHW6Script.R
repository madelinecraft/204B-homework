getwd()
setwd("/Users/madelinecraft/Desktop/204B")
mydata<-read.csv("cinteraction.csv")
head(mydata)

plot(mydata$fluidint, mydata$time, xlab="fluidint", ylab="time", main="fluid intelligence")
#relationship between fluidint (x1) and time is negative...logarithmic? linear? quadratic?

plot(mydata$relevantknow, mydata$time, xlab="relevantknow", ylab="time", main="relevant knowledge")
#relationship between relevantknow (x2) and time is negative...cosine? linear? quadratic?

####
m1x1<-lm(time~fluidint, data=mydata)
summary(m1x1)
m2x1<-lm(time~fluidint +I(fluidint^2), data=mydata)
summary(m2x1)
c=coef(m2x1)
curve(c[1]+c[2]*x+c[3]*x^2, add=TRUE)
anova(m1x1, m2x1)
#favors a linear relationship

m3x1<-nls(time~a+b*log(fluidint), start=list(a=8, b=-1), data=mydata)
summary(m3x1)
AIC(m1x1);AIC(m3x1)
#favors model 1
BIC(m1x1);BIC(m3x1)
#favors model 1
####relationship appears to be negative linear

m1x2<-lm(time~relevantknow, data=mydata)
summary(m1x2)
m2x2<-lm(time~relevantknow +I(relevantknow^2), data=mydata)
summary(m2x2)
c=coef(m2x2)
curve(c[1]+c[2]*x+c[3]*x^2, add=TRUE)
anova(m1x2, m2x2)
#favors a linear relationship

m3x2<-nls(time~a+b*sin(relevantknow), start=list(a=8, b=6), data=mydata)
c=coef(m3x2)
curve(c[1]+sin(x),add=TRUE)
AIC(m1x2);AIC(m3x2)
#favors a cosine relationship
BIC(m1x2);BIC(m3x2)
#favors a cosine relationship
####relationship appears to be cosine

##expectedtime=12.5819-0.0213*(mydata$fluidint)+0.1321*(mydata$relevantknow)-0.0173*(mydata$fluidint*mydata$relevantknow)

##expectedtime_lowtime=12.5819-0.0213*(85)+0.1321*(mydata$relevantknow)-0.0173*(85*mydata$relevantknow)

##expectedtime_meantime=12.5819-0.0213*(100)+0.1321*(mydata$relevantknow)-0.0173*(100*mydata$relevantknow)

##expectedtime_hightime=12.5819-0.0213*(115)+0.1321*(mydata$relevantknow)-0.0173*(115*mydata$relevantknow)

mod1<-lm(time~fluidint+relevantknow, data=mydata)
summary(mod1)
mod2<-lm(time~relevantknow+fluidint+relevantknow*fluidint, data=mydata)
summary(mod2)
lowtime=-1*sd(mydata$relevantknow)+mean(mydata$relevantknow)
lowtime
meantime=mean(mydata$relevantknow)
meantime
hightime=sd(mydata$relevantknow)+mean(mydata$relevantknow)
hightime

coef(mod2)
exp_low <- coef(mod2)[1] + coef(mod2)[2]*lowtime + coef(mod2)[3]*mydata$fluidint + coef(mod2)[4]*lowtime*mydata$fluidint

exp_mean <- coef(mod2)[1] + coef(mod2)[2]*meantime + coef(mod2)[3]*mydata$fluidint + coef(mod2)[4]*meantime*mydata$fluidint

exp_high <- coef(mod2)[1] + coef(mod2)[2]*hightime + coef(mod2)[3]*mydata$fluidint + coef(mod2)[4]*hightime*mydata$fluidint

exp_low
exp_mean
exp_high

mydata<-mydata[order(mydata$fluidint),]
head(mydata)

plot(mydata$fluidint, mydata$time, 
	type="n", 
	main="Moderated Associations of Fluid Intelligence and Time", 	
	ylab="Association of Fluid Intelligence and Time", 
	xlab="Fluid Intelligence Scores")
	
lines(mydata$fluidint, exp_low, lwd=3, col="blue")
lines(mydata$fluidint, exp_mean, lwd=3, col="red")
lines(mydata$fluidint, exp_high, lwd=3, col="green")
lines(c(0,0),c(0,10))
legend(130,9,lty=c(1,1,1),col=c("blue","red","green"),legend=c("-1SD","0SD","1SD"))







