getwd()
mydata<-read.csv("nonlinearhw.csv")
head(mydata)
par(mfrow=c(1,3))
plot(mydata$x1, mydata$y1, main="y1")
plot(mydata$x1, mydata$y2, main="y2")
plot(mydata$x1, mydata$y3, main="y3")

#START Y1--negative logarithmic 
m1y1<-lm(y1~x1,data=mydata)
m2y1<-lm(y1~x1+I(x1^2),data=mydata)
anova(m1y1,m2y1)
#m2y1 fits better
plot(mydata$x1, mydata$y1, main="y1")
abline(lm(formula=lm(y1~x1,data=mydata)),col="red")
c=coef(m2y1)
curve(c[1]+c[2]*x+c[3]*x^2, add=TRUE,lty=1,col="purple")
#####
plot(mydata$x1, mydata$y1, main="y1")
m3y1<-nls(y1~a+b*log(x1), start=list(a=6, b=-1), data=mydata)
c=coef(m3y1)
curve(c[1]+c[2]*log(x),add=TRUE,lty=1,col="green")
#####
AIC(m2y1);AIC(m3y1)
BIC(m2y1);BIC(m3y1)
#m3y1 fits best

#START Y2--sine curve 
m1y2<-lm(y2~x1,data=mydata)
m2y2<-nls(y2~a+sin(x1),start=list(a=5),data=mydata)
c=coef(m2y2)
plot(mydata$x1, mydata$y2, main="y2")
abline(lm(formula=lm(y2~x1,data=mydata)),col="green")
curve(c[1] + sin(x), add=TRUE, lty=1, col="red")
AIC(m1y2);AIC(m2y2)
BIC(m1y2);BIC(m2y2)
#m2y2 fits better

#START Y3--positive logarithmic 
m1y3<-lm(y3~x1,data=mydata)
m2y3<-lm(y3~x1+I(x1*x1),data=mydata)
anova(m1y3,m2y3)
#m2y3 fits better
abline(lm(formula=lm(y3~x1,data=mydata)),col="red")
c=coef(m2y3)
plot(mydata$x1, mydata$y3, main="y3")
curve(c[1]+c[2]*x+c[3]*x^2,add=TRUE,lty=1,col="purple")
#####
m3y3<-nls(y3~a+b*log(x1), start=list(a=1, b=1), data=mydata)
c=coef(m3y3)
plot(mydata$x1, mydata$y3, main="y3")
curve(c[1]+c[2]*log(x),add=TRUE,lty=1,col="green")
#####
AIC(m2y3);AIC(m3y3)
BIC(m2y3);BIC(m3y3)
#m2y3 fits best