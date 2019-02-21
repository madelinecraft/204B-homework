getwd()
setwd("/Users/madelinecraft/Desktop/204B")
mydata<-read.table('aids.txt')
mydata$Treatment=factor(mydata$Treatment)
head(mydata)
names=c("ID", "Treatment", "Age", "Male", "Week", "logCD4")
colnames(mydata)=names
head(mydata)
summary(mydata)

set.seed(100)
theseIDs<-sample(unique(mydata$ID),50)
subdata<-mydata[mydata$ID %in% theseIDs,]
subdata
library(ggplot2)
p<-ggplot(data=subdata, aes(x=Week, y=logCD4, group=ID, color=Treatment))+geom_line()+scale_color_gradient(high="pink")
p+geom_line()

library(nlme)
mod1=lme(logCD4~Week+Treatment+Male, random=~1|ID, method="REML", data=mydata)
summary(mod1)
#this only shows random effects for the intercept (sd), no random effects for the predictors

mod2=lme(logCD4~Week+Treatment+Male, random=~1+Week|ID, method="REML", data=mydata)
summary(mod2)

AIC(mod1);AIC(mod2)
BIC(mod1);BIC(mod2)
anova(mod1, mod2)
class(mydata$Treatment)