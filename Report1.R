#Report 1
#Yixiao Zhao
#301322030

rm(list=ls())
graphics.off()

#read data and do the plot
data<- read.csv("~/Desktop/report1/XTEITT01CNM156S.csv")
data.ts<- ts(data$XTEITT01CNM156S,start=c(1992,1),frequency=12)
plot(data.ts,ylab="percent", xlab="Year",main="Ratio of Exports to Import for China")

#linear regression model fit with output 
fit<-lm(data.ts~time(data.ts))
summary(fit)
plot(data.ts,ylab="Percent",xlab="Year",type="o",main="Ration of Export to Import for China")
abline(fit)

#residuals from the linear regression model fit 
residual<-ts(resid(fit),start=c(1992,1),frequency = 12)
plot(residual, ylab="Residual",xlab="Year")
 
#Standardised residuals
#Histogram and QQ plot
hist(data.ts,main = "Histogram of residual",xlab="Studentized residuals")
qqnorm(data.ts,main="Q-Q plot of studentized residuals")
qqline(rstudent(fit))

#Shapiro-wilk test for normailty
shapiro.test(rstudent(fit))

#Sample ACF for studentized residuals
acf(data.ts,main="Sample ACF for studentized residuals")

#Run test
install.packages("tseries")
library(tseries)
runs.test(factor(rstudent(fit)>0))

#BDS Test
install.packages("fNonlinear")
library(fNonlinear)
bdsTest(residual)




