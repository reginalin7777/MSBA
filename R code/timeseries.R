
harmon=read.csv('Harmon.csv',sep=',',header=T)
attach(harmon)

#TimePlot of cases
TimeIndex=seq(1,60)
plot(TimeIndex,Caseship, main='TimePlot of Cases')

#by sample mean
library(psych) 
describe(harmon[2],skew=F)


#by regression
reg.fit=lm(formula='Caseship~TimeIndex',data=as.data.frame(cbind(TimeIndex,Caseship)))
summary(reg.fit)

#prediction
testdata=as.data.frame(seq(61,61))
colnames(testdata)='TimeIndex'
rownames(testdata)=seq(61,61)
predict(reg.fit,testdata,se.fit=T,interval = "prediction")


#Extra vairable prep.
harmon$SIndexAll=rep(SIndex[1:12],5)
harmon$desCases=Caseship/(harmon$SIndexAll/100)
library(Hmisc)
harmon$CP_Lag1=Lag(Conpacks,1)
harmon$CP_Lag2=Lag(Conpacks,2)
harmon$DA_Lag1=Lag(DealAl,1)
harmon$DA_Lag2=Lag(DealAl,2)

#by multiple regression
mreg.fit=lm('desCases~Conpacks+DealAl+TimeIndex+CP_Lag1+CP_Lag2+DA_Lag1+DA_Lag2',data=harmon)
summary(mreg.fit)

#Diagnostic plots
stdres=rstandard(mreg.fit)
par(mfrow=c(2,2))
qqnorm(stdres,main='Normal Probability Plot',xlab='Normal Quantiles',ylab='Standardized Residual Quantiles')
abline(0,1)
plot(mreg.fit$fitted.values,stdres,main='Versus Fits',xlab='Fitted Value',ylab='Standardized Residual')
abline(0,0,lty=3)
hist(stdres,main='Histogram',xlab='Standardized Residual')
plot(TimeIndex[-1:-12],stdres,type="o",main='Versus Order',xlab='TimeIndex',ylab='Standardized Residual')
abline(0,0,lty=3)
par(mfrow=c(1,1))
#BP test
library(car)
ncvTest(mreg.fit)

#vif
vif(mreg.fit)

#prediction
rm(testdata)
testdata=as.data.frame(t(c(200000,120000,71881,234562,552536,376556,61)))
colnames(testdata)=c(colnames(harmon)[3:4],colnames(harmon)[7:10],'TimeIndex')
rownames(testdata)=seq(61,61)
predict(mreg.fit,testdata,se.fit=T,interval = "prediction")

#Cochrane-Orcutt procedure [Testing coefficient for residuals regression]
summary(lm(mreg.fit$residuals~Lag(mreg.fit$residuals,1)))



#ARIMA
Caseship13=Caseship[13:60]

plot(as.ts(Caseship13),main='Timeplot of Caseship', ylab='CaseShip')
library(tseries)
adf.test(Caseship13)

par(mfrow=c(2,1))
acf(Caseship13,main='ACF')
pacf(Caseship13,main='PACF')
par(mfrow=c(1,1))

#fitting AR(1) (or ARIMA(1,0,0))
ar1=arima(Caseship13,order=c(1,0,0))
ar1

#auto fit by R
#library(forecast)
auto.arima(Caseship13)

#Residuals diagnostic
tsdisplay(ar1$residuals,main='Residuals Diagnostic')
#prediction for next period
forecast(ar1,level=95,h=1)
#longer forecast plot
plot(forecast(ar1,h=10))

#Error measure
#mreg
n=length(Caseship13)
msd_mreg=sum((mreg.fit$residuals)^2)/n
mad_mreg=sum(abs(mreg.fit$residuals))/n
mape_mreg=sum(abs(mreg.fit$residuals/Caseship13))/n*100
round(c(msd_mreg,mad_mreg,mape_mreg))

#ar1
msd_ar1=sum((ar1$residuals)^2)/n
mad_ar1=sum(abs(ar1$residuals))/n
mape_ar1=sum(abs(ar1$residuals/Caseship13))/n*100
round(c(msd_ar1,mad_ar1,mape_ar1))

