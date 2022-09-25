
ins=read.csv('insurance.csv',sep=',',header=T)
attach(ins)

#summary statistics of the nemerical variales
library(psych) 
describe(cbind(ins[1:2],ins[7]))

#bar chart of the categorical data
plot(VehAge,las=1,main='Bar Chart of VehAge')
plot(Gender,las=1,main='Bar Chart of Gender')
plot(VehBody,las=2,main='Bar Chart of VehBody')
par(mar=c(8, 4, 4, 2))
plot(DrivAge,las=2,main='Bar Chart of DrivAge')
par(mar=c(5, 4, 4, 2))

#initial fit of Poisson regression
first.fit=glm(ClaimNb~.,data=ins,family='poisson')

summary(first.fit)

#Checking Overdispersion
c(mean(ins$ClaimNb),var(ins$ClaimNb))
c(mean(ins$ClaimNb/ins$Exposure),var(ins$ClaimNb/ins$Exposure))

#poisson regression with offset
poi.fit=glm(ClaimNb~.-Exposure+offset(log(Exposure)),family='poisson',data=ins)
summary(poi.fit)

#negative biominal regression with offset
library(MASS)
nb.fit=glm.nb(ClaimNb~.-Exposure+offset(log(Exposure)),data=ins)
summary(nb.fit)

#overdispersion test between poisson regression and NB regression
#LR test
library(pscl)
odTest(nb.fit)

#Cameron and Trivedi test
library(AER)
dispersiontest(poi.fit,trafo=2)

#LM test
LM=sum((poi.fit$y-poi.fit$fitted.values)^2-poi.fit$y)^2/(2*sum((poi.fit$fitted.values)^2))
p_LM=1-pchisq(LM,1)
p_LM

#Stepwise selection for NB regression
step_nb.fit=step(nb.fit,scope =list(upper=nb.fit) )

#deviance goodness of fit test
1-pchisq(step_nb.fit$deviance,step_nb.fit$df.residual)

#Testing for interaction terms followed by stewpwise result
temp=update(step_nb.fit,data=ins)
anova(temp)


#exp(beta) with 95% CI
exp(confint(step_nb.fit))
