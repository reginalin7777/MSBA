#load data
nopane=read.csv('Nopane.csv') 

#Regression 1
reg1=lm(UnitSales~NopaneAdDollars+CompetitionAdExpend+DumSegment+DumCopy,data=nopane)
summary(reg1)
#residuals plots for checking model assumption
par(mfrow=c(1,2))
plot(reg1,which=1:2)


#Regression 3
reg3=lm(UnitSales~NopaneAdDollars+DumSegment+DumCopy,data=nopane)
summary(reg3)
par(mfrow=c(1,2))
plot(reg3,which=1:2)

#Modified Regression 1
reg1_mod=lm(UnitSales~CompetitionAdExpend+NopaneAdDollars*DumSegment+NopaneAdDollars*DumCopy+DumSegment*DumCopy,data=nopane)
summary(reg1_mod)
par(mfrow=c(1,2))
plot(reg1_mod,which=1:2)

#Modified Regression 3
reg3_mod=lm(UnitSales~NopaneAdDollars*DumSegment+NopaneAdDollars*DumCopy+DumSegment*DumCopy,data=nopane)
summary(reg3_mod)
par(mfrow=c(1,2))
plot(reg3_mod,which=1:2)