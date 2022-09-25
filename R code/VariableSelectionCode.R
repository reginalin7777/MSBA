
data=read.csv('FinancialIndicators.csv',header=T,sep=',')
Fin=data[,6:28]
n=dim(Fin)[1]
p=dim(Fin)[2]
train_set=Fin[1:480,]
test_set=Fin[481:n,]

#correlation matrix
library(corrplot)
corrplot(cor(train_set), method = "ellipse",type = "upper")

#least square fit
fit=lm(StockPrice~.,data=train_set)
summary(fit)


#best subset
library(leaps)
best.subset <- regsubsets(StockPrice~.,data=train_set,nvmax=22)
best.subset.summary=summary(best.subset)
best.subset.by.cp=which.min(best.subset.summary$cp)
best.subset.by.adjr2=which.max(best.subset.summary$adjr2)


par(mfrow=c(1,2))
plot(best.subset.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(best.subset.by.adjr2, best.subset.summary$adjr2[best.subset.by.adjr2], col="red", cex =2, pch =20)
plot(best.subset.summary$cp, xlab="Number of Variables", ylab="CP", type="l")
points(best.subset.by.cp, best.subset.summary$cp[best.subset.by.cp], col="red", cex =2, pch =20)

library(HH)
best.subset.fit=lm.regsubsets(best.subset,8)
summary(best.subset.fit)

#stepwise selection method (AIC)/start from full model
step_var=step(fit,scope = list(upper=fit))
step.summary=summary(step_var)
#forward setup
# null_m=lm(StockPrice~1,data=train_set)
# step(null_m,scope = list(upper=fit),direction='forward')


#PCR
library(pls)
pcr.model=pcr(StockPrice~., data = train_set, scale = TRUE, validation = "CV")
summary(pcr.model)
validationplot(pcr.model, val.type="RMSEP",legend='topleft')
summary(lm(train_set[,1]~pcr.model$scores[,1:11]))
pcr.fit=pcr(StockPrice~.,ncomp=11, data = train_set, scale = TRUE, validation = "CV")
summary(pcr.fit)

#LASSO
library(glmnet)
lasso.cv=cv.glmnet(as.matrix(train_set[-1]), as.matrix(train_set[,1]),alpha=1,family="gaussian", nfolds=10)
plot(lasso.cv)
plot(glmnet(as.matrix(train_set[-1]), as.matrix(train_set[,1]),alpha=1,family="gaussian"),xvar='lambda')
lasso.fit = glmnet(as.matrix(train_set[-1]), as.matrix(train_set[,1]),alpha=1,family="gaussian",lambda=lasso.cv$lambda.min)
lasso.fit$beta

#correlation between a subset of the variables
subset_var=c(1,7,13,14,18,20,21)
library("PerformanceAnalytics")
chart.Correlation(train_set[,subset_var], histogram=TRUE)

# log transformation
log_train_set=log(train_set+0.000001)
log_test_set=log(test_set+0.000001)
names(log_train_set)=paste('log',names(log_train_set),sep='')
names(log_test_set)=paste('log',names(log_test_set),sep='')
corrplot(cor(log_train_set), method = "ellipse",type = "upper")
chart.Correlation(log_train_set[,subset_var], histogram=TRUE)
chart.Correlation(log_train_set, histogram=TRUE)

#best subset
log_best.subset <- regsubsets(logStockPrice~.,data=log_train_set,nvmax=22)
log_best.subset.summary=summary(log_best.subset)
log_best.subset.by.cp=which.min(log_best.subset.summary$cp)
log_best.subset.by.adjr2=which.max(log_best.subset.summary$adjr2)

par(mfrow=c(1,2))
plot(log_best.subset.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(log_best.subset.by.adjr2, log_best.subset.summary$adjr2[log_best.subset.by.adjr2], col="red", cex =2, pch =20)
plot(log_best.subset.summary$cp, xlab="Number of Variables", ylab="CP", type="l")
points(log_best.subset.by.cp, log_best.subset.summary$cp[log_best.subset.by.cp], col="red", cex =2, pch =20)

log_best.subset.fit=lm.regsubsets(log_best.subset,10)
summary(log_best.subset.fit)

#stepwise
log_fit=lm(logStockPrice~.,data=log_train_set)
summary(log_fit)
log_step_var=step(log_fit,scope=list(upper=log_fit))
summary(log_step_var)

#pcr
log_pcr.model=pcr(logStockPrice~., data = log_train_set, scale = TRUE, validation = "CV")
summary(log_pcr.model)
validationplot(log_pcr.model, val.type="RMSEP",legend='topleft')
summary(lm(log_train_set[,1]~log_pcr.model$scores[,1:10]))
log_pcr.fit=pcr(logStockPrice~.,ncomp=10, data = log_train_set, scale = TRUE, validation = "CV")
summary(log_pcr.fit)


#LASSO
#library(glmnet)
log_lasso.cv=cv.glmnet(as.matrix(log_train_set[-1]), as.matrix(log_train_set[,1]),alpha=1,family="gaussian",nfolds=10)
plot(log_lasso.cv)
plot(glmnet(as.matrix(log_train_set[-1]), as.matrix(log_train_set[,1]),alpha=1,family="gaussian"),xvar='lambda')
log_lasso.fit = glmnet(as.matrix(log_train_set[-1]), as.matrix(log_train_set[,1]),alpha=1,family="gaussian",lambda=log_lasso.cv$lambda.min)
log_lasso.fit$beta



#predictive mse

log_mse_best.subset=mean((predict(log_best.subset.fit,log_test_set)-log_test_set[,1])^2)

log_mse_step_var=mean((predict(log_step_var,log_test_set)-log_test_set[,1])^2)

log_mse_pcr=mean((predict(log_pcr.fit,log_test_set,ncomp=10)-log_test_set[,1])^2)

log_mse.lasso=mean((predict(log_lasso.cv, s=log_lasso.cv$lambda.min, as.matrix(log_test_set[-1]))-log_test_set[,1])^2)

c(log_mse_best.subset,log_mse_step_var,log_mse_pcr,log_mse.lasso)

#VIF
#best/step
vif(log_train_set[names(log_best.subset.fit$coefficients)[-1]])

#LASSO
vif(log_train_set[which(coef(log_lasso.fit)!=0)])
