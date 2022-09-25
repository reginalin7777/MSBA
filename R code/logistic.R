
credit=read.csv('C:/Users/imelvis/Dropbox/UST/Spring2018/ISOM5610/Pre/logistic/loan_data_clear.csv',sep=',',header=T)
credit$MARRIAGE=factor(credit$MARRIAGE)
credit$SEX=factor(credit$SEX)
train_set=credit[1:21000,]
test_set=credit[-(1:21000),]

#summary statsitics 
summary(credit)
prop.table(table(factor(credit$default)))

counts = table(credit$SEX, credit$default)
barplot(counts, main="Default count by SEX",
        xlab="Default", col=c("darkblue","red"),
        legend = c('male','female'), beside=TRUE)

counts = table(credit$EDUCATION, credit$default)
barplot(counts, main="Default count by EDUCATION",
        xlab="Default", col=seq(1,4,1),
        legend = levels(EDUCATION), beside=TRUE)

counts = table(credit$MARRIAGE, credit$default)
barplot(counts, main="Default count by MARRIAGE",
        xlab="Default", col=seq(1,4,1),
        legend = c('others','married','single','divorce'), beside=TRUE)



#full model
fit=glm('default~.',data=train_set,family=binomial)
summary(fit)

#Stepwise selection
step_fit=step(fit)
summary(step_fit)

# model will all predictors but AGE
new_fit=update(fit,.~.-AGE)
summary(new_fit)

#interaction plots
attach(train_set)
ggplot() +
  aes(x = EDUCATION, color = MARRIAGE, group = MARRIAGE, y = default) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + ggtitle('EDUCATION:MARRIAGE')

ggplot() +
  aes(x = EDUCATION, color = SEX, group = SEX, y = default) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + ggtitle('EDUCATION:SEX')

ggplot() +
  aes(x = MARRIAGE, color = SEX, group = SEX, y = default) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") + ggtitle('MARRIAGE:SEX')


# Test for interaction terms 
new_fit_int=update(new_fit,.~.+MARRIAGE:EDUCATION+SEX:EDUCATION)
library(lmtest)
lrtest(new_fit,new_fit_int)

#vif
library(car)
vif(new_fit)

#esimtated probabilities 
prob <- predict(new_fit,newdata=test_set, type = 'response')
table(test_set$default, prob > 0.5)

#odds ratio
OR_CI=exp(confint(new_fit))
cbind(exp(coef(new_fit)),OR_CI)

#Prediction Example
test_set[1,]
predict(new_fit,newdata=test_set[1,],type='response')

#ROC and error measure
library(InformationValue)
#detach('package:caret')
plotROC(test_set$default,prob)
sensitivity(test_set$default,prob)
specificity(test_set$default,prob)
confusionMatrix(test_set$default,prob)
misClassError(test_set$default,prob)


#Double density plot
p=predict(new_fit,type='response')
temp_train=cbind(train_set,p)
p=prob
temp_test=cbind(test_set,p)

ggplot(temp_train, aes( p, color = as.factor(default) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's estimated probabilities" ) 



#Confusion Matrix plot
source("C:/Users/imelvis/Dropbox/UST/Spring2018/ISOM5610/Pre/logistic/unbalanced_function.R")

cm_info <- ConfusionMatrixInfo( data = temp_test, predict = "p", 
                                actual = "default", cutoff = 0.5 )
cm_info$plot

#Optimal cutoff by considering cost
cost_fp <- 10
cost_fn <- 20
roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)

#error measure again
detach('package:caret')
sensitivity(test_set$default,prob,threshold = roc_info$cutoff)
specificity(test_set$default,prob,threshold = roc_info$cutoff)
confusionMatrix(test_set$default,prob,threshold = roc_info$cutoff)
misClassError(test_set$default,prob,threshold = roc_info$cutoff)


