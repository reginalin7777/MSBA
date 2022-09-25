emp = read.csv("HR.csv", header=TRUE)
summary(emp)
 
emp$Work_accident = as.factor(emp$Work_accident)
emp$promotion_last_5years = as.factor(emp$promotion_last_5years)

summary(emp)

library(survival)
library(survminer)
attach(emp)


#self-define function, for exploratory of the survival curves

ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'red', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = T, xlab = 'Time',
                   ylab = 'Survival', main = ''){
  
  library(ggplot2)
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = T, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata
    
    ugroups <- unlist(strsplit(names(s$strata), '='))[seq(2, 2*strata, by = 2)]
    getlast = function(x) {
      res=NULL
      for (mo in names(x$strata)) {
        sur = x[mo]$surv
        n = length(sur)
        res = append(res,sur[n])
      }
      return(res)
    }
    lastv = ugroups[order(getlast(s),decreasing=T)]
    groups = factor(ugroups,levels = lastv)
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group, lty = group))
    
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)}
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}

#Headcount of each department
table(emp$department)


#KM-curve, sales
emp_surv = with(emp, Surv(time=time_spend_company, event=left))
km.dept = survfit(emp_surv ~ department, data=emp)
ggsurv(km.dept,main = 'KM curves')
survdiff(emp_surv~department, data=emp, rho=0)
print(with(emp, km.dept), print.rmean=TRUE)

#KM-curve, salary
km.salary = survfit(data_surv ~ salary, data=emp)
ggsurv(km.salary,main = 'KM curves')
survdiff(data_surv~salary, data=emp, rho=0)
print(with(emp, km.salary), print.rmean=TRUE)


#fitted line with 95% confidence band
cox = coxph(Surv(time=time_spend_company,event=left)~.,data=emp)
summary(cox)

#stepwise selection
step(cox,scope=list(upper=cox))


#model diagnostic
cox.zph(cox)
par(mfrow=c(2,2))
plot(cox.zph(cox)[1:4])
plot(cox.zph(cox_refit_int)[1:4])
plot(cox.zph(cox_refit_int)[5:8])
plot(cox.zph(cox_refit_int)[9:12])

#log-log survival plot for department
#library(survminer)
fit<- survfit(Surv(time=time_spend_company,event=left) ~ department, data = emp)
ggsurvplot(fit, data = emp, fun = "cloglog")
#log-log survival plot for accident
fit<- survfit(Surv(time=time_spend_company,event=left) ~ Work_accident, data = emp)
ggsurvplot(fit, data = emp,fun = "cloglog")
#log-log survival plot for salary
fit<- survfit(Surv(time=time_spend_company,event=left) ~ salary, data = emp)
ggsurvplot(fit, data = emp,fun = "cloglog")

#refit with Work_accident, department, salary
cox_refit = coxph(Surv(time=time_spend_company,event=left)~
            Work_accident+department+salary,data=emp)
cox.zph(cox_refit)


##refit with Work_accident, department, salary with interaction 
cox_refit_int = coxph(Surv(time=time_spend_company,event=left)~
                    Work_accident*department+Work_accident*salary+salary*department,data=emp)
cox.zph(cox_refit_int)


#testing for the interaction terms
anova(cox_refit,cox_refit_int)

#model diagnostic again
par(mfrow=c(2,2))
plot(cox.zph(cox_refit_int)[1:4])
plot(cox.zph(cox_refit_int)[5:8])
plot(cox.zph(cox_refit_int)[9:12])
plot(cox.zph(cox_refit_int)[13:16])
plot(cox.zph(cox_refit_int)[17:20])
plot(cox.zph(cox_refit_int)[21:24])
plot(cox.zph(cox_refit_int)[25:28])
plot(cox.zph(cox_refit_int)[29:32])
plot(cox.zph(cox_refit_int)[33:36])
plot(cox.zph(cox_refit_int)[37:40])


#survival probability across department by fixing Work_accident=0 and salary=low
df_new=with(emp, data.frame(department=levels(department),
                            Work_accident=rep('0',10),salary=rep('low',10)))
temp=survfit(cox_refit_int,newdata=df_new)
ggsurvplot(temp, conf.int = FALSE,
           ggtheme = theme_minimal(),data=emp ,legend.labs=levels(department),title='Survival probability for Work_accident=0, salary=low',ylab='Survival probability',legend = c(0.1, 0.3))

#survival probability across department by fixing Work_accident=0 and salary=medium
df_new=with(emp, data.frame(department=levels(department),
                            Work_accident=rep('0',10),salary=rep('medium',10)))
temp=survfit(cox_refit_int,newdata=df_new)
ggsurvplot(temp, conf.int = FALSE,
           ggtheme = theme_minimal(),data=emp ,legend.labs=levels(department),title='Survival probability for Work_accident=0, salary=medium',ylab='Survival probability',legend = c(0.1, 0.3))

#survival probability across department by fixing Work_accident=0 and salary=high
df_new=with(emp, data.frame(department=levels(department),
                            Work_accident=rep('0',10),salary=rep('high',10)))
temp=survfit(cox_refit_int,newdata=df_new)
ggsurvplot(temp, conf.int = FALSE,
           ggtheme = theme_minimal(),data=emp ,legend.labs=levels(department),title='Survival probability for Work_accident=0, salary=high',ylab='Survival probability',legend = c(0.1, 0.3))

##
#survival probability across department by fixing Work_accident=1 and salary=low
df_new=with(emp, data.frame(department=levels(department),
                            Work_accident=rep('1',10),salary=rep('low',10)))
temp=survfit(cox_refit_int,newdata=df_new)
ggsurvplot(temp, conf.int = FALSE,
           ggtheme = theme_minimal(),data=emp ,legend.labs=levels(department),title='Survival probability for Work_accident=1, salary=low',ylab='Survival probability',legend = c(0.1, 0.3))

#survival probability across department by fixing Work_accident=1 and salary=medium
df_new=with(emp, data.frame(department=levels(department),
                            Work_accident=rep('1',10),salary=rep('medium',10)))
temp=survfit(cox_refit_int,newdata=df_new)
ggsurvplot(temp, conf.int = FALSE,
           ggtheme = theme_minimal(),data=emp ,legend.labs=levels(department),title='Survival probability for Work_accident=1, salary=medium',ylab='Survival probability',legend = c(0.1, 0.3))

#survival probability across department by fixing Work_accident=1 and salary=high
df_new=with(emp, data.frame(department=levels(department),
                            Work_accident=rep('1',10),salary=rep('high',10)))
temp=survfit(cox_refit_int,newdata=df_new)
ggsurvplot(temp, conf.int = FALSE,
           ggtheme = theme_minimal(),data=emp ,legend.labs=levels(department),title='Survival probability for Work_accident=1, salary=high',ylab='Survival probability',legend = c(0.1, 0.3))

#prediction (with PI) for 'marketing' without accident and low salary
x_new <- data.frame(department='marketing',Work_accident='0',salary='low')
ggsurvplot(survfit(cox_refit_int,newdata=x_new), data=emp,
           legend.labs=paste(x_new$department,',Work_accident=',x_new$Work_accident,', salary=',x_new$salary),censor=FALSE)

