library(readr)
library(stats)
install.packages("rms")
library(rms)
library(Hmisc)
mesdata <- read_csv("MES_PresApprov_ps300.csv")
View(mesdata)
#make time series
mesdata <- ts(mesdata,start=c(1954,1),end=c(1992,2),freq=4)
#1
model1 <- lm(app~pexp+pago+bfut+bago+viet+events,data=mesdata)
summary(model1)
#2
model1.res <- resid(model1)
##2a
plot(mesdata[1:154,2],model1.res,xlab="Time",ylab="Residuals",main="Residuals over Time")
abline(0,0)
##2b
View(model1)
ts(model1.res)
model1lag.res <- Lag(model1.res,-1)
model2b <- lm(model1.res~model1lag.res)
summary(model2b)
plot(model1lag.res,model1.res)
#3
##3a
ols3a <- lm(app~applag+pexp+pago+bfut+bago+viet+events,data=mesdata)
summary(ols3a)
##3b
summary(model1)
##3c
View(ols3a)
ols3a.res <- resid(ols3a)
ols3alag.res <- Lag(ols3a.res,-1)
model3c <- lm(ols3a.res~ols3alag.res)
summary(model3c)
plot(ols3a.res,ols3alag.res)
#4
model4 <- lm(app~applag+pexp+pago+bfut+bago+viet+events+dde+ghb+grf+jec+jfk+lbj+rmn+rwr,data=mesdata)
summary(model4)
#5
response_curve <-function(t,lag.coef,x.coef){
  response <- list(temporary.shock=rep(0,length(t)),permanent.shock=rep(0,length(t)))
  response$temporary.shock <- x.coef*lag.coef^(t-1)
  response$permanent.shock <- cumsum(response$temporary.shock)
  return(response)
}
t <- 154
lag.coef <- model4$coefficients["applag"]
x.coef <- model4$coefficients["bfut"]
response <- response_curve(t,lag.coef,x.coef)
par(mfrow=c(1,2))
plot(response$temporary.shock,type="l", main="Temporary Shock", xlab="Time", ylab="Response")
plot(response$permanent.shock,type="l", main="Permament Shock", xlab="Time", ylab="Response")
