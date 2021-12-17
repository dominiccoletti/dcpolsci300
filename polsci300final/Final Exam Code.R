#3
##3A
library(readr)
cbidata <- read_csv("polsci300final/ps300.CBI_CWB_INF_UE-1.csv")
summary(cbidata$cbi)
.5640-.3384
sd(cbidata$cbi)
##3B
###3B.1
cor(cbidata$inf,cbidata$cbi)
cor(cbidata$ue,cbidata$cbi)
###3B.2
cor.test(cbidata$inf,cbidata$cbi)
cor.test(cbidata$ue,cbidata$cbi)
##3C
###3C.1
####3C.1.cwb
mean(cbidata$cwb)
cbidata$cwbd <- rep(NA, nrow(cbidata))
cbidata$cwbd <- ifelse(cbidata$cwb>.447619,yes=1,no=0)
####3C.1.inf
mean(cbidata$inf)
cbidata$infd <- ifelse(cbidata$inf>7.042222,yes=1,no=0)
####3C.1.ued
mean(cbidata$ue)
cbidata$ued <- ifelse(cbidata$ue>mean(cbidata$ue),yes=1,no=0)
###3C.2
t.test(cbidata$ue,cbidata$cwbd)
t.test(cbidata$inf,cbidata$cwbd)
###3C.3
chisq.test(cbidata$infd,cbidata$cwbd)
chisq.test(cbidata$ued,cbidata$cwbd)
#4
##4A
###4A.2
(.38-.5)/(.4854/sqrt(400))
(.6-.5)/(.4899/sqrt(400))
###4A.3
####t-test
2*pt(q=-4.944376,df=399)
2*pt(q=-4.082466,df=399)
##4B
###4B.2
sdbtmeans <- (399*.4854^2+399*.4899^2)/(399*2)
b1t <- .38/(sdbtmeans*sqrt(1/200))
b1t
###4B.3
2*pt(q=-22.59815,df=799)
#5
##5A
model5a <- lm(inf~cbi,data=cbidata)
summary(model5a)
##5B
model5b <- lm(ue~cbi,data=cbidata)
summary(model5b)
##5C
###5C.1
model5c1 <- lm(inf~cbi+cwb,data=cbidata)
summary(model5c1)
###5C.2
model5c2 <- lm(ue~cbi+cwb,data=cbidata)
summary(model5c2)
#6
##6A
###6A.1
model6a <- lm(ue~cbi*cwb+lrgdpc,data=cbidata)
summary(model6a)
###6A.3
7.1169+9.1612
vcov(model6a)
sqrt(11.569645+(23.219338)*0^2+(2*-14.384207)*0)
sqrt(11.569645+(23.219338)*1^2+(2*-14.384207)*1)
###6A.4
cwb <- seq(from=0,to=1)
eff.cbi <- coef(model6a)[2]+coef(model6a)[5]*cwb
eff.cbi.se <- sqrt(11.569645+(23.219338)*cwb^2+(2*-14.384207)*cwb)
plot(x = cwb, y = eff.cbi, type = "l", xlab = "cwb", 
     ylab = "Conditional effect of cbi")
lines(x=cwb,y=eff.cbi+2.453685*eff.cbi.se,lty="dashed",col="blue")
lines(x=cwb,y=eff.cbi-2.453685*eff.cbi.se,lty="dashed",col="blue")
abline(h=0,lty=2,col="grey")
######reset workspace
rm(list = ls())
##6B
library(readr)
library(stats)
install.packages("rms")
library(rms)
library(Hmisc)
mesdata <- read_csv("MES_PresApprov_ps300.csv")
View(mesdata)
#make time series
mesdata <- ts(mesdata,start=c(1954,1),end=c(1992,2),freq=4)
ols6b <- lm(app~applag+pexp+pago+bfut+bago+viet+events,data=mesdata)
summary(ols6b)
###6B.2
response_curve <- function(t,coeff){
  #inputs: t = counter 1 to max # of time periods for response calculation
  #       coeff = vector c(estimated LDV coef, estimated X coef)
  lag.coeff <- coeff[1]
  x.coeff <- coeff[2]
  #make max.TimePeriods long placeholder for estimated responses
  response <- list(temporary.shock = rep(0,length(t)),
                   permanent.shock = rep(0,length(t)))
  #response to default temporary shock: x=+1 period 1, =0 thereafter:
  response$temporary.shock <- x.coeff * lag.coeff^(t-1) #tempshock_response
  #response to default permanent shock: x=+1 period 1, =1 thereafter:
  response$permanent.shock <- cumsum(response$temporary.shock) #permshock_response
  #output from the function:
  return(response)
}
x.coeff <- ols6b$coefficients["events"]
lag.coeff <- ols6b$coefficients ["applag"]
coeff <- c(lag.coeff,x.coeff)
max.t <- 154
t <- seq(1,max.t,by=1)
response <- response_curve(t,coeff) 
response$permanent.shock
log(1-.75)/log(lag.coeff)
###6B.3
install.packages("ARDL")
library(ARDL)
###6B.4
x.coeff <- ols6b$coefficients["bfut"]
lag.coeff <- ols6b$coefficients ["applag"]
response$permanent.shock
###6B.5
max.t <- 25
x.coeff <- ols6b$coefficients["pexp"]
lag.coeff <- ols6b$coefficients ["applag"]
response$permanent.shock
plot(t,response$permanent.shock,type="l",xlab="time",ylab="response to permanent +1 pexp shock",xlim=c(0,25),lwd=2, col="blue")
###6B.5+
max.t <- 25
x.coeff <- ols6b$coefficients["pexp"]
lag.coeff <- ols6b$coefficients ["applag"]
response$permanent.shock
plot(t,response$temporary.shock,type="l",xlab="time",ylab="response to permanent +1 pexp shock",xlim=c(0,25),lwd=2, col="green")
##6C
ed <- as.data.frame(read.csv("ExtendedDeterrence.csv"))
View(ed)
model6c <- glm(det_suc~imbalfor+stbalfor+ltbalfor+nuke+alliance+arms_xfr+for_trde,data=ed,family=binomial(link="logit"))
summary(model6c)
X <- model.matrix(model6c)
B <- model6c$coefficients
all.means <- apply(X,2,mean)
else.means <- all.means #pause to get iqr
summary(ed$imbalfor)
else.means['imbalfor'] <- 1.1295
phat1 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat1
else.means['imbalfor'] <- .535
phat0 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat0
phat1-phat0



