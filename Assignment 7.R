install.packages("car")
library(car)
#Q1
#make dummy vars
prob2$male <- ifelse(prob2$female==0,yes=1,no=0)
prob2$nonblack <- ifelse(prob2$black==0,yes=1,no=0)
#run models
modeli <- lm(prob2$votes~0+prob2$male+prob2$female)
summary(modeli)
modelii <- lm(prob2$votes~0+black+nonblack,data=prob2)
summary(modelii)
modeliii <- lm(prob2$votes~prob2$female)
summary(modeliii)
modeliv <- lm(prob2$votes~prob2$black)
summary(modeliv)
#Q2
##i
model2 <- lm(votes~female+black+party+redist+region+inc,data=prob2)
summary(model2)
##iii
confint(model2,"female",.9)
#iv
confint(model2,"black",.99)
#Q3
##3a
linearHypothesis(model2, c("regionMidwest=0","regionMidwest-regionSouth=0","regionMidwest-regionWest=0"))
##3b
linearHypothesis(model2,c("black=0","female=0"))
#Q4
##black-female
prob2$bf <- ifelse(prob2$female==1 & prob2$black==1, yes=1, no=0)
##black-male
prob2$bm <- ifelse(prob2$female==0 & prob2$black==1, yes=1, no=0)
##nonblack-female
prob2$nbf <- ifelse(prob2$female==1 & prob2$black==0, yes=1, no=0)
##nonblack-male
prob2$nbm <- ifelse(prob2$female==0 & prob2$black==0, yes=1, no=0)
##4a
model4a <- lm(votes~0+bf+bm+nbf+nbm,data=prob2)
summary(model4a)
##4b
model4b <- lm(votes~bm+nbf+nbm,data=prob2)
summary(model4b)
##4c
###make new var
prob2$interact <- prob2$female*prob2$black
###new model
model4c <- lm(votes~interact+female+black,data=prob2)
summary(model4c)
##4.2
###4.2a
linearHypothesis(model4a,c("bf-nbf=0"),singular.ok=TRUE)
###4.2b
linearHypothesis(model4b,c("nbf=0"),singular.ok=TRUE)
###4.2c
linearHypothesis(model4c,c("black+interact=0"))

