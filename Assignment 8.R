install.packages("interplot")
library(interplot)
#1a
prob2$region <- rep(NA, nrow(prob2))
east <- which(prob2$east == 1)
midwest <- which(prob2$midwest == 1)
south <- which(prob2$south == 1)
west <- which(prob2$west == 1)
prob2$region[east] <- "East"
prob2$region[midwest] <- "Midwest"
prob2$region[south] <- "South"
prob2$region[west] <- "West"
prob2$region <- as.factor(prob2$region)
model1a <- lm(votes~redist*party+region+inc+black+female,data=prob2)
summary(model1a)
#1b
library(car)
summary(model1a)
#1c
interplot(model1, var1="redist", var2="party") + 
  xlab("party (0 is R, 1 is D)") + 
  ylab("Party representativeness (marginal effect of redist on votes)") + 
  ggtitle("Responsiveness of Parties to Constituents")
#2a
acddata <- read.csv("AmorimnetoCox_DuvergerData-1.csv")
View(acddata)
model2a <- lm(enpp~lml*eneth,data=acddata)
summary(model2a)
#2b
library(car)
linearHypothesis(model2a,c("lml=0","lml:eneth=0"))
#2c
linearHypothesis(model2a,c("eneth=0","lml:eneth=0"))
#2d
#2e
summary(model2a)  #for clarity, to see the summary results of the model again
interplot(model2a, var1="lml", var2="eneth", rfill="gold")+ xlab("effective number of ethnic groups") +     ylab("marginal effect of electoral system on enpp")+     ggtitle("The Effect of Electoral System on enpp Given societal ethnic heterogeneity")
interplot(model2a, var1="eneth", var2="lml", rfill="gold")+ xlab("log of median district magnitude") +     ylab("marginal effect of ethnic heterogeneity on enpp")+     ggtitle("The Effect of Ethnic Heterogeneity on enpp Given district magnitude")
