library(readr)
prob2 <- read_csv("~/OneDrive - Umich/POLSCI 300/prob2.csv")
#Q1
##1a
##regress votes ~ redist
model1 <- lm(prob2$votes~prob2$redist)
summary(model1)
#1b
##scatterplot/abline
plot(prob2$redist,prob2$votes)
abline(model1, col="red") # regression line (prob2$votes~prob2$redist)
#1c
##t.test
summary(model1)
(1.4516-1)/.2655
2*pt(1.700942,368)
#Q2
#2a
model2 <- lm(cbind(prob2$votes) ~ prob2$redist + prob2$party)
summary (model2)
#2b
modelBz <- lm(prob2$votes ~ prob2$redist + prob2$party)
summary(modelBz)
modelBzX <- lm(prob2$redist ~ prob2$party)
summary(modelBzX)
-.05046*35.9385
#Q3
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
model3 <- lm(cbind(prob2$votes) ~ prob2$region + prob2$party + prob2$redist)
summary(model3)
(1.3881*20-9.243*0-23.4173*0+5.3285*0+37.2574*1)-(1.3881*10-9.243*0-23.4173*0+5.3285*0+37.2574*1)
(37.2574*1+1.3881*50+5.3285*0-23.4173*0-9.2403*0)-(37.2574*0+1.3881*50+5.3285*0-23.4173*0-9.2403*0)
#Q4
mean(prob2$redist[which(prob2$region=="Midwest")])
mean(prob2$redist[which(prob2$region=="West")])