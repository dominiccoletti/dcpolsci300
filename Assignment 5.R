#Import dataset
acddata <- read_csv("AmorimnetoCox_DuvergerData-1.csv")
library(readr)
View(acddata)
#Q1
#Create dummy var smd
acddata$smd <- ifelse(acddata$lml==0,1,0)
#Q1a Estimate
model1a <- lm(enep ~ smd, data=acddata)
#Q1b p-level
summary(model1a)
beta.est <- model1a$coefficients[2]
h0.value <- 0
beta.se <- summary(model1a)$coefficients[2,2]
t.stat <- (beta.est - h0.value)/beta.se
2*pt(-abs(t.stat), model1a$df.residual)
#Q1c t-test
t.test(acddata$enpp ~ acddata$smd)
#Q2a bivariate regression
model2a <- lm(enep ~ lml, data=acddata)
summary(model2a)
#Q2c regression-based prediction
(acddata$lml[which(acddata$country=="DENMARK")]) -> lmlden
(acddata$lml[which(acddata$country=="AUSTRIA")]) -> lmlaus
model2a$coefficients[2]*(lmlaus-lmlden)

#Q3a Regression
model3a <- lm(enep ~ lml, data=acddata)
summary(model3a)
1.9480+.4687
#Q3b
(acddata$lml[which(acddata$country=="DOMINICANREPU")]-acddata$lml[which(acddata$country=="UK")])*.4687
(acddata$enpp[which(acddata$country=="DOMINICANREPU")]-acddata$enpp[which(acddata$country=="UK")])
#Q3c
acddata$lml[which(acddata$country=="SPAIN")]*.4687
2.8083-.9120481

#Q4
summary(model3a)
summary(model2a)