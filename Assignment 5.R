#Import dataset
library(readr)
acddata <- read_csv("AmorimnetoCox_DuvergerData-1.csv")
View(acddata)
#Q1
#Create dummy var smd
acddata$smd <- ifelse(acddata$lml==0,1,0)
#Q1a Estimate
model1a <- lm(enep ~ smd, data=acddata)
summary(model1a)
beta.est <- model1a$coefficients[2]
h0.value <- 0
beta.se <- summary(model1a)$coefficients[2,2]
t.stat <- (beta.est - h0.value)/beta.se