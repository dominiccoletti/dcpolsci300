ed <- as.data.frame(read.csv("ExtendedDeterrence.csv"))
View(ed)
#1
model1 <- glm(det_suc~imbalfor+stbalfor+ltbalfor+nuke+alliance+arms_xfr+for_trde,data=ed,family=binomial(link="logit"))
summary(model1)
#3b
X <- model.matrix(model1)
B <- model1$coefficients
all.means <- apply(X,2,mean)
summary(ed$nuke)
Logit.MargEff.polint <- exp(sum(all.means*B))/(1+exp(sum(all.means*B)))*1/(1+exp(sum(all.means*B)))*B['nuke']
Logit.MargEff.polint
#3c
else.means <- all.means
else.means['nuke'] <- 1
phat1 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat1
else.means['nuke'] <- 0
phat0 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat0
phat1-phat0
#4
else.means <- all.means
else.means['for_trde'] <- 0
plot(ed$for_trde, jitter(ed$det_suc,1),xlim=c(0,10), xlab="protégé's share of defender's foreign trade", ylab="deterrence success", main="Logit Model of for_trde on det_suc & controls")
lines(sort(ed$for_trde), #x,y & y=eXB/(1+eXB): 
      sort(exp(sum(else.means*B)+B['for_trde']*X[,'for_trde'])/(1+exp(sum(else.means*B)+B['for_trde']*X[,'for_trde']))), col="blue", lty=2)
#5
model5 <- glm(det_suc~imbalfor+stbalfor+ltbalfor+nuke+alliance+arms_xfr+for_trde,data=ed,family=binomial(link="probit"))
summary(model5)
##diff method "nuke"
X <- model.matrix(model5)
B <- model5$coefficients
all.means <- apply(X,2,mean)
else.means <- all.means
else.means['nuke'] <- 1
phat1 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat1
else.means['nuke'] <- 0
phat0 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat0
phat1-phat0
##diff method "imbalfor"
X <- model.matrix(model5)
B <- model5$coefficients
all.means <- apply(X,2,mean)
else.means <- all.means
else.means['imbalfor'] <- 5
phat1 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat1
else.means['nuke'] <- 0
phat0 <- exp(sum(else.means*B))/(1+exp(sum(else.means*B)))
phat0
phat1-phat0


