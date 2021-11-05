#Q4
##black-female
prob2$bf <- ifelse(prob2$female==1 & prob2$black==1, yes=1, no=0)
##black-male
prob2$bm <- ifelse(prob2$female==0 & prob2$black==1, yes=1, no=0)
##nonblack-female
prob2$nbf <- ifelse(prob2$female==1 & prob2$black==0, yes=1, no=0)
##nonblack-male
prob2$nbm <- ifelse(prob2$female==0 & prob2$black==0, yes=1, no=0)
