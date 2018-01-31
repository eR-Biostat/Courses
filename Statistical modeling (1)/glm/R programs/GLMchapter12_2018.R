################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 12                                                   #
# Beyond Poisson and binomial Regression                       #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################


################################################################
##########Example 1: Employment data gamma and lognormal########
################################################################

employ <- read.table("C:/projects/GLM/data4glm/employ.txt",header=TRUE)
attach(employ)

#employ1<- transform(employ1, y = duration/max(duration), grade = factor(grade))
#employ1<- transform(employ1, y = log(duration), grade = factor(grade))

par(mfrow=c(1,2))
plot(month,duration)
plot(month,log(duration))



library(MASS)
boxcox(y ~ month + grade + month:grade, data = employ1)

m.normal.idt <- glm(duration ~ month + grade + month:grade,data = employ, family = gaussian(link = identity))
summary(m.normal.idt)
m.normal.inv <- glm(duration ~ month + grade + month:grade,data = employ, family = gaussian(link = inverse))
summary(m.normal.inv)
m.normal.log <- glm(duration ~ month + grade + month:grade,data = employ, family = gaussian(link = log))
summary(m.normal.log)
m.normal.log1 <- glm(duration ~ month + grade ,data = employ, family = gaussian(link = log))
summary(m.normal.log1)


extractAIC(m.normal.idt, k=2)
extractAIC(m.normal.inv, k=2)
extractAIC(m.normal.log, k=2)
extractAIC(m.normal.log1, k=2)

par(mfrow=c(1,1))
plot(month[grade==1],duration[grade==1],ylim=c(0,35))
points(month[grade==2],duration[grade==2],pch="+")
lines(month[grade==1],m.normal.log1$fit[grade==1])
lines(month[grade==2],m.normal.log1$fit[grade==2])


m.normal.log <- glm(duration ~ month + grade + month:grade, data = employ, family = gaussian(link = log))
summary(m.normal.log)
m.gamma.inv <- glm(duration ~ month + grade + month:grade, data = employ, family = Gamma(link = inverse))
summary(m.gamma.inv )

m.gamma.inv1 <- glm(duration ~ month , data = employ, family = Gamma(link = inverse))
summary(m.gamma.inv1)

m.gamma.inv2 <- glm(duration ~ -1+ month , data = employ, family = Gamma(link = inverse))
summary(m.gamma.inv2)


library(boot)
AIC(m.normal.log)
AIC(m.gamma.inv)
AIC(m.gamma.inv1)
AIC(m.gamma.inv2)


par(mfrow=c(1,1))
plot(month[grade==1],duration[grade==1],ylim=c(0,50))
points(month[grade==2],duration[grade==2],pch="+")
lines(month[grade==1],m.normal.log$fit[grade==1])
lines(month[grade==2],m.normal.log$fit[grade==2])
lines(month,m.gamma.inv2$fit,lty=1,col=2,lwd=2)



#################################################
## best model selection                        ##
#################################################


library(leaps)
library(xtable)
library(bestglm)
bestmodel2<-data.frame(month,grade,duration)
bestmodel2
bestglm(bestmodel2,family = gamma, IC="AIC")



#########################################################
# END OF CHAPTER 12                                     #
#########################################################

