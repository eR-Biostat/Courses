################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 9                                                    #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################

################################################################
## Example 1: Budworm data dose response data of the binomial  #
################################################################

ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive=20-numdead)
p<-numdead/20
budworm.lg <- glm(SF ~ sex*ldose, family=binomial)
summary(budworm.lg)

par(mfrow=c(1,2))
plot(p ~ ldose)
plot(p ~ log(ldose))

budworm.lg1 <- glm(SF ~ ldose, family=binomial)
budworm.lg2 <- glm(SF ~ sex + ldose, family=binomial)
budworm.lg3<- glm(SF ~ sex*ldose, family=binomial)

summary(budworm.lg3)


####### likelihood of the models#########

logLik(budworm.lg1)
logLik(budworm.lg2)
logLik(budworm.lg3)


budworm.lg1$null.deviance
budworm.lg1$deviance
budworm.lg2$null.deviance
budworm.lg2$deviance
budworm.lg3$null.deviance
budworm.lg3$deviance

extractAIC(budworm.lg1, k=2)
extractAIC(budworm.lg2, k=2)
extractAIC(budworm.lg3, k=2)


summary(budworm.lg3)
library(MASS)
stepAIC(budworm.lg3, direction = "backward")



################################################################
## Example 2: beetles data                                     #
################################################################

beetle<-read.table("C:/projects/GLM/data4glm/beetle.txt", header = TRUE)
attach(beetle)
p<-killed/beetles
unkilled<-beetles-killed
Proportionkilled<-p

plot(Proportionkilled~Dose, main="Proportion of the killed beetles")
plot(Dose, Proportionkilled, pch=16, type="o")


tapply(Proportionkilled, list(Dose), mean)

t1 <-glm(cbind(killed,unkilled)~Dose, family=binomial("logit"))
summary(t1)
t2 <-glm(cbind(killed,unkilled)~Dose, family=binomial("probit"))
summary(t2)
t3 <-glm(cbind(killed,unkilled)~Dose, family=binomial("cloglog"))
summary(t3)
extractAIC(t1, k=2)
extractAIC(t2, k=2)
extractAIC(t3, k=2)

####### likelihood of the models#########
L1<-logLik(t1)
L2<-logLik(t2)
L3<-logLik(t3)

#############Fitted value from the three models#########33
glm.logit<-t1$fitted.values
glm.probit<-t2$fitted.values
glm.cloglog<-t3$fitted.values
########### plot of the data and estimated model###########
plot(Proportionkilled~Dose, xlab="Dose", ylab="Proportion killed", ylim=c(0,1), pch=16)
lines(Dose,glm.logit,col=2)
lines(Dose,glm.probit,col=3)
lines(Dose,glm.cloglog,col=4)
legend(locator(1),legend=c("Logit","Probit","CLog-Log"),col=2:4, lty=c(1,3,4))




