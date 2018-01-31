################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 7                                                    #
# GLM                                                          #
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

library(MASS)
model.conf <-glm(cbind(killed,unkilled)~Dose, family=binomial("cloglog"), data=beetle)
confint(model.conf, level=0.95)

################################################################
## Example 3: mice data                                        #
################################################################

resp<-as.factor(c(rep(0,21),rep(1,2),rep(0,19),rep(1,13)))
trti<-as.factor(c(rep(1,21),rep(1,2),rep(2,19),rep(2,13)))
cbind(resp,trti)
table(trti,resp)
library(MASS)
fit.mice<-glm(resp~trti,family=binomial(link = "logit"))
summary(fit.mice)

confint(fit.mice, level=0.95)

################################################################
## Example 4: HIV data                                         #
################################################################

hivdat <- read.csv("C:/projects/VLIR/CrossCutting/CoursesUpdated/BinaryKasim/data/data 2_1_8.csv", header=TRUE)
hivdat
phiv<-mean(hivdat$hiv)
pie(c(phiv,1-phiv),labels=c("Positive","Negative"),col=c("1","2"))
par(mfrow=c(1,1))
plot(hivdat$age,hivdat$hiv)

hiv.fit1<-glm(hiv~age,family=binomial(link = "logit"),data=hivdat)
summary(hiv.fit1)
confint(hiv.fit1 , level=0.95)

#re=as.factor(c(rep(1,21),rep(0,2),rep(1,19),rep(0,13)))
#tr=as.factor(c(rep(1,21),rep(1,2),rep(2,19),rep(2,13)))
#cbind(re,tr)
#table(tr,re)
library(MASS)


#########################################################
# END OF CHAPTER 7                                      #
#########################################################
