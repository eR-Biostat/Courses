####################################################################
##                                                                ##
## The >eR-Biostat initative                                      ##
## Introduction to categorical data analysis and GLM using R      ##
## Part 5 & 6                                                     ##
## Written by                                                     ##
## Nasima Akhte, Adetayo Kasim and Ziv Shkedy                     ##
##                                                                ##
## 2017                                                           ##
##                                                                ##
####################################################################



############################################################
#### Example 1: The Aspirin and Myocardial Infaction Data###
############################################################

resp<-as.factor(c(rep(1,189),rep(0,10845),rep(1,104),rep(0 ,10933)))
trt<-as.factor(c(rep(1,189),rep(1,10845),rep(2,104),rep(2,10933)))
cbind(resp,trt)
table(trt,resp)
barplot(c(189,10845,104,10933),names=c("Present","Absent","Present","Absent"),col=c(1:4))

#########################################
# EXAMPLE 2: mice                       #
#########################################

resp<-as.factor(c(rep(0,21),rep(1,2),rep(0,19),rep(1,13)))
trti<-as.factor(c(rep(1,21),rep(1,2),rep(2,19),rep(2,13)))
cbind(resp,trti)
table(trti,resp)
#re=as.factor(c(rep(1,21),rep(0,2),rep(1,19),rep(0,13)))
#tr=as.factor(c(rep(1,21),rep(1,2),rep(2,19),rep(2,13)))
#cbind(re,tr)
#table(tr,re)
library(MASS)

#########################################
# EXAMPLE 3: malaria: Serological data  #
#########################################

agei<-c(1.5,4.0,7.5,12.5,17.5,25.0,35.0,47.0,60)
posi<-c(8,6,18,14,20,39,19,25,44)
ntot<-c(123,132,182,140,138,161,133,92,74)
negi<-ntot-posi
plot(agei,posi/ntot)
cbind(agei,posi,negi)
###################################
#########################################
# EXAMPLE 4: HIV Data                   #
#########################################
hivdat <- read.csv("C:/projects/VLIR/CrossCutting/CoursesUpdated/BinaryKasim/data/data 2_1_8.csv", header=TRUE)
hivdat
phiv<-mean(hivdat$hiv)
pie(c(phiv,1-phiv),labels=c("Positive","Negative"),col=c("1","2"))
par(mfrow=c(1,1))
plot(hivdat$age,hivdat$hiv)
################################################################
## Example 5: Budworm data dose response data of the binomial  #
################################################################

ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive=20-numdead)
p<-numdead/20
################################################################
###### Example 6: Heart disease                  ###############
################################################################
dhyes<-c(rep(1,24),rep(0,1355),rep(1,35),rep(0 ,603),rep(1,21),rep(0,192),rep(1,30),rep(0,224))
snoring<-c(rep(0,24),rep(0,1355),rep(2,35),rep(2,603),rep(4,21),rep(4,192),rep(5,30),rep(5,224))
cbind(dhyes,snoring)
table(snoring,dhyes)
par(mfrow=c(1,1))
counts <- table(dhyes, snoring)
barplot(counts, main="Heart disease Distribution by Snoring and ferquency",
  xlab="Snoring", col=c("blue","red"),legend = rownames(counts), beside=TRUE)



############################################################
############################################################
##  Modeling Binary Data                                  ##
############################################################
############################################################

############################################################
#### Example 1: The Aspirin and Myocardial Infaction Data###
############################################################


resp<-as.factor(c(rep(1,189),rep(0,10845),rep(1,104),rep(0 ,10933)))
trt<-as.factor(c(rep(1,189),rep(1,10845),rep(2,104),rep(2,10933)))
cbind(resp,trt)
table(trt,resp)
fit.myoc<-glm(resp~relevel(trt,2),family=binomial(link="logit"))
summary(fit.myoc)
barplot(c(189,10845,104,10933),names=c("Present","Absent","Present","Absent"),col=c(1:4))

############################################################
# EXAMPLE 2: mice                                          #
############################################################

resp<-as.factor(c(rep(0,21),rep(1,2),rep(0,19),rep(1,13)))
trti<-as.factor(c(rep(1,21),rep(1,2),rep(2,19),rep(2,13)))
table(trti,resp)
cbind(resp,trti)

fit.mice<-glm(resp~trti,family=binomial(link = "logit"))
summary(fit.mice)

exp(1.9719)
n=exp(-2.3514+1.9719)
n/(1+n)# probabilty that treatment have tumer

############################################################
# EXAMPLE 3: malaria                                       #
############################################################

agei<-c(1.5,4.0,7.5,12.5,17.5,25.0,35.0,47.0,60)
posi<-c(8,6,18,14,20,39,19,25,44)
ntot<-c(123,132,182,140,138,161,133,92,74)
negi<-ntot-posi
plot(agei,posi/ntot)
cbind(agei,posi,negi)

fit.malaria<-glm(cbind(posi,negi)~agei,family=binomial(link = "logit"))
summary(fit.malaria)
plot(posi/ntot~agei)
plot(fit.malaria)
exp(0.044672)
fv<-fit.malaria$fitted.values
plot(agei,fv,type="l",xlab="age",ylab="prevalnce",ylim=c(0,0.6))
points(agei,posi/ntot)
title("Data and predicted values")



############################################################
# EXAMPLE 4: HIV Data                                      #
############################################################

hivdat <- read.csv("C:/projects/VLIR/CrossCutting/CoursesUpdated/BinaryKasim/data/data 2_1_8.csv", header=TRUE)
hivdat
attach(hivdat)
library(boot)
phiv<-mean(hivdat$hiv)
hiv.fit1<-glm(hiv~age,family=binomial(link="logit"),data=hivdat)
summary(hiv.fit1)
h<-exp(-3.79597+0.07492*age)
fittt<-h/(1+h)#the fitted value or hiv.fit1$fitted.values
plot(age,hiv)
lines(age,fittt,type="l")


################################################################
## Example 5: Budworm data dose response data of the binomial  #
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
summary(budworm.lg1)
budworm.lg2 <- glm(SF ~ sex + ldose, family=binomial)
summary(budworm.lg2)
budworm.lg3<- glm(SF ~ sex*ldose, family=binomial)
summary(budworm.lg3)


################################################################
###### Example 6: Heart disease                  ###############
################################################################

dhyes<-c(rep(1,24),rep(0,1355),rep(1,35),rep(0 ,603),rep(1,21),rep(0,192),rep(1,30),rep(0,224))
snoring<-c(rep(0,24),rep(0,1355),rep(2,35),rep(2,603),rep(4,21),rep(4,192),rep(5,30),rep(5,224))
cbind(dhyes,trt)
table(snoring,dhyes)
fit.snoring<-glm(dhyes~as.factor(snoring),family=binomial(link="logit"))
summary(fit.snoring)
par(mfrow=c(1,1))
plot(snoring,dhyes)
plot(snoring,dhyes,ylab="Estimated probabilty for y=1",main="Data and estimated probabilty")
lines(snoring,fit.snoring$fitted.values)





############predictive plot##############
plot(c(1,32), c(0,1), type = "n", xlab = "dose",
     ylab = "prob", log = "x")
text(2^ldose, numdead/20, as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
   sex=factor(rep("M", length(ld)), levels=levels(sex))),
   type = "response"))
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
   sex=factor(rep("F", length(ld)), levels=levels(sex))),
   type = "response"))
###### End for modeling Binary Data####







