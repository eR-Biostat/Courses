####################################################################
##                                                                ##
## The >eR-Biostat initative                                      ##
## Introduction to categorical data analysis and GLM using R      ##
## Part 1 & 2                                                     ##
## Written by                                                     ##
## Nasima Akhte, Adetayo Kasim and Ziv Shkedy                     ##
##                                                                ##
## 2017                                                           ##
##                                                                ##
####################################################################
 




####################################################################
##  Introduction                                                  ##
####################################################################




####################################################################
##  Example 1                                                     ##
####################################################################
rbinom(5,1,0.7)

y<-rbinom(1000,1,0.7)
n1<-sum(y)
n1
barplot(c(n1,1000-n1),col=c(2,3),names=c("1","0"))


####################################################################
##  Example 2                                                     ##
####################################################################

barplot(c(0.125,0.375,0.375,0.125),col=c(2,2,2,2),names=c(0,1,2,3))

help(rbinom)

(0.5^3)*3

y<-rbinom(1,3,0.5)
y

y<-rbinom(10,3,0.5)
y
mean(y)
var(y)


y<-rbinom(1000000,3,0.5)
table(y)
mean(y)
3*0.5
var(y)
3*0.5*0.5

hist(y,col=2)


n1<-sum(y)
n1
barplot(c(n1,1000-n1),col=c(2,3),names=c("1","0"))

####################################################################
##  RD, RR and OR                                                 ##
####################################################################


####################################################################
##  2016: Example 1                                               ##
####################################################################


anemic <- read.csv("C:/projects/VLIR/CrossCutting/CoursesUpdated/BinaryKasim/data/Child anaemia.csv", header=TRUE)
#fix(anemic)
head(anemic)
genderAnemic <- table(anemic$Child_Gender,anemic$Child_Anemic)
genderAnemic
n=rowSums(genderAnemic)
barplot(c(genderAnemic[1,],genderAnemic[2,]),col=c(2,4))
barplot(c(genderAnemic[1,]/n[1],genderAnemic[2,]/n[2]),col=c(2,4))

RDanemic <- prop.test(x=genderAnemic[,2], n=rowSums(genderAnemic),  correct = FALSE)
RDanemic 
RD <- round(- diff(RDanemic$estimate), 3)
RDCI <- round(RDanemic$"conf.int", 3)
RDpvalue <- round(RDanemic$"p.value", 4)



####################################################################
##  Example 1&2 (RR)                                              ##
##  install.packages("bstats")                                    ##
####################################################################
library(bstats)

## example 1 with bstats  ##
genderAnemic <- table(anemic$Child_Gender, anemic$Child_Anemic)
genderAnemic
RRanemic <- oddsratio(x=genderAnemic[,2], n=rowSums(genderAnemic))
RRanemic
RR <- round(RRanemic$RR,3)
RRCI <- round(RRanemic$RRCI,3)

## example 2  with prop.test ##

ageAnemic <- table(anemic$Child_Agecat, anemic$Child_Anemic)
ageAnemic


RDanemic <- prop.test(x=ageAnemic[,2],  n= rowSums(ageAnemic) , correct = FALSE)
RDanemic


RD <- round(- diff(RDanemic$estimate),3)
RDCI <- round(RDanemic$"conf.int",3)
RDpvalue <- round(RDanemic$"p.value",4)




barplot(c(c(469,310)/(469+310),c(169,259)/(169+259)),col=c(2,3,2,3),
        names=c("No","Yes","No","Yes"))



####################################################################
##  Example 1& 2 (OR)                                             ##
##  install.packages("bstats")                                    ##
####################################################################

## example 1  ##

genderAnemic <- table(anemic$Child_Gender,anemic$Child_Anemic)
genderAnemic

ORanemic <- oddsratio(x=genderAnemic[,2], n=rowSums(genderAnemic))
ORanemic


## example 2  ##


ageAnemic <- table(anemic$Child_Agecat, anaemic$Child_Anemic)
ageAnemic

ORanemic <- oddsratio(x=ageAnemic[,2], n=rowSums(ageAnemic))
ORanemic

OR <- round(ORanemic$OR,3)
OR
ORCI <- round(ORanemic$ORCI,3)
ORCI




####################################################################
##  Example Aspirin                                               ##
##  install.packages("bstats")                                    ##
####################################################################


resp<-as.factor(c(rep(1,189),rep(0,10845),rep(1,104),rep(0 ,10933)))
trt<-as.factor(c(rep(1,189),rep(1,10845),rep(2,104),rep(2,10933)))
cbind(resp,trt)
table(trt,resp)
barplot(c(189,10845,104,10933),names=c("Present","Absent","Present","Absent"),col=c(1:4))



help(as.table)
Yes<-c(189,104)
No<-c(10845,10933)
Aspirin <- data.frame(cbind(Yes,No),row.names=c("Placebo","Aspirin"))
Aspirin1<-Aspirin
Aspirin1
barplot(c(189,10845,104,10933),col=c(1,2,1,2),names=c("Yes","No","Yes","No"))
barplot(c(c(189,10845)/(189+10845),c(104,10933)/(104+10933)),col=c(1,2,1,2),names=c("Yes","No","Yes","No"))

RAspirin1 <- prop.test(x=Aspirin1[,1], n=rowSums(Aspirin1),  correct = FALSE)
RAspirin1 

library(bstats)
RRAspirin1 <- oddsratio(x=Aspirin1[,1], n=rowSums(Aspirin1))
RRAspirin1




1/exp(-0.60544)




####################################################################
##  chi-square tests                                              ##
####################################################################


####################################################################
##  example 1                                                     ##
####################################################################


Anemic<-as.factor(c(rep("Yes",101),rep("No",99),rep("Yes",83),rep("No",117),rep("Yes",112),rep("No",89),rep("Yes",74),rep("No",126)))
Areas<-as.factor(c(rep("A",101),rep("A",99),rep("B",83),rep("B" ,117),rep("C",112),rep("C" ,89),rep("D",74),rep("D" ,126)))
#cbind(Anemic,Areas)
areaAnemic<-table(Anemic,Areas)
areaAnemic
chiArea <- chisq.test(areaAnemic,correct = FALSE)
chiArea


barplot(c(189,10845,104,10933),names=c("Present","Absent","Present","Absent"),col=c(1:4))



####################################################################
##  example 2                                                     ##
####################################################################


####################################################################
##  Example 2 (OR)                                                ##
##  install.packages("bstats")                                    ##
####################################################################


ageAnemic <- table(anemic$Child_Agecat, anaemic$Child_Anemic)
ageAnemic



barplot(c(c(469,310)/(469+310),c(169,259)/(169+259)),col=c(2,3,2,3),
        names=c("No","Yes","No","Yes"))

RDanemic <- prop.test(x=ageAnemic[,2],  n= rowSums(ageAnemic) , correct = FALSE)

RDanemic

p1<-310/779
p1
p2<-259/428
p2

z<-(p1-p2)/sqrt((p1*(1-p1)/779)+(p2*(1-p2)/428))
z
pnorm(z,0,1)*2
z^2 

ORanemic <- oddsratio(x=ageAnemic[,2], n=rowSums(ageAnemic))
ORanemic

chi.sq <- chisq.test(ageAnemic,correct = FALSE)
chi.sq

Oij <- ageAnemic
Oij

nplus. <- rowSums(ageAnemic)
nplus.
n.plus <- colSums(ageAnemic)
n.plus
npluplus <- sum(ageAnemic)
npluplus



Eij <- (nplus.%*%t(n.plus))/npluplus
Eij

tmp  <- ((Oij-Eij)^2)/Eij
X2 <- sum(tmp)
X2

df <- (nrow(areaAnemic)-1)*(ncol(areaAnemic)-1)
pvalue <- pchisq(X2, df,lower.tail = FALSE))


Eij

barplot(c(c(411.7664,367.2336)/(411.7664+367.2336),c(226.2336,201.7664)/(226.2336+201.7664
)),col=c(2,3,2,3),names=c("No","Yes","No","Yes"))


barplot(c(411.7664,367.2336,226.2336,201.7664),col=c(2,3,2,3),names=c("No","Yes","No","Yes"))


chi.sq <- chisq.test(Eij,correct = FALSE)
chi.sq

ORanemic <- oddsratio(x=Eij[,2], n=rowSums(Eij))
ORanemic






#########################################
# NEW CODE                              #
# Models for binary data                #
#########################################

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







