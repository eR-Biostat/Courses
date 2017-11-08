####################################################################
##                                                                ##
## The >eR-Biostat initative                                      ##
## Introduction to categorical data analysis and GLM using R      ##
## Part 3 & 4                                                     ##
## Written by                                                     ##
## Nasima Akhte, Adetayo Kasim and Ziv Shkedy                     ##
##                                                                ##
## 2017                                                           ##
##                                                                ##
####################################################################

####################################################################
##  Example 1                                                     ##
####################################################################


y<-c(35,21,9,6,1)
n<-c(40,40,40,40,40)
dose<-c(0.0028,0.0056,0.0112,0.0225,0.0450)
y
n
dose
plot(dose,y/n)

fit.1 <- glm(y/n~dose, family= gaussian(link=identity))
summary(fit.1)

plot(dose,y/n,ylim=c(-0.2,1.2))
lines(dose,fit.1$fit)
abline(0,0,col=2)

fit.2 <- glm(y/n~dose, family= gaussian(link=log))
summary(fit.2)

plot(dose,y/n,ylim=c(-0.2,1.2))
lines(dose,fit.1$fit)
lines(dose,fit.2$fit,col=3)
abline(0,0,col=2)

fit.3 <- glm(y/n~dose, family= binomial(link=logit))
summary(fit.3)

plot(dose,y/n,ylim=c(-0.2,1.2))
lines(dose,fit.1$fit)
lines(dose,fit.2$fit,col=3)
lines(dose,fit.3$fit,col=4)
abline(0,0,col=2)
  
####################################################################
##  Example 2                                                     ##
####################################################################

#anemic <- read.csv("C:/projects/VLIR/CrossCutting/CoursesUpdated/BinaryKasim/data/Child anaemia.csv", header=TRUE)
#anemic$y <- ifelse(anemic$Child_Anemic=="Yes",1,0) 
#anemic$gender <- ifelse(anemic$Child_Gender=="Boy",1,0)
#head(data.frame(anemic$y,anemic$gender))
#table(anemic$y,anemic$gender)
#fit.1 <- glm(y~gender, family=binomial(link=logit),data=anemic)
#summary(fit.1)
#confint(fit.1)
#barplot(c(189,10845,104,10933),names=c("Present","Absent","Present","Absent"),col=c(1:4))



y<-as.factor(c(rep(0,360),rep(1,326),rep(1,243),rep(0,278)))
gender<-as.factor(c(rep(1,360),rep(1,326),rep(0,243),rep(0,278)))
#cbind(y,gender)
table(y,gender)
fit.1 <- glm(y~gender, family=binomial(link=logit))
summary(fit.1)
confint(fit.1)

(278*326)/(243*360)
exp(0.03535) 

####################################################################
##  Example 3                                                     ##
####################################################################

#anemic <- read.csv("C:/projects/VLIR/CrossCutting/CoursesUpdated/BinaryKasim/data/Child anaemia.csv", header=TRUE)
#anemic$y <- ifelse(anemic$Child_Anemic=="Yes",1,0) 
#anemic<- anemic[anemic$Areas!="Missing value",]
#anemic$Areas <- as.factor(as.character(anemic$Areas))
#head(data.frame(anemic$y,anemic$Areas))
#table(anemic$y,anemic$Areas)
#fit.1 <- glm(y~Areas, family=binomial(link=logit),data=anemic)
#summary(fit.1)
#confint(fit.1)



y<-as.factor(c(rep(0,99),rep(1,101),rep(0,117),rep(1,83),rep(0,89),rep(1,112),rep(0,126),rep(1,74)))
Area<-as.factor(c(rep("A",99),rep("A",101),rep("B",117),rep("B",83),rep("C",89),rep("C",112),rep("D",126),rep("D",74)))
#cbind(y,Area)
table(y,Area)
fit.1 <- glm(y~Area, family=binomial(link=logit))
summary(fit.1)
confint(fit.1)





####################################################################
##  Example 4                                                     ##
####################################################################

#fastData<- read.csv("C:/projects/VLIR/CrossCutting/CoursesUpdated/BinaryKasim/data/fast.csv", header=TRUE)
#fastData
#plot(fastData$load,fastData$nfail/fastData$ntotal)
#fit.1 <- glm(cbind(nfail,ntotal-nfail)~load,family=binomial(link="logit"),data=fastData)
#summary(fit.1)
#plot(fastData$load,fastData$nfail/fastData$ntotal)
#lines(fastData$load,fit.1$fit,col=3)

load<-c(2500,2700,2900,3100,3300,3500,3700,3900,4100,4300)
ntotal<-c(50,70,100,60,40,85,90,50,80,65)
nfail<-c(10,17,30,21,18,43,54,33,60,51)
cbind(load,ntotal,nfail)

plot(load,nfail/ntotal)

fit.1 <- glm(cbind(nfail,ntotal-nfail)~load,family=binomial(link="logit"))
summary(fit.1)

plot(load,nfail/ntotal)
lines(load,fit.1$fit,col=3)

xx<-seq(from=1000,to=6000,length=1000)
eta1<- -5.3397115+0.0015484*xx 
pp<-exp(eta1)/(1+exp(eta1))
plot(xx,pp,pch=" ")
points(load,nfail/ntotal)
lines(xx,pp)
lines(load,fit.1$fit,col=3,lwd=3)




fit.2 <- glm(nfail/ntotal~load,family= gaussian(link=identity))
summary(fit.2)
fit.2$coefficients
plot(load,nfail/ntotal)
lines(load,fit.1$fit,col=3)
lines(load,fit.2$fit,col=2)


xx<-seq(from=1000,to=6000,length=1000)
eta1<- -5.3397115+0.0015484*xx 
eta2<- -0.6920048383+0.0003459824*xx
pp<-exp(eta1)/(1+exp(eta1))
pp2<-eta2
plot(xx,pp,pch=" ",ylim=c(-0.4,1.4))
points(load,nfail/ntotal)
lines(xx,pp,col=3)
lines(xx,pp2,col=2)






