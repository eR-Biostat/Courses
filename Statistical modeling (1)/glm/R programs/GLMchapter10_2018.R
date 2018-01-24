################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 10                                                   #
# model diagnostic                                             #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################


################################################################
## Example 1: beetles data                                     #
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


###### raw residual plots#############3
library(boot)
library(graphics)
r.res<-resid(t1)
plot(r.res)
abline(h=0, lty=2)
###### residual plots#############
r.pearson<-resid(t1, type="pearson")
plot(r.pearson)
abline(h=0, lty=2)
par(mfrow=c(1,1))
qqnorm(r.pearson)
abline(0,1)

###### adjusted residual plots############
hii <- hatvalues(t1)
sum(hii)
plot(hii,ylim=c(-1,1))
2*2/8
abline(0.5,0)
abline(-0.5,0)

r.adjusted <- r.pearson/sqrt(1 - hii)
plot(r.adjusted)
abline(h = 0, lty = 2)


#######Cooks distance for t1##########
p.t1 <- length(coef(t1))
cook.t1 <- ((r.pearson^2) * hii)/((1 - hii)^2)
cook.t11 <- cooks.distance(t1) * p.t1
plot(cook.t1)


############ diagnostic plot######################
library(boot)
glm.diag.plots(t1)



###################################################
## End of Chapter 10                             ##
###################################################






