################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 10                                                   #
# two Examples                                                 #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################

###########################################
# Effect of drug on cardiatic death       #
# (McCullagh & Nelder 1983)               #
###########################################

d<-c(41,60)
s<-c(692,682)
OR<-(41*682)/(60*692)
OR
log(OR)
par(mfrow=c(1,1))
barplot(c(c(41,692)/733,c(41,683)/742),names.arg=c("D-T","S-T","D-P","S-P"))
n<-c(733,742)
gr<-c("T","P")
cbind(d,s,n)


summary(glm(d/n~gr,family="binomial"))
exp(-0.3953)

###########################################
# Habitat preferences of lizards          #
# (McCullagh & Nelder 1983)               #
###########################################


habitat <- read.table("C:/projects/GLM/data4glm/habitat.txt")
dim(habitat)
names(habitat)<-c("G","Total","S","D","H","T")
habitat
f1<-glm((G/Total)~H+D+S+T,family="binomial",data=habitat)
summary(f1)


f1.over <- glm((G/Total)~H+D+S+T,family=quasibinomial,data= habitat)
summary(f1.over)
summary(f1.over)$dispersion



r.pearson<-resid(f1, type="pearson")
plot(r.pearson)
abline(h=0, lty=2)

par(mfrow=c(1,1))
qqnorm(r.pearson)
abline(0,1)

mean(r.pearson)
var(r.pearson)

r.s<-(r.pearson-mean(r.pearson))/sqrt((var(r.pearson)))
qqnorm(r.s)
abline(0,1)



par(mfrow=c(2,2))
plot(f1)


f2<-glm((G/Total)~H+D+S+T+T*S,family="binomial",data=habitat)
f3<-glm((G/Total)~H+D+S+T+T*H,family="binomial",data=habitat)
f4<-glm((G/Total)~H+D+S+T+T*D,family="binomial",data=habitat)
f5<-glm((G/Total)~H+D+S+T+S*H,family="binomial",data=habitat)
f6<-glm((G/Total)~H+D+S+T+S*D,family="binomial",data=habitat)
f7<-glm((G/Total)~H+D+S+T+H*D,family="binomial",data=habitat)



deviance(f1)
deviance(f2)
deviance(f3)
deviance(f4)
deviance(f5)
deviance(f6)
deviance(f7)


logLik(f1)
logLik(f2)
logLik(f3)
logLik(f4)
logLik(f5)
logLik(f6)
logLik(f7)




extractAIC(f1)
extractAIC(f2)
extractAIC(f3)
extractAIC(f4)
extractAIC(f5)
extractAIC(f6)
extractAIC(f7)



