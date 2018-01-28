################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 2                                                    #
# ANOVA and Linear regression                                  #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################

#########################################
# EXAMPLE 1:                            #
# Plant Weight Data (Dobson)            #
#########################################

ctl <- c(4.17,5.18,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89, 4.32,4.69)
group <- gl(2,10,labels=c("Ctl","Trt"))
weight <- c(ctl,trt)
cbind(weight,group)
boxplot(split(weight,group))
fit.D9 <- lm(weight ~ group)
summary(fit.D9)
anova(fit.D9)
plot(fit.D9)
fit.aov<-aov(weight ~ group)
summary(fit.aov)
anova(fit.aov)

#data()
#data(package=.packages(all.available=TRUE))
#data(nottem )
#names(Familydata)
#ls()
#fm<-read.table(Familydata, package="epicalc")


##############################################
# EXAMPLE 2:                                 #
# bodyweight and gestational age  (Dobson)   #
##############################################

bage<-c(40,38,40,35,36,37,41,40,37,38,40,38)
gage<-c(40,36,40,38,42,39,40,37,36,38,39,40)
bwei<-c(2968,2795,3163,2925,2625,2847,3292,3473,2628,3176,3421,2975)
gwei<-c(3317,2729,2935,2754,3210,2817,3126,2539,2412,2991,2875,3231)
age<-c(bage,gage)
weight<-c(bwei,gwei)
gender <- gl(2,12,24,labels=c("M","F"))
dat2<-data.frame(weight,age,gender)
dat2

plot(age,weight,pch=" ", main="Scatter Plot of Age and Weight")
points(age[gender=="F"],weight[gender=="F"],pch="o")
points(age[gender=="M"],weight[gender=="M"],pch="+")


fit.lm.0 <- lm(weight ~ 1,data=dat2)
summary(fit.lm.0)
fit.lm.1 <- lm(weight ~ age,data=dat2)
summary(fit.lm.1)

fit.lm.2 <- lm(weight ~ age + gender,data=dat2)
fit.lm.3 <- lm(weight ~ age + gender + age*gender,data=dat2)
fit.lm.4 <- lm(weight ~ age + age:gender,data=dat2)
summary(fit.lm.0)
summary(fit.lm.1)
summary(fit.lm.2)
summary(fit.lm.3)
summary(fit.lm.4)
anova(fit.lm.0,fit.lm.1)
anova(fit.lm.1,fit.lm.2)
anova(fit.lm.2,fit.lm.3)

plot(age,weight,pch=" ")
points(age[gender=="F"],weight[gender=="F"],pch="F")
points(age[gender=="M"],weight[gender=="M"],pch="M")
lines(age[gender=="F"],fit.lm.2$fit[gender=="F"])
lines(age[gender=="M"],fit.lm.2$fit[gender=="M"],lty=2)

par(mfrow=c(2,2))
plot(fit.lm.2$fit,fit.lm.2$resid)
abline(0,0)
plot(age,fit.lm.2$resid)
abline(0,0)
qqnorm(fit.lm.2$resid)



summary(fit.lm.2)
summary(fit.lm.3)
summary(fit.lm.4)


plot(age,weight,pch=" ")
points(age[gender=="F"],weight[gender=="F"],pch="F")
points(age[gender=="M"],weight[gender=="M"],pch="M")
lines(age[gender=="F"],fit.lm.4$fit[gender=="F"])
lines(age[gender=="M"],fit.lm.4$fit[gender=="M"],lty=2)

plot(fit.lm.2$fit,fit.lm.4$resid)
abline(0,0)
plot(age,fit.lm.4$resid)
abline(0,0)
qqnorm(fit.lm.4$resid)

par(mfrow=c(2,2))
plot(fit.lm.2)

summary(fit.lm.2)

bage<-c(40,38,40,35,36,37,41,40,37,38,40,38)
gage<-c(40,36,40,38,42,39,40,37,36,38,39,40)
bwei<-c(2968,2795,3163,2925,2625,2847,3292,3473,2628,3176,3421,2975)
gwei<-c(3317,2729,2935,2754,3210,2817,3126,2539,2412,2991,2875,3231)
age<-c(bage,gage)
weight<-c(bwei,gwei)
gender <-c(rep(0,12),rep(1,12))
dat2<-data.frame(weight,age,gender)
dat2

summary(fit.lm.2)

beta1<-seq(from=110,to=130,length=100)
res.i<-c(1:100)
for(i in 1:100)
{
predict<- -1610.28+beta1[i]*age-163.04*gender
res.i[i]<-sum((weight-predict)^2) 
}
par(mfrow=c(1,1))
plot(beta1,res.i,type="l")
lines(c(120.89,120.89),c(min(res.i),max(res.i)))


summary(fit.lm.2)

beta1<-seq(from=110,to=130,length=100)
beta2<-seq(from=-170,to=170,length=100)
res.i<-matrix(0,100,100)
for(i in 1:100)
{
for(j in 1:100)
{
predict<- -1610.28+beta1[i]*age+beta2[j]*(1-gender)
res.i[i,j]<-sum((weight-predict)^2) 
}
}

contour(beta1,beta2,res.i)

persp(beta1,beta2,res.i)



#########################################################
# END OF CHAPTER 2                                      #
#########################################################




