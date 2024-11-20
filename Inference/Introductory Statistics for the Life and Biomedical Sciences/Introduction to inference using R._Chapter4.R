##########################################################
#                                                        #
#                                                        #
# Introduction to Statistical inference using R          #
#                                                        #
# 2018                                                   #
# Ziv Shkedy                                             #
# >eR-BioStat                                            #
# Chapter 4                                              #
#                                                        #
#                                                        #
##########################################################





###########################################################
################# Chapter Four ############################
###########################################################

###########################################################
# 1.1: Notations and definitions                          #
###########################################################

help(airquality)
airquality$Ozone

ozone<-na.omit(airquality$Ozone)
length(ozone)
par(mfrow=c(1,2))
boxplot(ozone)
hist(ozone)

###########################################################
# 1.2 Random samples in R                                 #
###########################################################
par(mfrow=c(1,1))
x<-rnorm(100,0,1)
hist(x)
x<-rbinom(100,1,0.7)
hist(x)
x<-rpois(100,3)
x
table(x)


###########################################################
# Part 2: Point estimation for population mean and        #
#         population variance                             #
###########################################################

ozone=airquality$Ozone
meanozone=mean(ozone,na.rm=T)
meanozone
varozone=var(ozone,na.rm=T)
varozone
sqrt(varozone)

hist(ozone,breaks=25,main="Histogram of Mean ozone")
lines(c(meanozone,meanozone),c(0,23),col="red",lwd=2)
text(meanozone,25,round(meanozone,2)) 
text(100,25,"point estimate of mean ozone ",col=3)


######### american women data set ###################

help(women)
women


womenheight=women$height
meanheight=mean(womenheight,na.rm=T) 
meanheight

womenheight=women$height
varheight=var(womenheight,na.rm=T)
varheight

womenheight=women$height
varheight=var(womenheight,na.rm=T)
sqrt(varheight)



###########################################################
# Part 3:  Variability of estimates                       #
###########################################################

mx1=c(1:1000)
mx2=c(1:1000)
mx3=c(1:1000)
mx4=c(1:1000)
for(i in 1:1000)
{
  sample1=rnorm(2,5,1)
  sample2=rnorm(10,5,1)
  sample3=rnorm(50,5,1)
  sample4=rnorm(100,5,1)
  mx1[i]=mean(sample1)
  mx2[i]=mean(sample2)
  mx3[i]=mean(sample3)
  mx4[i]=mean(sample4)
}
mean(mx1)
par(mfrow=c(2,2))
hist(mx1,breaks=20, main="histogram of X-bar, n=2")
lines(c(mean(mx1),mean(mx1)),c(1,129),col="red",lwd=2)
lines(c(5,5),c(1,1000),col="blue")
hist(mx2,breaks=20, main="histogram of X-bar, n=10")
lines(c(mean(mx2),mean(mx2)),c(1,129),col="red",lwd=2)
lines(c(5,5),c(1,1000),col="blue")
hist(mx3,breaks=20, main="histogram of X-bar, n=50")
lines(c(mean(mx3),mean(mx3)),c(1,129),col="red",lwd=2)
lines(c(5,5),c(1,1000),col="blue")
hist(mx4,breaks=200, main="histogram of X-bar, n=100")
lines(c(mean(mx4),mean(mx4)),c(1,129),col="red",lwd=2)
lines(c(5,5),c(1,1000),col="blue")

#### critical values  #########################################

qt(0.975,13)
qt(0.025,13)


#####  C.I For Case 1 #########################################

ozone=na.omit(airquality$Ozone) 
sigma=32                          
sem=sigma/sqrt(n)         
E=qnorm(0.975)*sem    
xbar=mean(ozone) 
xbar+c(-E,E)

#### C.I with z.test() #######################################

library(TeachingDemos)
ozone=na.omit(airquality$Ozone)
sigma=32
z.test(ozone,sd=sigma)

##### C.I for case 2 #########################################

ozone=na.omit(airquality$Ozone) 
sigma=sd(ozone)                          
sem=sigma/sqrt(n)         
E=qnorm(0.975)*sem    
xbar=mean(ozone) 
xbar+c(-E,E)

#### C.I with z.test, case 2 ##################################

library(TeachingDemos)
ozone=na.omit(airquality$Ozone)
z.test(ozone, sd=sd(ozone),conf.level = 0.95)

#### C.I for case 3 ##########################################

Height=women$height 
n=length(Height)
SE=s/sqrt(n)
SE=qt(0.975,df=n-1)*SE
xbar=mean(Height) 
xbar+c(-E,E) 

#### C.I for case 3 with t.test() ############################
library(TeachingDemos)
t.test(women$height)


#### critical value ##########################################

xbar=2.91;s=sqrt(9.9119);n=173;H0=2.5
cc=qnorm(0.95)*(s/sqrt(n))+H0
cc

xbar=2.91;s=sqrt(9.9119);n=173;H0=2.5
test.stat=(xbar-H0)/(s/sqrt(n))
test.stat

crit.point=qnorm(0.95)
crit.point

#### Example: ozone level  ##################################

library(TeachingDemos)
x<-na.omit(airquality$Ozone)
s<-sqrt(var(x))
z.test(x,sd=s,mu=40)



#### inference with t distribution  #########################

x=c(22,19,17,26,21,20,29,27,22)
xbar=mean(x)
mu = 21
s = sd(x)
n = length(x)
t = (xbar-mu)/(s/sqrt(n)) 
t                      # test statistic 

alpha<-0.05
crit.val = qt(1-alpha, n-1, lower.tail = TRUE) 
crit.val     # critical value 


##### heights and weights for American women aged 30-39 ######

womenheight=women$height
t.test(womenheight,mu=60,conf.level=0.90)


#### one sample problem ######################################

xbar=50.8;s=sqrt(1.6);n=100;H0=51
test.statgy=(xbar-H0)/(s/sqrt(n))
test.statgy
crit.point1=qnorm(0.95,lower.tail=TRUE)#p=0.05 one tailed
-crit.point1

#### rejection region two-sided test   ######################

xbar=118;s=sqrt(98);n=50;H0=115
test.statcrop=(xbar-H0)/(s/sqrt(n))
test.statcrop
alpha = 0.05 
crit.pointcrop = qnorm(1-alpha/2)
crit.pointcrop 
-crit.pointcrop
c(-crit.pointcrop,crit.pointcrop)


#### p value   ##############################################


x=c(22,19,17,26,21,20,29,27,22)
xbar=mean(x)
mu = 21                
s = sd(x)
n = length(x)
t = (xbar-mu)/(s/sqrt(n))
alpha = .05 
pval = pt(t,df=n-1, lower.tail=FALSE) 
pval

#### p value   ##############################################

xbar=50.8;s=sqrt(1.6);n=100;H0=51
test.statgy=(xbar-H0)/(s/sqrt(n))
test.statgy
pval1 = pt(test.statgy,df=n-1,lower.tail=TRUE) 
pval1


#### p value : two sided ####################################


bar=118;s=sqrt(98);n=50;H0=115
test.statcrop=(xbar-H0)/(s/sqrt(n))
test.statcrop 
2*(1-pnorm(test.statcrop))                

 
###########################################################
################# End of Chapter Four #####################
###########################################################
 
 
 
 
 
 
 
 
############ part 3 p value
 x=c(22,19,17,26,21,20,29,27,22)
 xbar=mean(x)
 mu = 21                
  s = sd(x)
  n = length(x)
  t = (xbar-mu)/(s/sqrt(n))
  t
  alpha = .05 
  pval = pt(t,df=n-1, lower.tail=FALSE) 
  pval
########## two tailed test
xbar=50.8;s=sqrt(1.6);n=100;H0=51
 test.statgy=(xbar-H0)/(s/sqrt(n))
 test.statgy
 pval1 = pt(test.statgy,df=n-1, lower.tail=TRUE) 
 pval1
 bar=118;s=sqrt(98);n=50;H0=115
 test.statcrop=(xbar-H0)/(s/sqrt(n))
 test.statcrop 
 2*(1-pnorm(test.statcrop))  
 
 
 
 
 
 
 
 
 
##############Part 2b: Testing of hypotheses for population proportion 
pbar = 0.47
 prop = 0.4
 n = 100
 z=(pbar-prop)/sqrt((prop*(1-prop))/n)
 z        
 alpha=0.05
 crit.point=qnorm(1-alpha)#p=0.05 one tailed (upper)
 crit.point
########### p value
pbar = 0.47
 prop = 0.4
 n = 100
 z=(pbar-prop)/sqrt((prop*(1-prop))/n)
 z        
 alpha=0.1
 crit.point1=qnorm(1-alpha)#p=0.1 one tailed (upper)
 crit.point1
 pval = 1-pnorm(z, lower.tail=TRUE)  # upper tail 
 pval 
############# two tailed test
 pbar1 = 0.04           # sample proportion 
 prop1 = 0.02                # hypothesized value 
 n = 1000                 # sample size 
 z1=(pbar1-prop1)/sqrt((prop1*(1-prop1))/n)
 z1                      # test statistic 
 pval = 2*pnorm(z1, lower.tail=FALSE)  # upper tail 
 pval 
### the number of hours of sleep for each of 24 students in class.
sleep=c(7.75,8.5,8,6,8,6.33,8.17,7.75,7,6.5,8.75,8,7.5,3,6.25,8.5,9,6.5,9,9.5,9,8,8,9.5)
nine.hrs=ifelse(sleep>=9,"yes","no")
table(nine.hrs)
nine.hrs 
y=5;n=24
test=prop.test(y,n,p=0.5,alternative="two.sided",correct=FALSE)
test

##########################################################################
######################  Chapter 6 ########################################

## Test a difference between paired measurement ##
Before<-c(115, 112, 107, 119, 115, 138, 126, 108, 104, 115)
During<-c(128, 115, 106, 128, 122, 145, 132, 109, 102, 117)

library(MASS) 
t.test(Before, During, paired=TRUE)


## Test a difference between paired measurement ##
placebo <-c(78, 54, 142, 25, 101, 99, 94, 107, 64)
treatment <-c(79, 48, 52, 15, 61, 107, 77, 54, 5)

library(MASS) 
t.test(treatment, placebo, paired=TRUE)


## One sided test for two independent sample  ##
A <- c(17, 19, 15, 18, 21, 18)
B <- c(18, 15, 13, 16, 13)

library(MASS)
t.test(B, A,var.equal=T, alternative="less")


## Comparing two population proportions ##
library(MASS)
prop.test(c(7,13),c(10000,5000), correct = F)
#################################################################









