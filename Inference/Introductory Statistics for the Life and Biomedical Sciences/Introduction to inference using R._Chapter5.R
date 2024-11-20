##########################################################
#                                                        #
#                                                        #
# Introduction to Statistical inference using R          #
#                                                        #
# 2024                                                   #
# Ziv Shkedy                                             #
# >eR-BioStat                                            #
# Chapter 5                                              #
#                                                        #
#                                                        #
##########################################################


###########################################################
################# Chapter 5           #####################
###########################################################
 

 
 
####  Example: critical value t distribution ##############

x=c(22,19,17,26,21,20,29,27,22)
xbar=mean(x)
mu = 21                
s = sd(x)
n = length(x)
t = (xbar-mu)/(s/sqrt(n))
t
alpha = .05 
crit.val = qt(1-alpha, n-1, lower.tail = TRUE) 
crit.val     

#### heights and weights for American women aged 30–39 ####

womenheight=women$height
t.test(womenheight,mu=60,conf.level=0.90)


#### example: case a ######################################

xbar=50.8;s=sqrt(1.6);n=100;H0=51
test.statgy=(xbar-H0)/(s/sqrt(n))
test.statgy
crit.point1=qnorm(0.95,lower.tail=TRUE)#p=0.05 one tailed
crit.point1


#### example: case c ##################################### 

xbar=118;s=sqrt(98);n=50;H0=115
test.statcrop=(xbar-H0)/(s/sqrt(n))
test.statcrop
alpha = 0.05 
crit.pointcrop = qnorm(1-alpha/2)
crit.pointcrop 
-crit.pointcrop
c(-crit.pointcrop,crit.pointcrop)


#### example: p value ###################################

x=c(22,19,17,26,21,20,29,27,22)
xbar=mean(x)
mu = 21                
s = sd(x)
n = length(x)
t = (xbar-mu)/(s/sqrt(n))
alpha = .05 
pval = pt(t,df=n-1, lower.tail=FALSE) 
pval


#### example: p value (with t distribution) ############

xbar=50.8;s=sqrt(1.6);n=100;H0=51
test.statgy=(xbar-H0)/(s/sqrt(n))
test.statgy
pval1 = pt(test.statgy,df=n-1,lower.tail=TRUE) 
pval1


#### example: p value (two sided with N() distribution) ####

bar=118;s=sqrt(98);n=50;H0=115
test.statcrop=(xbar-H0)/(s/sqrt(n))
test.statcrop 
2*(1-pnorm(test.statcrop))                
 
 
#### Test a difference between paired measurement  ####

Before<-c(115, 112, 107, 119, 115, 138, 126, 108, 104, 115)
During<-c(128, 115, 106, 128, 122, 145, 132, 109, 102, 117)

library(MASS) 
t.test(Before, During, paired=TRUE)


#### Test a difference between paired measurement ####
placebo <-c(78, 54, 142, 25, 101, 99, 94, 107, 64)
treatment <-c(79, 48, 52, 15, 61, 107, 77, 54, 5)

library(MASS) 
t.test(treatment, placebo, paired=TRUE)

#### Test a difference between paired measurement ####

before<-sleep$extra[sleep$group == 1]
after<-sleep$extra[sleep$group == 2]
diff<-before-after
plot(c(1:10),diff)
abline(0,0,col=2)
mean(before)
mean(after)

t.test(before,after, paired = TRUE)


#### One sided test for two independent sample  ####
A <- c(17, 19, 15, 18, 21, 18)
B <- c(18, 15, 13, 16, 13)

library(MASS)
t.test(B, A,var.equal=T, alternative="less")

#### Example: chick weight data ####################

x<-chickwts$weight[chickwts$feed=="horsebean"]
y<-chickwts$weight[chickwts$feed=="linseed"]
mean(x)
mean(y)
boxplot(x,y)
t.test(x,y,var.equal=T)


###########################################################
################# End of Chapter 5    #####################
###########################################################
 

 

