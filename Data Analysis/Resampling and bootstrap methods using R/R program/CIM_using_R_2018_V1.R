##########################################################################################
##########################################################################################
#                                                                                        #
#                                                                                        #
#  COMPUTER INTENSIVE METHODS USING R                                                    #
#                                                                                        #
#  Ziv Shkedy                                                                            #
#  2018                                                                                  #
#                                                                                        #
#  R packages to install: bootstrap                                                      #
#                                                                                        #
##########################################################################################
##########################################################################################

library(bootstrap)

help(bootstrap)
stamp
help(stamp)
hist(stamp$Thickness,nclass=20)
mean(stamp$Thickness)

help(diabetes)
plot(diabetes$age,diabetes$logCpeptide)
cor(diabetes$age,diabetes$logCpeptide)



##########################################################################################
#                                                                                        #
#                                                                                        #
# 1. The accuracy of the sample mean                                                     #
# E&T:Chapter 2                                                                          #
#                                                                                        #
#                                                                                        #
##########################################################################################

help(mouse.c)


mouse.c
mouse.t
mean(mouse.c)
sqrt(var(mouse.c)/9)
mean(mouse.t)
sqrt(var(mouse.t)/7)



x<-rnorm(100,0,1)
hist(x,nclass=50,probability=TRUE)
mean(x)
x1<-seq(from=-3,to=3,length=1000)
dx<-dnorm(x1,0,1)
lines(x1,dx)

mx<-c(1:10000)
for(i in 1:10000)
{
x<-rnorm(100,0,1)
mx[i]<-mean(x)
}

hist(mx,nclass=50,probability=TRUE)
x1<-seq(from=-1,to=1,length=1000)
dx<-dnorm(x1,0,sqrt(1/100))
lines(x1,dx)
mean(mx)
var(mx)


##########################################################################################
#                                                                                        #
#                                                                                        #
# 2. Random sample for a population                                                      # 
# E&T: Chapter 3                                                                         #
#                                                                                        #
#                                                                                        #
##########################################################################################

help(law)

par(mfrow=c(1,2))
plot(law82$LSAT,law82$GPA,xlim=c(450,700),ylim=c(2.5,3.5))
points(law$LSAT,law$GPA,pch="o",col=2)
plot(law$LSAT,law$GPA,pch="o",xlim=c(450,700),ylim=c(2.5,3.5))

law
cor(law82$LSAT,law82$GPA)
cor(law$LSAT,law$GPA)

par(mfrow=c(1,1))
boxplot(law$LSAT,law82$LSAT,names=c("Random sample, n=15","population"))
mean(law$LSAT)
sqrt(var(law$LSAT)/15)
mean(law82$LSAT)
var(law82$LSAT)

m1<-mean(law$LSAT)
sig1<-sqrt(var(law$LSAT)/15)
m1
sig1
x1<-seq(from=550,to=650,length=1000)
dx<-dnorm(x1,m1,sig1)
plot(x1,dx,type="l")
lines(c(m1,m1),c(0,0.05),col=2)
lines(c(m1,m1)+sig1,c(0,0.05),col=3)
lines(c(m1,m1)-sig1,c(0,0.05),col=3)

pnorm(m1+sig1,m1,sig1)-pnorm(m1-sig1,m1,sig1)

r<-law82$LSAT/law82$GPA
hist(r,nclass=25)
mean(r)


##########################################################################################
#                                                                                        #
#                                                                                        #
# 3. The empirical distribution function and the plug-in principle                       # 
# E&T: Chapter 4                                                                         #
#                                                                                        #
#                                                                                        #
##########################################################################################

par(mfrow=c(1,1))
x<-seq(from=-3,to=3,length=1000)
dx<-dnorm(x,0,1)
plot(x,dx,type="l")

x1<-rnorm(10000,0,1)
hist(x1,nclass=50,col=1,probability=T)

par(mfrow=c(1,2))
x2<-rnorm(1000,0,1)
x2<-sort(x2)
px<-pnorm(x2,0,1)
plot(x2,px,type="l")
n<-length(x2)
px.e<-c(1:length(x2))/n
plot(x2,px.e,type="s")

par(mfrow=c(1,1))
x2<-rnorm(50,0,1)
x2<-sort(x2)
x<-seq(from=-3,to=3,length=1000)
px<-pnorm(x,0,1)
plot(x,px,type="l")
n<-length(x2)
px.e<-c(1:length(x2))/n
lines(x2,px.e,type="s")

nsim<-1000
mx<-c(1:nsim)
for(i in 1:nsim)
{
x<-rnorm(10,0,1)
mx[i]<-mean(x)
}


hist(mx,nclass=20,col=2)

nsim<-1000
mx<-c(1:nsim)
for(i in 1:nsim)
{
x<-rbinom(500,1,0.75)
mx[i]<-mean(x)
}

hist(mx,nclass=20,col=0)



##########################################################################################
#                                                                                        #
#                                                                                        #
#  The bootstrap estimate of the standard error                                          #
#  E&T: Chapter 6                                                                        #
#                                                                                        #
#                                                                                        #
##########################################################################################



##########################################################################################
# Example 1: the bootstrap estimate of the standard error for the mean                   # 
##########################################################################################



x <- c(11.201, 10.035, 11.118, 9.055, 9.434, 9.663, 10.403, 11.662, 9.285,8.84)
var(x)
var(x)/10
n<-length(x)

####################################
# non parametric bootstrap         # 
####################################


B<-1000
mx<-c(1:B)
for(i in 1:B){
cat(i)
boot.i<-sample(x,n,replace=T)
mx[i]<-mean(boot.i)
}
var(mx)

####################################
# Parametric bootstrap             # 
####################################

B<-1000
MLx<-mean(x)
Varx<-var(x)
mx<-c(1:B)
for(i in 1:B){
cat(i)
boot.i<-rnorm(n,MLx,sqrt(Varx))
mx[i]<-mean(boot.i)
}
var(mx)

##########################################################################################
# Example 2: the correlation coefficient                                                 #
##########################################################################################

x<-c(29,435,86,1090,219,503,47,3524,185,98,952,89)
y<-c(127,214,133,208,153,184,130,217,141,154,194,103)
cbind(x,y)
plot(x,y)
cor.obs<-cor(x,y)
n<-length(x)
index<-c(1:n)
B<-1000
obs.8<-cor.xy<-c(1:B)
for(i in 1:B)
{
cat(i)
boot.i<-sample(index,n,replace=T)
obs.8[i]<-sum(boot.i==8)
x.b<-x[boot.i]
y.b<-y[boot.i]
cor.xy[i]<-cor(x.b,y.b)
}

plot(obs.8)
plot(obs.8,cor.xy)
boxplot(split(cor.xy,obs.8))

hist(cor.xy,probability=T,col=0,nclass=30)
lines(c(cor.obs,cor.obs),c(0,10),lwd=3)

##########################################################################################
# Example 3 The air quality data                                                         #
##########################################################################################

attach(airquality)
Ozone<-na.omit(Ozone)
B<-10000
q.boot<-matrix(0,B,3)
for(b in 1:B)
{
Ozone.boot<-sample(Ozone,size=116,replace=TRUE)
q.boot[b,]<-quantile(Ozone.boot,probs=c(0.25,0.5,0.75))
}
 
par(mfrow=c(2,2))
hist(q.boot[,1],nclass=50)
hist(q.boot[,2],nclass=50)
hist(q.boot[,3],nclass=50)
boxplot(q.boot[,1],q.boot[,2],q.boot[,3])


##########################################################################################
#                                                                                        #
#                                                                                        #
# The bootstrap standard error: examples                                                 # 
# E&T: Chapter 7                                                                         #
#                                                                                        #
#                                                                                        #
##########################################################################################

##########################################################################################
# Example 1: the score data                                                              #
##########################################################################################


help(scor)
head(scor)
boxplot(scor)

pairs(scor)
cov(scor)[5,3]
r.obs<-mean(scor$sta/scor$alg)

n<-length(scor$sta)
B<-1000
index<-c(1:n)
sig35<-ratio.b<-c(1:B)
for(i in 1:B)
{
index.b<-sample(index,n,replace=TRUE)
scor.b<-scor[index.b,]
cov.b<-cov(scor.b)
sig35[i]<-cov.b[5,3]
ratio.b[i]<-mean(scor.b$sta/scor.b$alg)
}

hist(sig35,nclass=50)
lines(c( 121.8706,121.8706),c(0,500),col=2,lwd=3)
var(sig35)
sqrt(var(sig35))


par(mfrow=c(1,2))
plot(scor$sta,scor$alg)
abline(0,1)
hist(scor$sta/scor$alg,main=" ")
m.r<-mean(scor$sta/scor$alg)
lines(c(m.r,m.r),c(0,100),col=2,lwd=3)

par(mfrow=c(1,1))
hist(ratio.b,nclass=50)
lines(c(m.r,m.r),c(0,500),col=2,lwd=3)
lines(c(0.8,0.8),c(0,500),col=4,lwd=3)


var(ratio.b)
sum(ratio.b<0.8)


##########################################################################################
# Example 2: the fuel data                                                               #
##########################################################################################

fuel.frame<-read.table('C:/projects/cim/UpdatesSlides_2017/Data/fuel.txt')
names(fuel.frame)<-c("ID","Weight","Mileage")
par(mfrow=c(1,1))

x<-fuel.frame$Weight
y<-fuel.frame$Mileage
y<-y[order(x)]
x<-sort(x)
plot(x,y)
fit.lm<-lm(y~x+x^2)
par(mfrow=c(1,1))
plot(x,y)
lines(x,fit.lm$fit,lwd=2)
fit.lo1<-loess(y~x)
fit.lo2<-loess(y~x,span=0.5)
lines(x,fit.lo1$fit,lwd=2,col=4)
lines(x,fit.lo2$fit,lwd=2,col=3)
legend(3000,35,c("regression","loess:lambda=0.75","loess: lambda=0.5"),col=c(1,4,3),lty=c(1,1,1))

x<-3200
newdat<-data.frame(x)
predict(fit.lm,newdat)
predict(fit.lo2,newdat)
predict(fit.lo1,newdat)



####################################
# Non parametric bootstrap         # 
####################################

x<-fuel.frame$Weight
y<-fuel.frame$Mileage
y<-y[order(x)]
x<-sort(x)
plot(x,y)
n<-length(x)
index<-c(1:n)
B<-1000
x.boot<-y.boot<-fit.b.lm<-fit.b.lo<-matrix(0,n,B)
x.b<-3000
newdat<-data.frame(x.b)
pred.lm<-pred.lo<-c(1:B)
for(i in 1:B){
cat(i)
boot.i<-sample(index,n,replace=T)
x.b<-x[boot.i]
y.b<-y[boot.i]
y.b<-y.b[order(x.b)]
x.b<-sort(x.b)
fit.lm.i<-lm(y.b~x.b+x.b^2)
fit.lo.i<-loess(y.b~x.b,span=0.5)
x.boot[,i]<-x.b
y.boot[,i]<-y.b
fit.b.lm[,i]<-fit.lm.i$fit
fit.b.lo[,i]<-fit.lo.i$fit
pred.lo[i]<-predict(fit.lo.i,newdat)
pred.lm[i]<-predict(fit.lm.i,newdat)
}



plot(x,y,ylim=c(18,40))
for(i in 1:B)
{
lines(x.boot[,i],fit.b.lm[,i])	
}


plot(x,y,ylim=c(18,40))
for(i in 1:B)
{
lines(x.boot[,i],fit.b.lo[,i])	
}

####################################
# prediction                       # 
####################################


par(mfrow=c(1,2))
hist(pred.lm,col=0,main=" ")
title("linear regression: r(3200)")
hist(pred.lo,col=0,main=" ")
title("loess: r(3200)")

var(pred.lm)
var(pred.lo)

quantile(pred.lm,probs=c(0.025,0.975))
quantile(pred.lo,probs=c(0.025,0.975))


####################################
# confidence intervals             # 
####################################



cir<-quantile(pred.lm,probs=c(0.025,0.975))
cilo<-quantile(pred.lo,probs=c(0.025,0.975))
par(mfrow=c(1,2))
hist(pred.lm,col=0,nclass=25,probability=T)
lines(c(cir[1],cir[1]),c(0,1),lwd=2,col=3)
lines(c(cir[2],cir[2]),c(0,1),lwd=2,col=3)
title("linear regression: r(3000)")
hist(pred.lo,col=0,nclass=25,probability=T)
lines(c(cilo[1],cilo[1]),c(0,1),lwd=2,col=3)
lines(c(cilo[2],cilo[2]),c(0,1),lwd=2,col=3)
title("loess: r(3000)")


##########################################################################################
#                                                                                        #
#                                                                                        #
# Bootstrap confidence intervals                                                         # 
# E&T: Chapter 12,13,14                                                                  #
#                                                                                        #
#                                                                                        #
##########################################################################################

par(mfrow=c(1,1))
z<-c(94,197,16,38,99,141,23)
y<-c(52,104,146,10,51,30,40,27,46)
z
y
boxplot(z,y)


####################################
# confidence interval (classic)    # 
####################################


z <- c(94, 197, 16, 38, 99, 141, 23)
z
t.hat<-mean(z)
se.t.hat<-sqrt(var(z)/7)
t.hat
se.t.hat

qnorm(0.975,0.1)
z <- c(94, 197, 16, 38, 99, 141, 23)
z
t.hat<-mean(z)
se.t.hat<-sqrt(var(z)/7)
t.hat
se.t.hat

C.alpha<-qnorm(0.975,0,1)
C.alpha

x<-seq(from=-3,to=3,length=1000)
dx<-dnorm(x,0,1)
plot(x,dx,type="l")
lines(c(C.alpha,C.alpha),c(0,1),col=2)



t.test(z,conf.level=0.9)


#####################################
#improve  interval                 #
#####################################

B=10000
t.boot<-c(1:B)
for(b in 1:B)
{
x.boot<-sample(z,size=length(z),replace=T)
t.boot[b]<-mean(x.boot)
}

bias.t<-mean(tboot)-t.hat
bias.t

vt<-sqrt(var(t.boot)
vt
C.alpha<-qnorm(0.975,0,1)
C.alpha

c(t.hat-bias.t-se.t.hat*C.alpha,t.hat-bias.t+se.t.hat*C.alpha)





#####################################
#improve  interval                 #
#####################################

B=10000
t.boot<-c(1:B)
for(b in 1:B)
{
x.boot<-sample(z,size=length(z),replace=T)
t.boot[b]<-mean(x.boot)
}


t.up<-quantile(t.boot,probs=c(0.95))
t.lo<-quantile(t.boot,probs=c(0.05))
hist(t.boot,nclass=20,col=0,probability=TRUE)
lines(c(t.up,t.up),c(0,20),col=2)
lines(c(t.lo,t.lo),c(0,20),col=2)


c(2*t.hat-t.lo,2*t.hat-t.up)





#####################################
#Bootstrap standard normal interval #
#####################################

B=10000
t.boot<-c(1:B)
for(b in 1:B)
{
t.boot[b]<-rnorm(1,t.hat,se.t.hat)
}

quantile(t.boot,probs=c(0.05,0.95))
lo<-quantile(t.boot,probs=c(0.05,0.95))[1]
up<-quantile(t.boot,probs=c(0.05,0.95))[2]

hist(t.boot,probability=T,nclass=50)
lines(c(45.3,45.3),c(0,0.1),col=2)
lines(c(128.4,128.4),c(0,0.1),col=2)
lines(c(37.82004,37.82004),c(0,0.2),col=3)
lines(c(135.89425,135.89425),c(0,0.2),col=3)
lines(c(lo,lo),c(0,0.2),col=4)
lines(c(up,up),c(0,0.2),col=4)


#####################################
#Bootstrap t interval               #
#####################################

B=10000
t.boot<-c(1:B)
for(b in 1:B)
{
x.boot<-sample(z,size=length(z),replace=T)
se.boot<-sqrt(var(x.boot)/length(z))
t.boot[b]<-(mean(x.boot)-t.hat)/se.boot
}
quantile(t.boot,probs=c(0.05,0.95))
qt(0.95,6)
qt(0.05,6)

hist(t.boot,probability=T,nclass=100,ylim=c(0,0.5),xlim=c(-10,10))
xx<-seq(from=-3,to=3,length=1000)
dx2<-dt(xx,6)
lines(xx,dx2,col=2)
lines(rep(qt(0.95,6),2),c(0,0.4),col=2)
calpha<-quantile(t.boot,probs=c(0.95))
lines(rep(calpha,2),c(0,0.4),col=4)
qt(0.95,6)
quantile(t.boot,probs=c(0.95))




up<-quantile(t.boot,probs=c(0.95))
lo<-quantile(t.boot,probs=c(0.05))
c(t.hat+se.t.hat*lo,t.hat+se.t.hat*up)




#####################################
#Bootstrap percentiles interval     #
#####################################
B=10000
t.boot<-c(1:B)
for(b in 1:B)
{
x.boot<-sample(z,size=length(z),replace=T)
t.boot[b]<-mean(x.boot)
}

quantile(t.boot,probs=c(0.05,0.95))
lo<-quantile(t.boot,probs=c(0.05,0.95))[1]
up<-quantile(t.boot,probs=c(0.05,0.95))[2]
hist(t.boot,probability=T,nclass=50)
lines(c(45.3,45.3),c(0,0.1),col=2)
lines(c(128.4,128.4),c(0,0.1),col=2)
lines(c(37.82004,37.82004),c(0,0.2),col=3)
lines(c(135.89425,135.89425),c(0,0.2),col=3)
lines(c(lo,lo),c(0,0.2),col=4)
lines(c(up,up),c(0,0.2),col=4)


xx<-seq(from=-3,to=3,length=1000)
dx1<-dnorm(xx,0,1)
dx2<-dt(xx,6)
plot(xx,dx1,type="l",xlab=" ",ylab=" ",xaxt="n",yaxt="n")
lines(xx,dx2,col=2)
legend(-3,0.4,c("N(0,1)","t(6)"),lty=c(1,1),col=c(1,2))
lines(c(1.645,1.645),c(0,0.4))
lines(rep(qt(0.95,6),2),c(0,0.4),col=2)

Bootstrap interval for the standard error of the mean


####################################
#Bootstrap interval for the        #
#standard error of the mean        #
####################################


z <- c(94, 197, 16, 38, 99, 141, 23)  
z
t.hat<-mean(z)
se.t.hat<-sqrt(var(z)/7)
t.hat
se.t.hat
t.test(z,conf.level=0.9)


B=10000
t.boot<-c(1:B)
for(b in 1:B)
{
x.boot<-sample(z,size=length(z),replace=T)
t.boot[b]<-sqrt(var(x.boot)/7)
}
quantile(t.boot,probs=c(0.05,0.95))


lo<-quantile(t.boot,probs=c(0.05,0.95))[1]
up<-quantile(t.boot,probs=c(0.05,0.95))[2]
hist(t.boot,probability=T,nclass=50)
lines(c(lo,lo),c(0,0.2),col=2,lwd=2)
lines(c(up,up),c(0,0.2),col=2,lwd=2)
lines(c(se.t.hat,se.t.hat),c(0,0.2),col=3,lwd=2)



sqrt(var(z))/mean(z)

#non parametric
B=10000
t.boot<-c(1:B)
for(b in 1:B)
{
x.boot<-sample(z,size=length(z),replace=T)
t.boot[b]<-sqrt(var(x.boot))/mean(x.boot)
}
quantile(t.boot,probs=c(0.05,0.95))
lo<-quantile(t.boot,probs=c(0.05,0.95))[1]
up<-quantile(t.boot,probs=c(0.05,0.95))[2]
hist(t.boot,probability=T,nclass=50)
lines(c(lo,lo),c(0,5),col=2,lwd=2)
lines(c(up,up),c(0,5),col=2,lwd=2)


##########################################################################################
# Example: The law school data - correlation                                             #
##########################################################################################


library(bootstrap)
law
B<-1000
cor.b<-c(1:B)
n<-length(law$LSAT)
index<-c(1:n)

for(i in 1:B)
{
index.b<-sample(index,n,replace=TRUE)
LAST.b<-law$LSAT[index.b]
GPA.b<-law$GPA[index.b]
cor.b[i]<-cor(LAST.b,GPA.b)
}


par(mfrow=c(1,1))
plot(law$LSAT,law$GPA,xlim=c(450,700),ylim=c(2.5,3.5))
cor.low<-cor(law$LSAT,law$GPA)
cor.low
hist(cor.b,nclass=50)
lines(c(cor.low,cor.low),c(0,100),col=2,lwd=3)
ci<-quantile(cor.b,probs=c(0.025,0.975))
ci
lines(c(ci[1],ci[1]),c(0,100),col=4,lwd=3)
lines(c(ci[2],ci[2]),c(0,100),col=4,lwd=3)





##########################################################################################
#                                                                                        #
#                                                                                        #
# Bootstrap tests                                                                        #
# E&T: Chapter 16                                                                        #
#                                                                                        #
#                                                                                        #
##########################################################################################


######################################################
## One sample bootstrap                             ##
######################################################

x1<-seq(from=-4,to=4,length=1000)
dx1<-dnorm(x1,0,1)
plot(x1,dx1,type="l",xlab=" ",ylab=" ")
lines(c(2.5,2.5),c(0,0.4),col=2,lwd=2)



z <- c(94, 197, 16, 38, 99, 141, 23)
z
mean(z)
sd(z)
t.test(z,mu=129)$statistic
t.test(z,mu=129,alternative="less")



x<-seq(from=-3,to=3,length=1000)
dx<-dt(x,6)
plot(x,dx,type="l")
lines(c(-1.67,-1.67),c(0,1),lwd=3,col=2)
pt(-1.67,6)

mz <- mean(z)
mz
n <- length(z)
z.tilde <- z - mz + 129
mean(z.tilde)

nz<-length(z)
t.obs<-t.test(z,mu=129)$statistic
B<-5000
t.boot<-c(1:B)
for(b in 1:B)
{
z.b<-sample(z.tilde,size=nz,replace=T)
t.boot[b]<-t.test(z.b,mu=129)$statistic
}

Pmc<-(1+sum(abs(t.boot) > abs(t.obs)))/(B+1)
Pmc


hist(t.boot,nclass=50,probability=T)
lines(c(t.obs,t.obs),c(0,1),lwd=3,col=2)


######################################
## two samples (16.1)               ##
######################################


x1<-seq(from=-4,to-=4,length=1000)
dx1<-dnorm(x,0,1)
dx2<-dnorm(x,1,1)

plot(x,dx1,type="l",xlab=" ",ylab=" ")
lines(x,dx2,col=2)

plot(x,cumsum(dx1),type="l",xlab=" ",ylab=" ")
lines(x,cumsum(dx2),col=2)

help(mouse.c)
y <- c(10, 27, 31, 40, 46, 50, 52, 104, 146)
x <- c(16, 23, 38, 94, 99, 141, 197)
y
x
mean(y)
mean(x)
boxplot(x,y)

t.test(x,y,var.equal=TRUE)

z <- c(x, y)
m <- length(x)
n <- length(y)
mn <- m + n
t.obs <- mean(x) - mean(y)
t.obs
B<-5000
t.boot<-c(1:B)
for(b in 1:B)
{
z.b<-sample(z,size=mn,replace=T)
x.b<-z.b[1:n]
y.b<-z.b[(n+1):mn]
t.boot[b]<-mean(x.b)-mean(y.b)
}
hist(t.boot,nclass=50,probability=T)
lines(c(t.obs,t.obs),c(0,1),lwd=3,col=2)
(sum(t.boot>t.obs)+1)/(B+1)
t.test(x,y,alternative="greater",var.equal=TRUE)


t.obs <- t.test(x,y)$statistic
t.obs
B<-2500
t.boot<-c(1:B)
for(b in 1:B)
{
z.b<-sample(z,size=mn,replace=T)
x.b<-z.b[1:n]
y.b<-z.b[(n+1):mn]
t.boot[b]<-t.test(x.b,y.b)$statistic
}
hist(t.boot,nclass=50,probability=T)
lines(c(t.obs,t.obs),c(0,1),lwd=3,col=2)
Pmc<-(1+sum(t.boot > t.obs))/(B+1)
Pmc


hist(t.boot,nclass=150,probability=T)
dstat<-dt(sort(t.boot),14)
lines(sort(t.boot),dstat,col=2,lwd=2)





######################################
## two samples (16.2)               ##
######################################



y <- c(10, 27, 31, 40, 46, 50, 52, 104, 146)
x <- c(16, 23, 38, 94, 99, 141, 197)
z<-c(x,y)
m <- length(x)
n <- length(y)
my<-mean(y)
mx<-mean(x)
mz<-mean(z)
x.tilde<-x-mx+mz
y.tilde<-y-my+mz
mean(x.tilde)
mean(y.tilde)



t.obs <- t.test(x,y)$statistic
t.obs
B<-1000
t.boot<-c(1:B)
for(b in 1:B)
{

x.b<-sample(x.tilde,m,replace=TRUE)
y.b<-sample(y.tilde,n,replace=TRUE)
t.boot[b]<-t.test(x.b,y.b)$statistic
}
hist(t.boot,nclass=50,probability=T)
lines(c(t.obs,t.obs),c(0,1),lwd=3,col=2)
Pmc<-(1+sum(t.boot > t.obs))/(B+1)
Pmc


###############################################
# City population data                        #
# pop1920: u                                  # 
# pop1930: x                                  #
###############################################


city<-read.table('C:/projects/cim/UpdatesSlides_2017/Data/city.txt', header=FALSE, 
                           na.strings=".", dec=".",  strip.white=TRUE,
                col.names=c("pop1920","pop1930"))

city
n<-length(city$pop1920)
u<-city$pop1920
x<-city$pop1930
plot(u,x,xlab="1920 population",ylab="1930 population")
abline(0,1)
cbind(x[1:10],u[1:10])
t.obs<-mean(x)/mean(u)
t.obs
B<-5000
t.boot<-c(1:B)
for(b in 1:B)
{
index<-sample(c(1:n),size=n,replace=T)
u.boot<-u[index]
x.boot<-x[index]
t.boot[b]<-mean(x.boot)/mean(u.boot)
}

hist(t.boot,nclass=50,probability=T,main=" ",xlab="bootstrap replicates")
lines(c(t.obs,t.obs),c(0,20),lwd=3,col=2)
(1+sum(t.boot > t.obs))/(B+1)
## pair t-test ##
z<-u-x
mean(z)
z.tilde<-z-mean(z)
mean(z.tilde)
nz<-length(z)
t.test(z,mu=0)
t.obs<-t.test(z,mu=0)$statistic
t.obs
B<-5000
t.boot<-c(1:B)
for(b in 1:B)
{
z.b<-sample(z.tilde,size=nz,replace=T)
t.boot[b]<-t.test(z.b,mu=0)$statistic
}
hist(t.boot,nclass=50,probability=T,main=" ",xlab="bootstrap replicates"
,xlim=c(-7,7))
lines(c(t.obs,t.obs),c(0,20),lwd=3,col=2)
Pmc<-(1+sum(abs(t.boot) > abs(t.obs)))/(B+1)
Pmc

###############################################
# Example 3: correlation test                 #
# The cars data                               #
###############################################

help(cars)
plot(cars$speed,cars$dist)


x<-cars$speed
n<-length(x)
y<-cars$dist
index<-c(1:n)
rho<-cor(x,y)
rho
B<-1000
coeff.boot<-c(1:B)
index<-c(1:n)
for(b in 1:B)
{
index.b<- sample(index,size=n,replace=TRUE)
x.boot<- x[index.b]
y.boot<- y[index.b]
coeff.boot[b]<-cor(x.boot,y.boot)
}

hist(coeff.boot,nclass=50)
lines(c(rho,rho),c(0,500),col=2,lwd=3)


n<-length(x)
B<-1000
coeff.boot<-c(1:B)
for(b in 1:B)
{
x.boot<- sample(x,size=n,replace=TRUE)
y.boot<- sample(y,size=n,replace=TRUE)
coeff.boot[b]<-cor(x.boot,y.boot)
}

hist(coeff.boot,nclass=50,xlim=c(-1,1))
lines(c(rho,rho),c(0,500),col=2,lwd=3)
sum(abs(coeff.boot)>rho)




MLx<-mean(x)
MLy<-mean(v)
Sigx<-sqrt(var(x))
Sigy<-sqrt(var(y))
n<-length(x)
B<-1000
coeff.boot<-c(1:B)
for(b in 1:B)
{
x.boot<- rnorm(n,MLx,Sigx)
y.boot<- rnorm(n,MLy,Sigy)
coeff.boot[b]<-cor(x.boot,y.boot)
}

hist(coeff.boot,nclass=50,xlim=c(-1,1))
lines(c(rho,rho),c(0,500),col=2,lwd=3)
sum(abs(coeff.boot)>rho)


##########################################################################################
#                                                                                        #
#                                                                                        #
# Permutaion tests                                                                       #
# E&T: Chapter 15                                                                        #
#                                                                                        #
#                                                                                        #
##########################################################################################


x<-c(94,197,16,38,99,141,23)
y<-c(52,104,146,10,51,30,40,27,46)
z<-(x,y)
z
n<-length(x)
m<-length(y)
mean(x)-mean(y)
boxplot(x,y)
t.test(x,y,alternative="greater",var.equal=TRUE)
t.obs<-t.test(x,y,alternative="greater",var.equal=TRUE)$statistic

xt<-seq(from=-3,to=3,length=1000)
d.t<-dt(xt,14)
plot(xt,d.t,type="l")
lines(c(1.12,1.12),c(0,0.5),col=3,lwd=3)

z<-c(x,y)
r.x<-rep("x",n)
r.y<-rep("y",m)
r.x<-c(r.x,r.y)
data.frame(sort(z),r.x[order(z)])

#z<-c(94,197,16)
#y<-c(52,104,146)
#n<-length(z)
#m<-length(y)



z<-c(x,y)
B<-10000
v.boot<-t.boot<-c(1:B)
for(b in 1:B)
{
z.boot<-sample(z,size=n+m,replace=FALSE)
v.voot[b]<-var(z.boot)
x.boot<-z.boot[1:n]
y.boot<-y.boot[(n+1):(n+m)]
t.boot[b]<-t.test(x.boot,y.boot,alternative="greater",var.equal=TRUE)$statistic
}
hist(t.boot,nclass=50,probability=TRUE)
lines(c(t.obs,t.obs),c(0,10),col=3,lwd=3)
(1+sum(t.boot>t.obs))/(B+1)


##########################################################################################
#                                                                                        #
#                                                                                        #
# Estimates for bias (the patch data)                                                    #
# E & T: Chapter 10                                                                      #
#                                                                                        #
#                                                                                        #
##########################################################################################


library(bootstrap)
help(patch)


y<-patch$y
z<-patch$z
mean(y)
mean(z)
theta.obs<-mean(y)/mean(z)

n<-length(z)
B<-1000
index<-c(1:n)
theta.boot<-c(1:B)
for(i in 1:B)
{	
cat(i)
i.boot<-sample(index, size=n, replace=T)
y.boot<-y[i.boot]
z.boot<-z[i.boot]
theta.boot[i]<-mean(y.boot)/mean(z.boot)
}	
	
hist(theta.boot,col=0,nclass=30,probability=T)
lines(c(theta.obs,theta.obs),c(0,5),lwd=2,col=6)



m.boot <- mean(theta.boot)
b.boot <- m.boot - theta.obs
b.boot

plot(c(1:B),theta.boot)
abline(m.boot,0,col=2)
abline(theta.obs,0,col=4)


plot(c(1:B),theta.boot,ylim=c(-0.1,-0.06))
abline(m.boot,0,col=2)
abline(theta.obs,0,col=4)


##########################################################################################
#                                                                                        #
#                                                                                        #
# Cross validation (the hormone data)                                                    #
# E & T: Chapter 17                                                                      #
#                                                                                        #
#                                                                                        #
##########################################################################################

hormone

y<-hormone$amount
x<-hormone$hrs

plot(x,y)

fit.lm<-lm(y~x)
summary(fit.lm)

plot(x,fit.lm$resid)
abline(0,0)

#####################################
# Leave one out CV                  #
#####################################


n <- length(x)
beta.cv <- fit.cv <- c(1:n)
for(i in 1:n) {
	cat(i)
	x.cv <- x[ - c(i)]
	y.cv <- y[ - c(i)]
	fit.lm.cv <- lm(y.cv ~ x.cv)
	fit.cv[i] <- fit.lm.cv$coeff[1] + fit.lm.cv$coeff[2]*x[i]
	beta.cv[i] <- fit.lm.cv$coeff[2]
}



res.cv <- y - fit.cv
cv.score <- sqrt(sum((res.cv^2))/n)
cv.score
cv.score/(sqrt(sum((y - fit.lm$fit)^2)/27))

plot(x,fit.lm$resid,ylim=c(-5,5))
points(x,res.cv,pch="+",col=2)
abline(0,0)

plot(x,beta.cv)
abline(fit.lm$coeff[2],0)


plot(x,y)
lines(x,fit.lm$fit)
x.10<-x[-c(10)]
y.10<-y[-c(10)]
points(x.10,y.10,col=2)
fit.lm.10<-lm(y.10~x.10)
summary(fit.lm.10)
lines(x.10,fit.lm.10$fit,col=2)

x[10]
y[10]


##########################################################################################
#                                                                                        #
#                                                                                        #
# The Jackknife (the airquality data)                                                    #
# E & T: Chapter 11                                                                      #
#                                                                                        #
#                                                                                        #
##########################################################################################

x <- na.omit(airquality$Ozone)
hist(x, col = 0)
mean(x)
n <- length(x)
n
var(x)/n


B<-1000
m.x<-c(1:B)
for(i in 1:B)
{
x.boot<-sample(x,n,replace=TRUE)
m.x[i]<-mean(x.boot)
}
hist(m.x,nclass=50)

mean(x)
n <- length(x)
var(x)/n
m.boot <- m.jack <- c(1:n)
 for(i in 1:n) {
	cat(i)
	x.jack <- x[ - c(i)]
	m.jack[i] <- mean(x.jack)
	x.boot <- sample(x, size = n, replace = T)
	m.boot[i] <- mean(x.boot)
}


par(mfrow=c(2,1))
hist(m.boot,nclass=10,xlim=c(30,50),main=" ")
hist(m.jack,nclass=10,xlim=c(30,50),main=" ")


m.b<-mean(m.boot)
m.j<-mean(m.jack)
par(mfrow=c(3,1))
hist(m.boot-m.b,nclass=10,main=" ",xlim=c(-10,8))
hist(m.jack-m.j,nclass=10,main=" ",xlim=c(-10,8))
hist(sqrt(n-1)*(m.jack-m.j),nclass=10,main=" ",xlim=c(-10,8))


##########################################
# estimation of bias: the patch data     #
##########################################

z<-patch$z
y<-patch$y
theta.obs <- mean(y)/mean(z)
theta.obs

n <- length(z)
m.jack <- c(1:n)
for(i in 1:n) {
	cat(i)
	z.jack <- z[ - c(i)]
	y.jack <- y[ - c(i)]
	m.jack[i] <- mean(y.jack)/mean(z.jack)
}
(n - 1) * (mean(m.jack) - theta.obs)



##########################################################################################
#                                                                                        #
#                                                                                        #
# Regression models (the hormone data , cell data and Tooth Strength Data)               #
# E & T : Chapter 9                                                                      #
#                                                                                        #
#                                                                                        #
##########################################################################################


help(hormone)
plot(hormone$hrs,hormone$amount)

n<-length(hormone$amount)
fit.lm<-lm(hormone$amount~hormone$hrs)
summary(fit.lm)
beta0<-summary(fit.lm)$coeff[1,1]
beta1<-summary(fit.lm)$coeff[2,1]
sigma<-2.378

#################################
# non parametric bootstrap      #
#################################


B<-10000
beta0.b<-beta1.b<-c(1:B)
index<-c(1:n)

for (i in 1:B)
{
index.b<-sample(index,n,replace=TRUE)
hormone.b<-hormone[index.b,]
fit.lm.b<-lm(hormone.b$amount~hormone.b$hrs)
beta0.b[i]<-summary(fit.lm.b)$coeff[1,1]
beta1.b[i]<-summary(fit.lm.b)$coeff[2,1]
}


hist(beta0.b,nclass=50)
hist(beta1.b,nclass=50)
quantile(beta0.b,probs=c(0.025,0.975))
quantile(beta1.b,probs=c(0.025,0.975))


#################################
# semi parametric bootstrap     #
# (bootstraping residuls)       #
#################################
y<-hormone$amount
x<-hormone$hrs
fit.lm <- lm(y ~ x)
ei <- fit.lm$resid
n <- length(x)
B <- 1000
beta0 <- beta1 <- c(1:B)
for(i in 1:B) {
	cat(i)
	e.boot <- sample(ei, size = n, replace = T)
	y.boot <- fit.lm$coeff[1] + fit.lm$coeff[2]*x + e.boot
	x.boot <- x
	fit.boot <- lm(y.boot ~ x.boot)
	beta0[i] <- fit.boot$coeff[1]
	beta1[i] <- fit.boot$coeff[2]
}

hist(beta0,nclass=50)
hist(beta1,nclass=50)
quantile(beta0,probs=c(0.025,0.975))
quantile(beta1,probs=c(0.025,0.975))

par(mfrow=c(1,1))

#################################
# parametric bootstrap          #
#################################

B<-10000
beta0.b<-beta1.b<-c(1:B)
amount.b<-c(1:n)
for (i in 1:B)
{
for(j in 1:n)
{
amount.b[j]<-rnorm(1,beta0+beta1*hormone$hrs[j],sigma)
}
fit.lm.b<-lm(amount.b~hormone$hrs)
beta0.b[i]<-summary(fit.lm.b)$coeff[1,1]
beta1.b[i]<-summary(fit.lm.b)$coeff[2,1]
}

hist(beta0.b,nclass=50)
hist(beta1.b,nclass=50)
quantile(beta0.b,probs=c(0.025,0.975))
quantile(beta1.b,probs=c(0.025,0.975))



##########################################################################################
# Example 2:  the cell data                                                              #
#E & T: Chapter 9, Section 9.6.& 9.7                                                     #
##########################################################################################

library(bootstrap)
help(cell)
plot(cell$dose,cell$log.surv,ylim=c(-12,0))
y<-cell$log.surv
x<-cell$dose
x[13]
y[13]
x2<-x^2
x13<-x[-c(13)]
x132<-x13^2
y13<-y[-c(13)]

##########################################################
# models for all data (n=13)                             #
##########################################################

plot(cell$dose,cell$log.surv,ylim=c(-12,0))
fit.lm2<-lm(y~-1+x+x2)
fit.lm2a<-lm(y13~-1+x13+x132)
summary(fit.lm2)
summary(fit.lm2a)
lines(x,fit.lm2$fit,col=2)
lines(x13,fit.lm2a$fit,col=4)

##########################################################
# models for all data (n=14)                             #
##########################################################

plot(cell$dose,cell$log.surv,ylim=c(-12,0))
fit.lm1<-lm(y~-1+x)
fit.lm2<-lm(y~-1+x+x2)
summary(fit.lm1)
summary(fit.lm2)
lines(x,fit.lm1$fit)
lines(x,fit.lm2$fit,col=2)



##########################################################
# robust regression models for all data (n=14)           #
##########################################################



library(rlm)
library(MASS)


plot(cell$dose,cell$log.surv,ylim=c(-12,0))
fit.lm1<-lm(y~-1+x)
fit.lm2<-lm(y~-1+x+x2)
lines(x,fit.lm1$fit)
lines(x,fit.lm2$fit,col=2)
fit.rob<-rlm(y~-1+x)
summary(fit.rob)
lines(x,fit.rob$fit,col=6)


fit.rob2<-rlm(y~-1+x+x2)
summary(fit.rob2)
plot(cell$dose,cell$log.surv,ylim=c(-12,0))
lines(x,fit.rob$fit)
lines(x,fit.rob2$fit,col=2)

##########################################################
# robust regression models for all data (n=14)           #
# non parametric bootstrap                               #
##########################################################


xu<-unique(x)
nu<-length(xu)
n<-length(y)
B<-1000
pred.rob<-matrix(0,nu,B)
dim(pred.rob)
index<-c(1:n)

for(i in 1:B)
{
index.b<-sample(x,n,replace=TRUE)
x.b<-x[index.b]
y.b<-y[index.b]
fit.rob.b<-rlm(y.b~-1+x.b)
#beta0<-summary(fit.rob.b)$coeff[1,1]
#beta1<-summary(fit.rob.b)$coeff[2,1]
beta1<-summary(fit.rob.b)$coeff[1,1]
#pred.rob[,i]<-beta0+beta1*xu
pred.rob[,i]<-beta1*xu
}


ci<-matrix(0,nu,2)
plot(cell$dose,cell$log.surv,ylim=c(-12,0))
for(i in 1:nu)
{
i<-1
xxx<-na.omit(pred.rob[i,])
xxx1<-xxx[xxx<0]
ci[i,]<-quantile(xxx1,probs=c(0.025,0.75))
}
ci
lines(xu,ci[,1],lty=2)
lines(xu,ci[,2],lty=2)
lines(x,fit.rob$fit)

hist(xxx1,nclass=50,main=" ")
lines(c(ci[1,1],ci[1,1]),c(0,100),col=2,lwd=3)
lines(c(ci[1,2],ci[1,2]),c(0,100),col=2,lwd=3)
lines(c(fit.rob$fit[1],fit.rob$fit[1]),col=4,lwd=3)



##########################################################
# RSS ans MSR                                            #
##########################################################

beta<-seq(from=-0.9,to=-0.5,length=10000)
RAS<-RSS<-MSR<-c(1:10000)
n<-length(x)
for(i in 1:10000)
{
y.hat<-beta[i]*x
r<-(y-y.hat)
r.q<-r^2
RSS[i]<-(1/n)*sum(r.q)
MSR[i]<-median(r.q)
RAS[i]<-(1/n)*sum(abs(r))
}

m.RSS<-min(RSS)
m.MSR<-min(MSR)
m.RAS<-min(RAS)
beta.rss<-beta[RSS==m.RSS]
beta.msr<-beta[MSR==m.MSR]
beta.ras<-beta[RAS==m.RAS]



plot(beta,RSS,type="l")
points(beta.rss,m.RSS,pch="+",cex=3)
m.RSS
beta.rss

plot(beta,MSR,type="l")
points(beta.msr,m.MSR,pch="+",cex=3)
m.MSR
beta.msr


plot(beta,RAS,type="l")
points(beta.ras,m.RAS,pch="+",cex=3)
m.RAS
beta.ras

plot(x,y)
lines(unique(x),unique(x)*beta.rss,col=2)
lines(unique(x),unique(x)*beta.msr,col=3)
lines(unique(x),unique(x)*beta.ras,col=4)
legend(10,-2,c("OLS","MSR","RAS"),lty=c(1,1,1),col=c(2,3,4))


mm<-c(m.RSS,m.MSR,m.RAS)
beta.i<-c(beta.rss,beta.msr,beta.ras)
data.frame(mm,beta.i)
      

##########################################################################################
# Example:  tooth strength data                                                          #
#E & T: Chapter 14, Section 14.5                                                         #
##########################################################################################


library(bootstrap)
help(tooth)
tooth

pairs(tooth[,-c(1)])
fit.lm.D<-lm(strength~D1+D2,data=tooth)
summary(fit.lm.D)
fit.lm.E<-lm(strength~E1+E2,data=tooth)
summary(fit.lm.E)

par(mfrow=c(1,1))
plot(fit.lm.D$fit,fit.lm.E$fit)
abline(0,1)
plot(fit.lm.D$resid,fit.lm.E$resid)
abline(0,1)



n<-length(tooth$strength)
RSS.E<-sum((tooth$strength-fit.lm.E$fit)^2)
RSS.E
RSS.D<-sum((tooth$strength-fit.lm.D$fit)^2)
RSS.D
theta.o<-(RSS.E-RSS.D)/n
theta.o


B<-1000
RSS.E.b<-RSS.D.b<-theta<-c(1:B)
index<-c(1:13)
for(i in 1:B)
{
index.b<-sample(index,n,replace=TRUE)
tooth.b<-tooth[index.b,]
fit.lm.D.b<-lm(strength~D1+D2,data=tooth.b)
fit.lm.E.b<-lm(strength~E1+E2,data=tooth.b)
RSS.E.b[i]<-sum((tooth.b$strength-fit.lm.E.b$fit)^2)
RSS.D.b[i]<-sum((tooth.b$strength-fit.lm.D.b$fit)^2)
theta[i]<-(RSS.E.b[i]-RSS.D.b[i])/n
}
hist(theta,nclass=50)
ci<-quantile(theta,probs=c(0.025,0.975))
ci
lines(c(ci[1],ci[1]),c(0,500),col=2,lwd=3)
lines(c(ci[2],ci[2]),c(0,500),col=2,lwd=3)
lines(c(theta.o,theta.o),c(0,500),col=4,lwd=3)

sqrt(var(theta))
sum(theta<theta.o)
qqplot(RSS.E.b,RSS.D.b)
abline(0,1)
boxplot(RSS.E.b,RSS.D.b,names=c("RSE(E)","RSE(D)"))



fit.lm.R<-lm(strength~D1+D2,data=tooth)
fit.lm.F<-lm(strength~D1+D2+E1+E2,data=tooth)
anova(fit.lm.R,fit.lm.F)[2,4]
anova(fit.lm.R,fit.lm.F)[2,5]
1-pf(0.0762,2,8)


B<-1000
RSS.R.b<-RSS.F.b<-theta<-c(1:B)
index<-c(1:13)
for(i in 1:B)
{
index.b<-sample(index,n,replace=TRUE)
tooth.b<-tooth[index.b,]
fit.lm.R.b<-lm(strength~D1+D2,data=tooth.b)
fit.lm.F.b<-lm(strength~D1+D2E1+E2,data=tooth.b)
RSS.E.b[i]<-sum((tooth.b$strength-fit.lm.E.b$fit)^2)
RSS.D.b[i]<-sum((tooth.b$strength-fit.lm.D.b$fit)^2)
theta[i]<-(RSS.E.b[i]-RSS.D.b[i])/n
}
hist(theta,nclass=50)
ci<-quantile(theta,probs=c(0.025,0.975))
ci
lines(c(ci[1],ci[1]),c(0,500),col=2,lwd=3)
lines(c(ci[2],ci[2]),c(0,500),col=2,lwd=3)
lines(c(theta.o,theta.o),c(0,500),col=4,lwd=3)

sqrt(var(theta))
sum(theta<theta.o)
qqplot(RSS.E.b,RSS.D.b)
abline(0,1)
boxplot(RSS.E.b,RSS.D.b,names=c("RSE(E)","RSE(D)"))


##########################################################################################
#                                                                                        #
#                                                                                        #
# Generelazied linear models                                                             #
#                                                                                        #
#                                                                                        #
##########################################################################################



##########################################################################################
## dose-response experiment                                                             ##
##########################################################################################

beetle<-read.table("C:/projects/GLM/data4glm/beetle.txt", header = TRUE)
attach(beetle)
dim(beetle)

p.b<-killed/beetles
unkilled<-beetles-killed
Proportionkilled<-p.b
par(mfrow=c(1,1))
plot(Dose,Proportionkilled, main="Proportion of the killed beetles",ylim=c(0,1))


fit.beetles<-glm(cbind(killed,unkilled)~Dose,family=binomial(link = "logit"))
summary(fit.beetles)$coefficients
summary(fit.beetles)$coefficients[,3]
lines(Dose,fit.beetles$fit)


#################################
## parametrix bootstrap         #
#################################
attach(beetle)
p.b<-killed/beetles
B<-1000
prob.boot<-matrix(0,8,B)
test.stat<-coeff.boot<-matrix(0,2,B)
pos.boot<-c(1:8)
for(b in 1:B)
{
for(i in 1:8)
{
pos.boot[i]<-sum(rbinom(beetles[i],1,p.b[i]))
}
neg.boot<-beetles-pos.boot
fit.boot<-glm(cbind(pos.boot,neg.boot)~Dose,family=binomial(link = "logit"))
prob.boot[,b]<-fit.boot$fit
coeff.boot[,b]<-fit.boot$coefficients
test.stat[,b]<-summary(fit.boot)$coefficients[,3]
}


par(mfrow=c(1,1))
hist(coeff.boot[1,],main="alpha",nclass=25)
lines(c(fit.beetles$coefficients[1],fit.beetles$coefficients[1]),c(0,550),
      col=2,lwd=2)
quantile(coeff.boot[1,], probs = c(0.025, 0.975))
 
hist(coeff.boot[2,],main="beta",nclass=25)
lines(c(fit.beetles$coefficients[2],fit.beetles$coefficients[2]),c(0,300),
      col=2,lwd=2)
quantile(coeff.boot[2,], probs = c(0.025, 0.975))




par(mfrow=c(1,1))
plot(Dose,Proportionkilled, main="Proportion of the killed beetles",ylim=c(0,1))
lines(Dose,fit.beetles$fit)
for(b in 1:B)
{
lines(Dose,prob.boot[,b],col=3)
}
lines(Dose,fit.beetles$fit,col=1,lwd=3)
points(Dose,Proportionkilled)



##########################################################################################
## serological data for malaria                                                         ##
##########################################################################################

agei<-c(1.5,4.0,7.5,12.5,17.5,25.0,35.0,47.0,60)
posi<-c(8,6,18,14,20,39,19,25,44)
ntot<-c(123,132,182,140,138,161,133,92,74)
negi<-ntot-posi
plot(agei,posi/ntot)
cbind(agei,posi,negi)
fit.malaria1<-glm(cbind(posi,negi)~agei,family=binomial(link = "logit"))
summary(fit.malaria1)
summary(fit.malaria1)$coefficients
summary(fit.malaria1)$coefficients[,3]
lines(agei,fit.malaria1$fit)

#######################################
## Estimation                        ##
#######################################

p.b<-posi/ntot
B<-1000
prob.boot<-matrix(0,9,B)
test.stat<-coeff.boot<-matrix(0,2,B)
pos.boot<-c(1:9)
for(b in 1:B)
{
for(i in 1:9)
{
pos.boot[i]<-sum(rbinom(ntot[i],1,p.b[i]))
}
neg.boot<-ntot-pos.boot
fit.boot<-glm(cbind(pos.boot,neg.boot)~agei,family=binomial(link = "logit"))
prob.boot[,b]<-fit.boot$fit
coeff.boot[,b]<-fit.boot$coefficients
test.stat[,b]<-summary(fit.boot)$coefficients[,3]
}


par(mfrow=c(1,1))
hist(coeff.boot[1,],main="alpha",nclass=25)
lines(c(fit.beetles$coefficients[1],fit.beetles$coefficients[1]),c(0,550),
      col=2,lwd=2)
quantile(coeff.boot[1,], probs = c(0.025, 0.975))
 
hist(coeff.boot[2,],main="beta",nclass=25)
lines(c(fit.beetles$coefficients[2],fit.beetles$coefficients[2]),c(0,300),
      col=2,lwd=2)
quantile(coeff.boot[2,], probs = c(0.025, 0.975))




par(mfrow=c(1,1))
plot(agei,p.b, ylab="sero prevalence",ylim=c(0,1))
lines(agei,fit.malaria1$fit)
for(b in 1:B)
{
lines(agei,prob.boot[,b],col=3)
}
lines(agei,fit.malaria1$fit,col=1,lwd=3)
points(agei,p.b)





#######################################
## Bootstrap tests                   ##
#######################################



fit.malaria0<-glm(cbind(posi,negi)~1,family=binomial(link = "logit"))


summary(fit.malaria0)$coefficients
eta<- -1.62690 
prob.i<-exp(eta)/(1+exp(eta))
prob.i<-rep(prob.i,9)
prob.i
plot(agei,posi/ntot)
lines(agei,prob.i,col=2)




B<-1000
prob.boot<-matrix(0,9,B)
test.stat<-coeff.boot<-matrix(0,2,B)
pos.boot<-c(1:9)
for(b in 1:B)
{
for(i in 1:9)
{
pos.boot[i]<-sum(rbinom(ntot[i],1,prob.i[i]))
}
neg.boot<-ntot-pos.boot
fit.boot<-glm(cbind(pos.boot,neg.boot)~agei,family=binomial(link = "logit"))
prob.boot[,b]<-fit.boot$fit
coeff.boot[,b]<-fit.boot$coefficients
test.stat[,b]<-summary(fit.boot)$coefficients[,3]
}
par(mfrow=c(1,1))
hist(coeff.boot[1,],main="alpha",xlim=c(-3,-1))
lines(c(fit.malaria1$coefficients[1],fit.malaria1$coefficients[1]),c(0,150),
      col=2,lwd=2)

hist(coeff.boot[2,],main="beta",xlim=c(-0.025,0.05))
lines(c(fit.malaria1$coefficients[2],fit.malaria1$coefficients[2]),c(0,300),
      col=2,lwd=2)




par(mfrow=c(1,2))
hist(coeff.boot[2,],main="beta",xlim=c(-0.025,0.05))
lines(c(fit.malaria1$coefficients[2],fit.malaria1$coefficients[2]),c(0,300),
      col=2,lwd=2)
hist(test.stat[2,],main="t-beta",xlim=c(-10,10))
lines(c(9.903614,9.903614),c(0,300),
      col=2,lwd=2)


9.903614 


dim(prob.boot)
plot(agei,posi/ntot)
lines(agei,fit.malaria1$fit)
for(i in 1:B)
{
lines(agei,prob.boot[,i],lty=1,col=2)
}

hist(coeff.boot[2,])






fit.malaria<-glm(cbind(posi,negi)~agei,family=binomial(link = "logit"))
summary(fit.malaria)
eta<- -2.714074+0.044672*agei
prob.i<-exp(eta)/(1+exp(eta))
lines(agei,prob.i)



prob.i

B<-100
prob.boot<-matrix(0,9,B)
coeff.boot<-matrix(0,2,B)
pos.boot<-c(1:9)
for(b in 1:B)
{
for(i in 1:9)
{
pos.boot[i]<-sum(rbinom(ntot[i],1,prob.i[i]))
}
neg.boot<-ntot-pos.boot
fit.boot<-glm(cbind(pos.boot,neg.boot)~agei,family=binomial(link = "logit"))
prob.boot[,b]<-fit.boot$fit
coeff.boot[,b]<-fit.boot$coefficients
}

hist(coeff.boot[1,],nclass=25)
hist(coeff.boot[2,],nclass=25)



plot(agei,posi/ntot)
cbind(agei,posi,negi)


fit.malaria<-glm(cbind(posi,negi)~agei,family=binomial(link = "logit"))
summary(fit.malaria)
eta<- -2.714074+0.044672*agei
prob.i<-exp(eta)/(1+exp(eta))
lines(agei,prob.i)
for(b in 1:B)
{
lines(agei,prob.boot[,b])
}

CI.b<-matrix(0,9,2)
for(i in 1:9)
{
CI.b[i,]<-quantile(prob.boot[i,],probs=c(0.025,0.975))
}

plot(agei,posi/ntot)
cbind(agei,posi,negi)
lines(agei,prob.i)
lines(agei,CI.b[,1])
lines(agei,CI.b[,2])


###################################################
# parametric bootstrap                            #
###################################################

resp<-as.factor(c(rep(0,21),rep(1,2),rep(0,19),rep(1,13)))
trti<-as.factor(c(rep(1,21),rep(1,2),rep(2,19),rep(2,13)))
table(trti,resp)
dim(cbind(resp,trti))

fit.mice<-glm(resp~trti,family=binomial(link = "logit"))
summary(fit.mice)
exp(1.9719)

library(MASS)
fit.mice<-glm(resp~trti,family=binomial(link = "logit"))
confint(fit.mice, level=0.95)

exp(confint(fit.mice, level=0.95))

B<-1000
index<-c(1:55)
or.b<-c(1:B)

for(b in 1:B)
{
index.b<-sample(index,size=55,replace=T)
resp.b<-resp[index.b]
trti.b<-trti[index.b]
fit.mice.b<-glm(resp.b~trti.b,family=binomial(link = "logit"))
summary(fit.mice.b)
or.b[b]<-fit.mice.b$coefficients[2]
}

hist(or.b,nclass=50)
quantile(or.b)


resp.1<-as.factor(c(rep(0,21),rep(1,2)))
trti.1<-as.factor(c(rep(1,21),rep(1,2)))
n1<-length(resp.1)
index1<-c(1:n1)

resp.2<-as.factor(c(rep(0,19),rep(1,13)))
trti.2<-as.factor(c(rep(2,19),rep(2,13)))
n2<-length(resp.2)
index2<-c(1:n2)


B<-1000
or.b<-c(1:B)

for(b in 1:B)
{
index.b.1<-sample(index1,size=n1,replace=T)
index.b.2<-sample(index2,size=n2,replace=T)
resp.b1<-resp.1[index.b.1]
#trti.b1<-trti.1[index.b.1]
resp.b2<-resp.2[index.b.2]
#trti.b2<-trti.2[index.b.2]
resp.b<-as.numeric(c(resp.b1,resp.b2))-1
#trti.b<-as.factor(c(trti.b1,trti.b2))
trti.b<-as.factor(c(rep(1,n1),rep(2,n2)))
fit.mice.b<-glm(resp.b~trti.b,family=binomial(link = "logit"))
summary(fit.mice.b)
or.b[b]<-fit.mice.b$coefficients[2]
}

hist(or.b,nclass=50)
quantile(or.b)







