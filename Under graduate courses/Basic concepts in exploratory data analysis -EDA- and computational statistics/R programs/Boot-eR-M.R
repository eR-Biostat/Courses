#######################################################
#######################################################
## Basic Concept of bootstrap                        ##
## >eR-Biostat                                       ##
## 2017                                              ##
## Dinberu Seyoum and Ziv Shkedy and Martin Otava    ##
#######################################################

#######################################################
##### chapter 1: Measure of location      #############
#######################################################


### Empirical distribution   ###
par(mfrow=c(1,1))
x<-seq(from=-3,to=3,length=1000)
dx<-dnorm(x,0,1)
plot(x,dx,type="l")

x1<-rnorm(10000,0,1)
hist(x1,nclass=50,col=0,probability=T)

par(mfrow=c(1,2))
x2<-rnorm(1000,0,1)
x2<-sort(x2)
px<-pnorm(x2,0,1)
plot(x2,px,type="l")
n<-length(x2)
px.e<-c(1:length(x2))/n
plot(x2,px.e,type="s")

### Sample mean from normal   ###

par(mfrow=c(1,1))
x2<-rnorm(50,0,1)
x2<-sort(x2)
x<-seq(from=-3,to=3,length=1000)
px<-pnorm(x,0,1)
plot(x,px,type="l")
n<-length(x2)
px.e<-c(1:length(x2))/n
lines(x2,px.e,type="s")

### 1000 samples of size 10 from N(0,1) ##

nsim<-1000
mx<-c(1:nsim)
for(i in 1:nsim){
x<-rnorm(10,0,1)
mx[i]<-mean(x)
}
hist(mx,nclass=20,col=1)

### 1000 samples of size 500 from B(1,0.75) ##


nsim<-1000
mx<-c(1:nsim)
for(i in 1:nsim){
x<-rbinom(500,1,0.75)
mx[i]<-mean(x)
}

hist(mx,nclass=20,col=1)

#######################################################
## Bootstrap estimate for standard error of          ##  
## the sample mean                                   ##
#######################################################

x <- c(11.201, 10.035, 11.118, 9.055, 9.434, 9.663, 10.403, 11.662, 9.285,8.84)
mean(x)
var(x)

###  Non parametric Bootstrap with B=1000     ###
set.seed(1234)
n<-length(x)
B<-1000
mx<-c(1:B)
for(i in 1:B){
cat(i)
boot.i<-sample(x,n,replace=T)
mx[i]<-mean(boot.i)
}
var(mx)

###  Parametric Bootstrap with B=1000        ###

set.seed(1234)
B<-1000
MLx<-mean(x)
Varx<-var(x)
mx<-c(1:B)
for(i in 1:B){
cat(i)
boot.i<-rnorm(n, MLx, sqrt(Varx))
mx[i]<-mean(boot.i)
}
var(mx)

#######################################################
## Bootstrap for the correlation                     ##
#######################################################

x <- c(29, 435, 86, 1090, 219, 503, 47, 3524, 185, 98, 952, 89)
y <- c(127, 214, 133, 208, 153, 184, 130, 217, 141, 154, 194, 103)

cor(x, y)
plot(y~x)

### correlation for given x and y data             ##

cor(x[x < 2000], y[x < 2000])
plot(x[x < 2000], y[x < 2000])


x<-c(29,435,86,1090,219,503,47,3524,185,98,952,89)
y<-c(127,214,133,208,153,184,130,217,141,154,194,103)
cbind(x,y)
plot(x,y)
cor.obs<-cor(x,y)
n<-length(x)
index<-c(1:n)
B<-1000
obs.8<-cor.xy<-c(1:B)
for(i in 1:B){
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

#######################################################
## Estimating bais with Bootstrap                    ##
#######################################################

#######################################################
## The patch data                                    ##
#######################################################

control <- c(9243, 9671, 11792, 13357, 9055, 6290, 12412, 18806)
oldpatch <- c(17649 , 12013 , 19979 , 21816 , 13850 , 9806 , 17208 , 29044 )
newpatch <- c(16449, 14614, 17274, 23798, 12560, 10157, 16570, 26325)

y <- newpatch-oldpatch
z <- oldpatch-control

mean(y)
mean(z)
theta.obs <- mean(y)/mean(z)
theta.obs

##### Bootstrap replicates                        ####

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

################# Quantiles
quantile(theta.boot, probs = c(0.025, 0.975))

