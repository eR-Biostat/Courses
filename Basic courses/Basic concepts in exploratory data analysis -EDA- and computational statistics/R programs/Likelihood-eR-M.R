#######################################################
#######################################################
## Likelihood Function and Maximum                   ## 
## Likelihood estimators                             ##
## >eR-Biostat                                       ##
## 2017                                              ##
## Dinberu Seyoum and Ziv Shkedy and Martin Otava    ##
#######################################################
#######################################################


#######################################################
### Bernoulli likelihood                             ##
#######################################################

par(mfrow=c(2,2))
N <- 10
p <- c(0.1,0.25,0.5,0.8)
x <- 0:10
for(i in 1:4){
x1 <- dbinom(x,N,p[i])
plot(x1, type="h", xlab="x", ylab=expression(P(x)),
ylim=c(0,0.4), lwd=2, col="red")
j <- p[i]
title(main = substitute(p == j,list(j=j)))
}

###   Likelihood                                  #####
p <- seq(0,1,0.01)
d <- length(p)
N <- 10
x <- 4
j <- m <- seq(1:d)
for(i in 1:d){
j[i] <- p[i]^x*(1-p[i])^(N-x)
}
plot(p, j, type="l", col=4, ylab="likelihood L(p)", main="likelihood function of p")

### Likelihood and log-likelihood               ######  
p <- seq(0,1,0.01)
d <- length(p)
N <- 10
x <- 4
j <- log.lik <- m <- seq(1:d)
for (i in 1:d){
log.lik[i] <- x*log(p[i])+(N-x)*log(1-p[i])
j[i] <- p[i]^x*(1-p[i])^(N-x)
}
par(mfrow=c(1,2))
plot(p, j, type="l",col=4, ylab="likelihood L(p)", main="likelihood function of p")
plot(p, log.lik, type="l", col=3, ylab="Log-likelihood L(p)", main="Log-likelihood function of p")

#####or
log.lik[i] <- x*log(p[i])+(N-x)*log(1-p[i])
#####or
dbinom(x, prob=p, size=N, log=TRUE)
##### Negative log
- dbinom(x, prob=p, size=N, log=TRUE)


mle.p <- x/N
var.p <- mle.p*(1-mle.p)/N
var.p

#######################################################
### Maximization Bernoulli likelihood                ##
#######################################################


maxim <- function (p,x,N){
p^x*(1-p)^(N-x)
}
pmax <- optimize(maxim, c(0, 1), tol = 0.000001, N = 10, x=4, maximum=TRUE)
pmax

#### checking
mle.p <- 4/10



#######################################################
##  Poisson distribution                             ##
#######################################################

par(mfrow=c(2,2))
x <- seq(0,50,1)
lambda <- c(5,15,25,35)
t <- length(lambda)
for (i in 1:4){
prob <- dpois(x, lambda[i], log = FALSE)
plot(prob, type="h", xlab="x",
ylab = expression(P(x)),
ylim=c(0,.2),lwd=2, col="red")
j <- lambda[i]
title(main = substitute(lambda == j, list(j=j)))
}

### likelihood                                  ######

lambda <- seq(0,5,0.01)
like <- ((exp(-10*lambda))*(lambda^20))/207360
plot(lambda, like, type="l", col=2, lwd=2, main="Poisson Likelihood", xlab=expression(lambda), ylab="Likelihood")

### likelihood                                  ######

lambda2 <- seq(1.75,2.25,0.001)
like2 <- (exp(-10*lambda2)*lambda2^20)/207360
plot(lambda2, like2, type="l", col=2, lwd=2, main="Poisson Likelihood", xlab=expression(lambda), ylab="Likelihood")

### MLE estimator                              ###### 
x <- c(5,0,1,1,0,3,2,3,4,1)
mle.la <- sum(x)/length(x)
mle.la

###  Likelihood                               #######
lam <- seq(1,5,0.1)
t <- length(lam)
x <- c(5,0,1,1,0,3,2,3,4,1)
su <- sum(x)
me <- mean(x)
pro <- c(1:t)
for (i in 1:t){
pro[i]<-(lam[i]^su)*(exp(-length(x)*lam[i]))
}
plot(lam, pro, type="l", col=4, ylab="likelihood", main="likelihood function")

#### Example 2
x2 <- c(2, 2, 0, 1, 3, 6, 4, 4, 4, 1)
mle.la2 <- sum(x2)/length(x2)
mle.la2

#######################################################
### Sampling distribution of the sample mean        ### 
#######################################################

poi.data <- c(5,0,1,1,0,3,2,3,4,1)
lambda <- mean(poi.data)
t <- 5000
x <- b.mean <- seq(1:t)
for (i in 1:t){
b.sample <- rpois(10,lambda)
b.mean[i] <- mean(b.sample)
}
lambda
mean(b.mean)
sd(b.mean)

hist(b.mean, freq = FALSE, col = "grey", xlab="MLE of lambda", main="sampling distribution of MLE of lambda")
box()
curve(dnorm(x, mean=mean(b.mean), sd=sd(b.mean)), col='red', add=TRUE)

###   MLE                                  ####
     
y <- c(4,1,4,3,0,2,3,1,0,1)
mean(y)

#######################################################
### Maximization Poisson likelihood                  ##
#######################################################

poisson.lik <- function(mu,y){
y <- c(4,1,4,3,0,2,3,1,0,1)
N <- length(y)
b <- sum(y)
logl <- b*log(mu)-N*mu
}
xmax <- optimize(poisson.lik, c(0, 5), tol = 0.000001, maximum=TRUE)
xmax


#######################################################
###  Normal distribution                             ##
#######################################################

y <- c(1.364, 0.235, -0.846, -0.285, -1.646)

normal.lik1 <- function(mu,sigma2,y){
N <- length(y)
t <- length(mu)
logl <- array(1,t)
for (i in 1:t){
logl[i] <- -0.5*N*log(2*pi)-0.5*N*log(sigma2)-(1/(2*sigma2))*sum((y-mu[i])**2)}
return(logl)
}
y <- c(1.364, 0.235, -0.846, -0.285, -1.646)
mu <- seq(-2, 2,0.1)
sigma2 <- 1
logl <- normal.lik1(mu,sigma2,y)
plot(mu, logl, type="l",col=6, ylab="likelihood L(p)", main="log-likelihood function of mean")

mean(y)

## MLE                                 ###
      

y2 <- c(0.9898790, 1.4552127, -1.5438397, -2.3792939,-0.3809298)
mean(y2)
mean((y2-mean(y2))^2)

g <- function(mu){
sigma2 <- 1
y2 <- c(0.9898790,1.4552127,-1.5438397,-2.3792939,-0.3809298)
N <- length(y2)
t <- y2-mu
h <- t**2
logl <- -0.5*N*log(2*pi)-0.5*N*log(sigma2)-(1/(2*sigma2))*sum(h)
}
xmax <- optimize(g, c(-2, 2), maximum=TRUE)
xmax

#######################################################
### Maximization Normal likelihood                   ##
#######################################################

g2 <- function(theta){
  y2 <- c(0.9898790,1.4552127,-1.5438397,-2.3792939,-0.3809298)
  mu <- theta[1]
  sigma2 <- theta[2]
  N <- length(y2)
  logl<- -0.5*N*log(2*pi)-0.5*N*log(sigma2)-(1/(2*sigma2))*sum((y2-mu)^2)
}
max.theta <- optim(c(0,1), g2, method="Nelder-Mead", control = list(fnscale = -1))
max.theta


library(rgl)

x <- rep(seq(-2,2,by=0.01),401)
y <- rep(seq(0.01,4.01,by=0.01),each=401)
theta <- data.frame(x,y)
z <- apply(theta, MARGIN=1, g2)
plot3d(x,y,z,col="red")

