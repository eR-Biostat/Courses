library(SemiPar)

source("C:\\Users\\P073787\\Documents\\Teaching\\Zuid-Afrika\\gamCourse_functions.R")

data(fossil)

plot(fossil$age, fossil$strontium.ratio, xlab="Age", ylab="Strontium Ratio", cex=1.25, pch=16, col="lightgray")

#MODEL WITH 4 KNOTS
sj <- seq(min(fossil$age),max(fossil$age),length=4) 
X <- tf.X(fossil$age,sj) ## get model matrix
b <- lm(fossil$strontium.ratio ~ X - 1) ## fit model

s <- seq(min(fossil$age),max(fossil$age), length=200)
Xp <- tf.X(s,sj) ## prediction matrix
lines(s,Xp %*% coef(b), lwd=2, col=1) 

loglambda = seq(-9,11,length=1000)
n <- length(fossil$strontium.ratio)
V <- rep(NA,1000)

for (i in 1:1000) { ## loop through smoothing params
  b <- prs.fit(fossil$strontium.ratio, fossil$age, sj, exp(loglambda[i])) ## fit model
  trF <- sum(influence(b)$hat[1:n]) ## extract EDF
  rss <- sum((fossil$strontium.ratio-fitted(b)[1:n])^2) ## residual SS
  V[i] <- n*rss/(n-trF)^2 ## GCV score
}

logsp <- loglambda[which.min(V)]
logsp

b.pen <- prs.fit(fossil$strontium.ratio, fossil$age, sj, exp(logsp)) ## penalized regression fit
lines(s, Xp %*% coef(b.pen), col=2, lwd=2) 



#MODEL WITH 24 KNOTS
sj2 <- seq(min(fossil$age),max(fossil$age),length=24) 
X2 <- tf.X(fossil$age,sj2) ## get model matrix
b2 <- lm(fossil$strontium.ratio ~ X2 - 1) ## fit model

Xp2 <- tf.X(s,sj2) ## prediction matrix
lines(s,Xp2 %*% coef(b2), lwd=2, col=3) 

loglambda = seq(-9,11,length=1000)
n <- length(fossil$strontium.ratio)
V <- rep(NA,1000)

for (i in 1:1000) { ## loop through smoothing params
  b2 <- prs.fit(fossil$strontium.ratio, fossil$age, sj2, exp(loglambda[i])) ## fit model
  trF <- sum(influence(b2)$hat[1:n]) ## extract EDF
  rss <- sum((fossil$strontium.ratio-fitted(b2)[1:n])^2) ## residual SS
  V[i] <- n*rss/(n-trF)^2 ## GCV score
}

logsp2 <- loglambda[which.min(V)]
logsp2

b.pen2 <- prs.fit(fossil$strontium.ratio, fossil$age, sj2, exp(logsp2)) ## penalized regression fit
lines(s, Xp2 %*% coef(b.pen2), col=4, lwd=2) 
