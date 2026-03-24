library(SemiPar)

data(lidar)

plot(x=lidar$range, lidar$logratio, pch=16, cex.lab=1.25, col="lightgray", xlab="range", ylab="log ratio")

#MODEL WITH 48 KNOTS
sj <- seq(min(lidar$range),max(lidar$range),length=48) 
X <- tf.X(lidar$range,sj) ## get model matrix
b <- lm(lidar$logratio ~ X - 1) ## fit model

s <- seq(min(lidar$range),max(lidar$range), length=200)
Xp <- tf.X(s,sj) ## prediction matrix
lines(s,Xp %*% coef(b), lwd=2, col=2) #unpenalized

loglambda = seq(-9,11,length=1000)
n <- length(lidar$logratio)
V <- rep(NA,1000)

for (i in 1:1000) { ## loop through smoothing params
  b2 <- prs.fit(lidar$logratio, lidar$range, sj, exp(loglambda[i])) ## fit model
  trF <- sum(influence(b2)$hat[1:n]) ## extract EDF
  rss <- sum((lidar$logratio-fitted(b2)[1:n])^2) ## residual SS
  V[i] <- n*rss/(n-trF)^2 ## GCV score
}

logsp <- loglambda[which.min(V)]
logsp

b.pen <- prs.fit(lidar$logratio, lidar$range, sj, exp(logsp)) ## penalized regression fit
lines(s, Xp %*% coef(b.pen), col=3, lwd=2) 
