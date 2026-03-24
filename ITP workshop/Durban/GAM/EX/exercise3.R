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
lines(s,Xp %*% coef(b), lwd=2, col=2) 

#MODEL WITH 24 KNOTS
sj2 <- seq(min(fossil$age),max(fossil$age),length=24) 
X2 <- tf.X(fossil$age,sj2) ## get model matrix
b2 <- lm(fossil$strontium.ratio ~ X2 - 1) ## fit model

Xp2 <- tf.X(s,sj2) ## prediction matrix
lines(s,Xp2 %*% coef(b2), lwd=2, col=3) 
