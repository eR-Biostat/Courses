library(SemiPar)
library(nlme)

data(fossil)

plot(fossil$age, fossil$strontium.ratio, xlab="Age", ylab="Strontium Ratio", cex=1.25, pch=16, col="lightgray")

#linear mixed effects model
nbKnots <- 24
sD <- seq(min(fossil$age), max(fossil$age), length=200) ## prediction data
sj2 <- seq(min(fossil$age), max(fossil$age), length=nbKnots) ## generate knots
X0 <- tf.X(fossil$age,sj2) ## X in original parametr.
Xp.orig <- tf.X(sD,sj2) ## Xpred in original parametr.
D <- rbind(0,0,diff(diag(nbKnots),difference=2))#sqrt penalty
diag(D) <- 1
X <- t(backsolve(t(D),t(X0))) ## re-parameterized X
Z <- X[,-c(1,2)]; X <- X[,1:2] ## mixed model matrices
Xp1 <- t(backsolve(t(D),t(Xp.orig))) ## re-parametrized Xpred
g <- factor(rep(1,nrow(X))) ## dummy factor
m <- lme(strontium.ratio ~ X - 1, random=list(g = pdIdent(~ Z-1)), data=fossil) #fit model
lines(sD,Xp1 %*% as.numeric(coef(m)), lwd=2, col=1)
