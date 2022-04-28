#########################################################
#                                                       #
#                                                       #
#        Linear regression in R                         #
#                                                       #
#                                                       #
#########################################################

#########################################################
## The cars data                                        #
#########################################################

data(cars)
head(cars)

#########################################################
## ggplot                                               #
#########################################################

library(ggplot2)
theme_set(theme_bw())



pl <- ggplot(cars) + geom_point(aes(x=speed, y=dist), size=2, colour="#993399") + 
  xlab("Speed (mph)") + ylab("Stopping distance (ft)")  
print(pl)
attach(cars)
lm0 <- lm(dist ~ 1)
lm0
summary(lm0)

pl + geom_hline(yintercept=coef(lm0)[1],size=1, colour="#339900")
lm1 <- lm(dist ~ speed, data=cars)
coef(lm1)
summary(lm1)
confint(lm1)
pl  + geom_abline(intercept=coef(lm1)[1],slope=coef(lm1)[2],size=1, colour="#339900")
par(mfrow = c(1, 2))
plot(lm1, which=c(1,2))

set.seed(100)
n <- nrow(cars)
i.training <- sort(sample(n,round(n*0.8)))
cars.training <- cars[i.training,]
cars.test  <- cars[-i.training,]
pred1a.test <- predict(lm1, newdata=cars.test)


lm1.training <- lm(dist ~ speed, data=cars.training)
pred1b.test <- predict(lm1.training, newdata=cars.test)
data.frame(cars.test, pred1a.test, pred1b.test)
dist.test <- cars.test$dist
par(mfrow=c(1,2))
plot(pred1b.test, dist.test)
abline(a=0, b=1, lty=2)
plot(pred1b.test, pred1a.test)
abline(a=0, b=1, lty=2)


cor.test <- cor(pred1a.test, dist.test)
R2.test <- cor.test^2
R2.test

i.training <- c(6:45)
cars.training <- cars[i.training,]
cars.test  <- cars[-i.training,]
dist.test <- cars.test$dist
pred1a.test <- predict(lm1, newdata=cars.test)
lm1.training <- lm(dist ~ speed, data=cars.training)
pred1b.test <- predict(lm1.training, newdata=cars.test)
par(mfrow=c(1,2))
plot(pred1b.test, dist.test)
abline(a=0, b=1, lty=2)
plot(pred1b.test, pred1a.test)
abline(a=0, b=1, lty=2)


alpha <- 0.05
df.new <- data.frame(speed=(6:23))
conf.dist <- predict(lm1, newdata = df.new, interval="confidence", level=1-alpha) 
head(conf.dist)

pred.dist <- predict(lm1, newdata = df.new, interval="prediction", level=1-alpha) 
head(pred.dist)

df.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
df.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]
pl +   
  geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=df.new, aes(x=speed, y=fit), colour="#339900", size=1)

lm2 <- lm(dist ~  speed + I(speed^2))
summary(lm2)

X <- model.matrix(lm2)
S <- solve(t(X)%*%X)
d <- sqrt(diag(S))
R <- S/(d%*%t(d))
R

f <- function(x,c) coef(lm2)[1] + coef(lm2)[2]*x + coef(lm2)[3]*x^2
#f <- function(x,c) coef(lm2)[1]*x + coef(lm2)[2]*x^2
pl  + geom_abline(intercept=coef(lm1)[1],slope=coef(lm1)[2],size=1, colour="#339900") +
  stat_function(fun=f, colour="red", size=1)

lm2t <- lm(dist ~ speed + I(speed^2), data=cars.training)
f <- function(x,c) coef(lm2t)[1] + coef(lm2t)[2]*x + coef(lm2t)[3]*x^2
pl + stat_function(fun=f, colour="#339900", size=1) + geom_point(data=cars.test, aes(x=speed, y=dist), color="blue")

lm2.noint <- lm(dist ~ -1 + speed + I(speed^2))
coef(lm2.noint)
X <- model.matrix(lm2.noint)
head(X)


df.new <- data.frame(speed=(0:27))
conf.dist <- predict(lm2.noint, newdata = df.new, interval="confidence", level=1-alpha) 
pred.dist <- predict(lm2.noint, newdata = df.new, interval="prediction", level=1-alpha) 
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
df.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]
pl +   
  geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=df.new, aes(x=speed, y=fit), colour="#339900", size=1)


lm2.poly <- lm(dist ~ poly(speed, degree=2, raw=T))
M <- model.matrix(lm2.poly)
head(M)
print(coef(lm2.poly))


lm1.ortho <- lm(dist ~ poly(speed, degree=1))
M1 <- model.matrix(lm1.ortho)
head(M1)

coef(lm1.ortho)

lm2.ortho <- lm(dist ~ poly(speed, degree=2))
M2 <- model.matrix(lm2.ortho)
head(M2)

t(M2)%*%M2

summary(lm2.ortho)


summary(lm1.ortho)$coefficients


anova(lm0, lm1)

anova(lm1, lm2)


logLik(lm0)
logLik(lm1)
logLik(lm2)

dl <- 2*as.numeric(logLik(lm1) - logLik(lm0))
1-pchisq(dl,1)

dl <- 2*as.numeric(logLik(lm2) - logLik(lm1))
1-pchisq(dl,1)

AIC(lm0,lm1,lm2)
BIC(lm0,lm1,lm2)


library(gridExtra)
pl1 <- pl + scale_x_log10() 
pl2 <- pl + scale_y_log10() 
grid.arrange(pl1, pl2, nrow=1)

lm1.log <- lm(log(dist) ~ log(speed))
summary(lm1.log)

par(mfrow=c(1,2))
plot(log(speed),log(dist))
abline(a=coef(lm1.log)[1], b=coef(lm1.log)[2], col="#339900")
plot(lm1.log, which=1)


logLik(lm1.log)
logLik(lm1.log) - sum(log(dist))
AIC(lm1.log) - 2*sum(log(1/dist))
BIC(lm1.log) - 2*sum(log(1/dist))