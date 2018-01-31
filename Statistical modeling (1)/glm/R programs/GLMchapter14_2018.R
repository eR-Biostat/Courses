################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 14                                                   #
# Over dispersion                                              #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################


################################################################
##Example 1: Germination of seeds (Binomial)                   #
################################################################

fac<-read.table("C:/projects/GLM/data4glm/seed.txt", header = TRUE)
fact <- transform(fac, prop = germ/total, extr.seed = interaction(extract,seed))
fact

plot(prop ~ extr.seed, data = fact, las = 1, ylab = "proportion")
points(prop ~ extr.seed, data = fact, pch = 16)
title("proportion of germinating seeds")

g <- glm(cbind(germ, total - germ) ~ extract + seed + extract:seed, family = binomial, data = fact)
r.pears<-residuals(g, type="pearson")
summary(g)
summary(r.pears)

############### Overdipersion parameter #######################
X2 <- sum(residuals(g, type = "pearson")^2)
phi <- X2/g$df.residual
phi <- g$deviance/g$df.residual
g <- glm(cbind(germ, total - germ) ~ extract + seed + extract:seed, family = binomial, data = fact)
g.over <- glm(cbind(germ, total - germ) ~ extract + seed + extract:seed, family = quasibinomial, data = fact)
summary(g.over)
summary(g.over)$dispersion

######### confidence Interval       ###########################
library(MASS)
fact$prop <- with(fact, germ/total)
g <- glm(cbind(germ, total - germ) ~ extract + seed + extract:seed, family = binomial, data = fact)
g.over.alt <- glm(prop ~ extract + seed + extract:seed,weights = total, family = quasibinomial, data = fact)

confint(g)
confint(g.over.alt)

library(doBy)
library(R2HTML)
logit.over <- esticon(g.over, c(1, 0, 1, 0))[c(2, 7, 8)]
probability <- exp(logit.over)/(1 + exp(logit.over))
logit.g <- esticon(g, c(1, 0, 1, 0))[c(2, 7, 8)]
probability <- exp(logit.g)/(1 + exp(logit.g))

################### testing of overdispersion    ###############

p.val <- pchisq(summary(g.over)$dispersion * g$df.residual,
g$df.residual, lower = F)



################################################################
# example 2 binary example from McCulagh & Nelder              #  
# habitat data                                                 #
################################################################

habitat <- read.table("C:/projects/GLM/data4glm/habitat.txt")
dim(habitat)
names(habitat)<-c("G","Total","S","D","H","T")
habitat
f1<-glm((G/Total)~H+D+S+T,family="binomial",data=habitat)
summary(f1)


f1.over <- glm((G/Total)~H+D+S+T,family=quasibinomial,data= habitat)
summary(f1.over)
summary(f1.over)$dispersion




################################################################
#######Example 3: Ship Damage loglinear                  #######
################################################################


library(MASS)
data(ships)
ships2 <- subset(ships, service > 0)
ships2$year <- as.factor(ships2$year)
ships2$period <- as.factor(ships2$period)

glm0 <- glm(formula = incidents ~ type + year + period,
    family = poisson(link = "log"), data = ships2,
    offset = log(service))
summary(glm0)

glm1 <- update(glm1, family = quasipoisson(link = "log"))
summary(glm1)

#############Over dispresion estimation             ###########
                 
X2 <- sum(residuals(glm1, type = "pearson")^2)
phi <- X2/glm1$df.residual
phi <- g$deviance/glm1$df.residual
anova(glm0, test = "Chisq")
anova(glm1, test = "F")



################################################################
##Exatra Example (not in the slides)                           #
################################################################


mobility <- data.frame(
number = c(50,45,8,18,8,28,174,84,154,55,11,78,110,
223,96,14,150,185,714,447,3,42,72,320,411),
father = factor(rep(1:5,rep(5,5))),
son = factor(rep(1:5,5)),
diff = factor(abs(rep(1:5,rep(5,5))-rep(1:5,5))),
up = factor(rep(1:5,rep(5,5))>rep(1:5,5)))

mobility.glm <- glm(number~father+son+diff+up, family=poisson, data=mobility)
summary(mobility.glm)
tapply(round(resid(mobility.glm,type="dev"),1), list(mobility$father,mobility$son), function(x) x)

mobility.glm.over <- glm(number~father+son+diff+up, family=quasipoisson, data=mobility)
summary(mobility.glm.over)


X2 <- sum(residuals(mobility.glm.over, type = "pearson")^2)
phi <- X2/mobility.glm.over$df.residual
phi <- mobility.glm.over$deviance/mobility.glm.over$df.residual

######### confidence Interval                      ###########
library(MASS)
confint(mobility.glm.over)

library(doBy)
library(R2HTML)
log.over <- esticon(mobility.glm.over, c(1, 0, 1, 0))[c(2, 7, 8)]
probability <- exp(log.over)
log.g <- esticon(mobility.glm, c(1, 0, 1, 0))[c(2, 7, 8)]
probability <- exp(log.g)



#########################################################
# END OF CHAPTER 14                                     #
#########################################################

