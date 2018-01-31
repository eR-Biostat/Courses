################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 13                                                   #
# Poisson Regression and log linear models                     #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################



################################################
## Exatra example (not in the course)         ##
################################################


Survey.1 <- read.table("C:/projects/GLM/data4glm/agresti.txt", header=T)
attach(Survey.1)
Survey.1

M1<-glm(y~1, family=poisson, data=Survey.1)
M2<-glm(y~race+belief , family=poisson, data=Survey.1)
M3<-glm(y~race+belief+race:belief,family=poisson, data=Survey.1)
summary(M2)


AIC(M1)
AIC(M2)
AIC(M3)

anova.glm(M2,M3,test="Chisq")




################################################
## example 1:melanoma                         ##
################################################



melanoma <- read.table("C:/projects/GLM/data4glm/melanoma.txt", header=T)
attach(melanoma)
melanoma

M1<-glm(y~1, family=poisson, data=melanoma)
M2<-glm(y~Tumor+Type, family=poisson, data=melanoma)
M3<-glm(y~Tumor+Type+Tumor:Type,family=poisson, data=melanoma)


AIC(M1)
AIC(M2)
AIC(M3)

anova.glm(M2,M3,test="Chisq")




summary(M2)



################################################
## example 2:death penality                   ##
################################################


deathpenalty <- data.frame(
number = c(53,11,0,4,414,37,16,139),
victim = c("W","W","B","B","W","W","B","B"),
defendant = c("W","B","W","B","W","B","W","B"),
death = rep(c("yes","no"),rep(4,2)))

plot(xtabs(number~ victim + defendant+ death, deathpenalty))

M1<-glm(number~victim*defendant*death, family=poisson, data=deathpenalty)
summary(M1)
step(M1)



M2<-glm(number~victim+defendant+death
        +victim:defendant
        +victim:death
        +defendant:death,
        family=poisson, data=deathpenalty)
summary(M2)



################################################
## example 3:Antibiotic Prescription          ##
################################################

diag <- rep(c("bron", "sinus", "URI", "pneu"), 5)
time <- rep(c("win96", "spr96", "sum96", "aut96", "spr97"), rep(4, 5))
count = c(113, 99, 410, 60, 58, 37, 228, 43, 40, 23, 125, 30,108, 50, 366, 56, 100, 32, 304, 45)

rt <- data.frame(diag = factor(diag, unique(diag)),
                 time = factor(time, unique(time)),
                 count = c(113, 99, 410, 60, 58, 37, 228, 43, 40, 23, 125, 30,
                 108, 50, 366, 56, 100, 32, 304, 45))
#plot(xtabs(count~ time + diag, rt))
#plot(xtabs(fitted(ind)~ time + diag, rt))
#round(t(xtabs(residuals(ind)~ time + diag, rt)), 1)

M1 <- glm(count ~ 1, family=poisson, data=rt)
M2 <- glm(count ~ diag+time, family=poisson, data=rt)
M3 <- glm(count ~  diag+time+diag:time,family=poisson, data=rt)


AIC(M1)
AIC(M2)
AIC(M3)

anova.glm(M2,M3,test="Chisq")

summary(M3)

step(M3)

summary(M3)



#bestmodel3<-data.frame(diag, time,count)
#bestglm(bestmodel3,family = poisson, IC="AIC")


#########################################################
# END OF CHAPTER 13                                     #
#########################################################

