################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 11                                                   #
# Poisson Regression                                           #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################


#########################################
# EXAMPLE 1:                            #
#########################################



stress <- read.table("C:/projects/GLM/data4glm/stress.txt", sep=",",header=TRUE)
attach(stress)
names(stress) 
stress



respGLM <- glm(respondents ~ month, family=poisson, data=stress)
summary(respGLM)

plot(respondents ~ month, xlab = "Months",ylab = "Subjects", xlim=c(0,20), ylim=c(0,20))
lines(month,respGLM$fit)

#########################################
# EXAMPLE 2:                            #
#########################################

Resignations <- read.table("C:/projects/GLM/data4glm/resign.txt", header=T)
attach(Resignations)
plot(Res ~ log(years), pch=19, col=c(4,2)[Gov]) # Use palette() to find out which colour corresponds
title("Ministerial Resignations against log(years)")
legend(locator(1), legend= c("conservative", "labour"), col=c(4,2), pch=19)

first0.glm <- glm(Res ~ Gov, poisson); 
summary(first0.glm)

first.glm <- glm(Res ~ log(years)+Gov+  Gov:log(years), poisson); summary(first.glm)

next.glm<- glm(Res ~ Gov + offset(log(years)), poisson); summary(next.glm)

last.glm <- glm(Res ~log(years),poisson); summary(last.glm)


plot(Res ~ log(years), pch=19, col=c(4,2)[Gov]) # Use palette() to find out which colour corresponds
title("Ministerial Resignations against log(years)")
legend(locator(1), legend= c("conservative", "labour"), col=c(4,2), pch=19)

l <- (0:25)/10
fv <- exp(0.3168 + 0.9654*l)
lines(l,fv)

################  Likelihood ratio test  ##############

first.glm1 <- glm(Res ~ Gov, poisson); summary(first.glm)
first.glm2 <- glm(Res ~ log(years)+Gov, poisson); summary(first.glm)
first.glm3 <- glm(Res ~ log(years)+Gov+  Gov:log(years), poisson); summary(first.glm)
first.glm4<- glm(Res ~ Gov + offset(log(years)), poisson); summary(next.glm)

############### AIC                    ###############
extractAIC(first.glm1, k=2)
extractAIC(first.glm2, k=2)
extractAIC(first.glm3, k=2)
extractAIC(first.glm4, k=2)

########## confidence interval              #########
first.glm2 <- glm(Res ~ log(years)+Gov, poisson); summary(first.glm)
confint(first.glm2, level=0.95)

########## LRT: ANOVA                ################

anova(first.glm3,first.glm2, test ="Chisq")


###########  Model selection            ############

step(first.glm, direction = "backward")



#########################################
# EXAMPLE 3:                            #
#########################################


CHD <- read.table("C:/projects/GLM/data4glm/chd1.txt", header=T)
attach(CHD)
CHD

fit.chd1<-glm(death ~ age + smoke+offset(log(pop)), poisson)
fit.chd2<-glm(death ~ age + smoke+age:smoke+offset(log(pop)), poisson)
age2<-age^2
fit.chd3<-glm(death ~ age+age2+smoke+age:smoke+offset(log(pop)), poisson)

plot(CHD$age[smoke=="s"],CHD$death[smoke=="s"]/CHD$pop[smoke=="s"],ylim=c(0,0.03))
points(CHD$age[smoke=="ns"],CHD$death[smoke=="ns"]/CHD$pop[smoke=="ns"],pch="+")

summary(fit.chd1)
summary(fit.chd2)
summary(fit.chd3)


plot(CHD$age[smoke=="s"],CHD$death[smoke=="s"]/CHD$pop[smoke=="s"],ylim=c(0,0.03))
points(CHD$age[smoke=="ns"],CHD$death[smoke=="ns"]/CHD$pop[smoke=="ns"],pch="+")
lines(CHD$age[smoke=="ns"],fit.chd1$fit[smoke=="ns"]/CHD$pop[smoke=="ns"])
lines(CHD$age[smoke=="s"],fit.chd1$fit[smoke=="s"]/CHD$pop[smoke=="s"])


plot(CHD$age[smoke=="s"],CHD$death[smoke=="s"]/CHD$pop[smoke=="s"],ylim=c(0,0.03))
points(CHD$age[smoke=="ns"],CHD$death[smoke=="ns"]/CHD$pop[smoke=="ns"],pch="+")
lines(CHD$age[smoke=="ns"],fit.chd2$fit[smoke=="ns"]/CHD$pop[smoke=="ns"])
lines(CHD$age[smoke=="s"],fit.chd2$fit[smoke=="s"]/CHD$pop[smoke=="s"])



plot(CHD$age[smoke=="s"],CHD$death[smoke=="s"]/CHD$pop[smoke=="s"],ylim=c(0,0.03))
points(CHD$age[smoke=="ns"],CHD$death[smoke=="ns"]/CHD$pop[smoke=="ns"],pch="+")
lines(CHD$age[smoke=="ns"],fit.chd3$fit[smoke=="ns"]/CHD$pop[smoke=="ns"])
lines(CHD$age[smoke=="s"],fit.chd3$fit[smoke=="s"]/CHD$pop[smoke=="s"],lty=2)


extractAIC(fit.chd1, k=2)
extractAIC(fit.chd2, k=2)
extractAIC(fit.chd3, k=2)


