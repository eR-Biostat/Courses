################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 8                                                    #
# GLM                                                          #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################



################################################################
## Example 1: Budworm data dose response data of the binomial  #
################################################################

ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive=20-numdead)
p<-numdead/20
budworm.lg <- glm(SF ~ sex*ldose, family=binomial)

summary(budworm.lg)$cov.unscaled


summary(budworm.lg)



par(mfrow=c(1,2))
plot(p ~ ldose)
plot(p ~ log(ldose))

budworm.lg1 <- glm(SF ~ ldose, family=binomial)
budworm.lg2 <- glm(SF ~ sex + ldose, family=binomial)
budworm.lg3<- glm(SF ~ sex*ldose, family=binomial)

summary(budworm.lg1)
summary(budworm.lg2)
anova(budworm.lg1,budworm.lg2)
anova.glm(budworm.lg1,budworm.lg2,test="Chisq") 

summary(budworm.lg3)

anova.glm(budworm.lg2,budworm.lg3,test="Chisq") 

anova.glm(budworm.lg1,budworm.lg2,budworm.lg3,test="Chisq") 

update(budworm.lg3,~. -sex:ldose)

#########################################################
# End of code for Chapter 8                            ##
#########################################################

#########################################################
# Some extra code                                      ##
#########################################################

logLik(budworm.lg1)
logLik(budworm.lg2)
logLik(budworm.lg3)


budworm.lg1$null.deviance
budworm.lg1$deviance
budworm.lg2$null.deviance
budworm.lg2$deviance
budworm.lg3$null.deviance
budworm.lg3$deviance
