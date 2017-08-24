######################################################################
#          Basic concept in statistical modeling:                    #
#                   Logistic regression                              #                              
#                                                                    #
#                                                                    #
#                       Developed by                                 #                                 
#                                                                    #
#           Ziv Shkedy (Hasselt University, Belgium)                 #
#                                                                    #
#                            &                                       #
#                                                                    #
#          Legesse Kassa Debusho (UNISA, South Africa)               #
#                                                                    #
######################################################################



######################################################################
# Part 4: Modelling Binary Data using the glm() function)            #
######################################################################

######################################################################
# Example 1: Smoked Mice                                             #
######################################################################


mice <- data.frame(Treatm=c("Treated", "Control"),
           Tumour = c(21,19), Total = c(23,32))
attach(mice)
mice            


##############################
# Modeling(I):only intercept)#
##############################

fit1.mice <- glm(cbind(Tumour, Total-Tumour)~1, 
              data = mice, family = binomial("logit"))
summary(fit1.mice)
summary(fit1.mice)$coeff


############################
# Modeling  (II)           #
############################

fit2.mice <- glm(cbind(Tumour ,Total-Tumour)~factor(Treatm),
                   data = mice, family = binomial("logit"))
summary(fit2.mice)

summary(fit2.mice)$coeff

y<-c(rep(1,21),rep(0,2),rep(1,19),rep(0,13))
gr<-c(rep("treated",23),rep("control",32))

fit2a.mice<-glm(y~as.factor(gr),family = binomial("logit"))
summary(fit2a.mice)


OR1<-(21*13)/(19*2)
OR1
log(OR1)
summary(fit2.mice)$coeff

##############################
# Modeling(III):a model      #
                without      #
#               intercept    #
##############################

fit3.mice <- glm(cbind(Tumour ,Total-Tumour)~-1 + factor(Treatm),
                 data = mice, family = binomial("logit"))
summary(fit3.mice)$coeff



######################################################################
# Example 2: Serological data                                        #
######################################################################


Serolog <- read.table('C:/projects/eR-Biostat/courses/Intro_Stat_Mod using R/LogisticReg/Data/Serological.txt', 
               header = TRUE, na.strings = "NA", dec = ".")

attach(Serolog)
print(Serolog)

p <- pos/N
plot(p ~ Age, xlab = "Age", ylab = "Prevalence")


############################
# Modeling  (I)            #
############################

fit.Sero <- glm(pos/N ~ Age, data = Serolog, 
               family = binomial)
summary(fit.Sero)

############################
# data and fitted model    #
############################

plot(p ~ Age, xlab = "Age", ylab = "Prevalence")
lines(Age, fit.Sero$fit)



######################################################################
# Example 3: Bioassay                                                #
######################################################################

serum <- read.table('C:/projects/eR-Biostat/courses/Intro_Stat_Mod using R/LogisticReg/Data/Serum.txt', 
                    header = TRUE, na.strings = "NA", dec = ".")
print(serum)

attach(serum)

serum$ldose <- log(serum$dose)


############################
# Modeling  (I)            #
############################

fit.serum <- glm(death/N ~ ldose, data = serum, 
               family = binomial)
summary(fit.serum)

############################
# data and fitted model    #
############################

plot(death/N  ~ ldose, data = serum, xlab = "Dose", ylab = "Proportion of deaths")
lines(serum$ldose, fit.serum$fit)



######################################################################
# Example 4: Determination of ESR                                    #
######################################################################

esr <- read.table('C:/projects/eR-Biostat/courses/Intro_Stat_Mod using R/LogisticReg/Data/ESR.txt', 
                   header = TRUE, na.strings = "NA", dec = ".")
print(esr)
attach(esr)


############################
#  Plot of the data        #
############################

plot(Y ~ Fib, data = esr, xlab = "Level of Fibrinogen", ylab = "ESR > 20")


############################
# Modeling  (I)            #
############################

fit.esr <- glm(Y ~ Fib, data = esr, family = binomial)
summary(fit.esr)

############################
# data and fitted model    #
############################

plot(Y ~ Fib, data = esr, xlab = "Level of Fibrinogen", ylab = "ESR > 20")
lines(Fib, fit.esr$fit)

summary(fit.esr)$coeff



######################################################################
# Example 5: Pneumoconiosis data                                     #
######################################################################

Years<-c(5.8,15.0,21.5,27.5,33.5,39.5,46.0,51.5)
Cases<-c(0,1,3,8,9,8,10,5)
Miners<-c(98,54,43,48,51,38,28,11)

data.frame(Years,Cases,Miners)
CW<-cbind(Cases,Miners-Cases)
CW

plot(Years,Cases/Miners, xlab = "Exposure year", ylab = "Proportion of cases")

fit.miners2 <- glm(CW~ Years, family = binomial)
summary(fit.miners2)


plot(Years,Cases/Miners, xlab = "Exposure year", ylab = "Proportion of cases",ylim=c(0,0.6))
lines(Years,fit.miners2$fit)




