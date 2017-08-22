###########################################################################################
#      Basic concept in statistical modeling using R                                      #                               #
#                       Developed by                                                      #                     
#   Ziv Shkedy (Hasselt University) & Legesse Kassa Debusho, University of Pretoria       #
#                                                                                         # 
#   istall the R package : lmtest                                                         #
#                                                                                         # 
###########################################################################################




##########################################################################################
## PART 1: SIMPLE LINEAR REGRESSION                                                      #
##########################################################################################


##########################################################################################
##        A Biopharmaceutical Problem                                                    #
##########################################################################################


#####################################
##  The data in R                   #
#####################################


Dose <- c(60,60,60,90,90,90, 120,120,120,150,150,150,180,180,180)
Score <- c(56.07362,49.45516,56.07840,74.18539,73.13873,77.35170,95.37789,93.03198,
           92.46663,117.61100,123.56117,119.12260,130.81847,137.31600,139.09742)
dose.data <- cbind(Dose,Score)
print(dose.data)
plot(Dose,Score)

#####################################
##  Simple linear regression in  R  #
#####################################

fit.dose <- lm(Score ~ Dose)
summary(fit.dose)

anova(fit.dose)

#####################################
##  Data and predicted model        #
#####################################

plot(Dose,Score, 
   ylab = "Test Score (Y)", xlab = "Dose level (X)")
x <- Dose
y <- fit.dose$fit
lines(x,y)


#####################################
##  Diagnostic plot                 #
#####################################

par(mfrow=c(2,2))
plot(Score,fit.dose$fit, xlab = "Observed", 
   ylab = "Predicted", main = "Observed versus predicted values")
abline(0,1)
hist(fit.dose$resid,col = 0,main = "Histogram for residuals")
qqnorm(fit.dose$resid)


#####################################
##  Diagnostic plot                 #
#####################################

par(mfrow = c(2,2))
plot(fit.dose)


#####################################
# Shapiro-Wilk normality test       #
#####################################

shapiro.test(residuals(fit.dose))

#####################################
#  studentized Breusch-Pagan test   #
#####################################

#######################################################################
## Testing the Constant Variance Assumption                          ##
## We will use the Breusch-Pagan test to decide whether the variance ##
## of the residuals is nonconstant.                                  ##
## The null hypothesis is that the variance is the same for all      ##
## observations, and the alternative hypothesis                      ##
## is that the variance is not the same for all observations.        ##
## We reject the null hypothesis if BP is too large, which happens   ##
## when the explained variation in the                               ##
## new model is large relative to the unexplained variation in the   ##
## original model.                                                   ##
#######################################################################

library(lmtest)
bptest(fit.dose)

#####################################
#  Durbin-Watson test               #
#####################################


#######################################################################
## Testing the Independence Assumption                               ##
## Durbin-Watson test                                                ##
#######################################################################

library(lmtest)
dwtest(fit.dose, alternative = "two.sided")


################################################################
## EXAPLE 1: All model asgsumptions hold                        #
################################################################

example1 <- read.table('C:/projects/eR-Biostat/courses/Intro_Stat_Mod using R/Regression/Data/example1.txt', 
                        header = TRUE, na.strings = "NA", dec = ".")

names(example1)   
attach(example1)
print(example1)


#####################################
#  Scatter plot of the data         #
#####################################

plot(y ~ x, data = example1, ylab = "Y", xlab = "X")


#####################################
#  Fitting the model                #
#####################################

fit.example1 <- lm(y ~ x, data = example1)
summary(fit.example1)
aov(fit.example1)

#####################################
##  Data and predicted model        #
#####################################

plot(y ~ x, data = example1, ylab = "Y", xlab = "X")
x <- example1$x
y <- fit.example1$fit
lines(x,y)

#####################################
##  Diagnostic plot                 #
#####################################

par(mfrow=c(2,2))
plot(fit.example1$fit, fit.example1$resid, xlab = "Residuals", 
   ylab = "Predicted values", main = "Predicted values versus residuals")
plot(example1$x, fit.example1$resid, xlab = "Predictor", 
   ylab = "Residuals", main = "Residuals versus predictor")
qqnorm(fit.example1$resid)

#####################################
##  Diagnostic plot                 #
#####################################
par(mfrow = c(2,2))
plot(fit.example1)


#####################################
##  Residual analysis               #
#####################################
shapiro.test(residuals(fit.example1))

library(lmtest)
bptest(fit.example1)

library(lmtest)
dwtest(fit.example1, alternative = "two.sided")




################################################################
## EXAPLE 2: The variance is not constant                      #
################################################################


example2 <- read.table('C:/projects/eR-Biostat/courses/Intro_Stat_Mod using R/Regression/Data/example2.txt', 
                        header = TRUE, na.strings = "NA", dec = ".")
names(example2)   
attach(example2)
print(example2)

#####################################
#  Scatter plot of the data         #
#####################################

plot(y ~ x, data = example2, ylab = "Y", xlab = "X")


#####################################
#  Fitting the model                #
#####################################

fit.example2 <- lm(y ~ x, data = example2)
summary(fit.example2)

aov(fit.example2)

#####################################
##  Data and predicted model        #
#####################################

plot(y ~ x, data = example2, ylab = "Y", xlab = "X")
x <- example2$x
y <- fit.example2$fit
lines(x,y)

#####################################
##  Diagnostic plot                 #
#####################################

par(mfrow=c(2,2))
plot(fit.example2$resid, fit.example2$fit, xlab = "Predicted values", 
        ylab = "Residuals", main = "Predicted values versus residuals")
plot(example2$x, fit.example2$resid, xlab = "Predictor", 
   ylab = "Residuals", main = "Residuals versus predictor")
qqnorm(fit.example2$resid)

#####################################
##  Diagnostic plot                 #
#####################################
par(mfrow = c(2,2))
plot(fit.example2)


#####################################
##  Residual analysis               #
#####################################
shapiro.test(residuals(fit.example2))

library(lmtest)
bptest(fit.example2)

library(lmtest)
dwtest(fit.example2, alternative = "two.sided")


################################################################
#     EXAPLE 3: Structure in the residuals                     #
################################################################

example3 <- read.table('C:/projects/eR-Biostat/courses/Intro_Stat_Mod using R/Regression/Data/example3.txt', 
                        header = TRUE, na.strings = "NA", dec = ".")
names(example3)   
attach(example3)
print(example3)

#####################################
#  Scatter plot of the data         #
#####################################
plot(y ~ x, data = example3, ylab = "Y", xlab = "X")

#####################################
#  Fitting the model                #
#####################################

fit.example3 <- lm(y ~ x, data = example3)
summary(fit.example3)
aov(fit.example3)


#####################################
##  Data and predicted model        #
#####################################

plot(y ~ x, data = example3, ylab = "Y", xlab = "X")
x <- example3$x
y <- fit.example3$fit
lines(x,y)

#####################################
##  Diagnostic plot                 #
#####################################

par(mfrow=c(2,2))
plot(fit.example3$resid, fit.example3$fit, xlab = "Predicted values", 
        ylab = "Residuals", main = "Predicted values versus residuals")
plot(example3$x, fit.example3$resid, xlab = "Predictor", 
   ylab = "Residuals", main = "Residuals versus predictor")
qqnorm(fit.example3$resid)

#####################################
##  Diagnostic plot                 #
#####################################
par(mfrow = c(2,2))
plot(fit.example3)


#####################################
##  Residual analysis               #
#####################################

shapiro.test(residuals(fit.example3))

library(lmtest)
bptest(fit.example3)

library(lmtest)
dwtest(fit.example3, alternative = "two.sided")



################################################################
## EXAPLE 4: The distribution of the residuals is not normal   #
################################################################



example4 <- read.table('C:/projects/eR-Biostat/courses/Intro_Stat_Mod using R/Regression/Data/example4.txt', 
                        header = TRUE, na.strings = "NA", dec = ".")

names(example4)   
attach(example4)
print(example4)

#####################################
#  Scatter plot of the data         #
#####################################
par(mfrow=c(1,1))
plot(y ~ x, data = example4, ylab = "Y", xlab = "X")

#####################################
#  Fitting the model                #
#####################################

fit.example4 <- lm(y ~ x, data = example4)
summary(fit.example4)
aov(fit.example4)

#####################################
##  Data and predicted model        #
#####################################

plot(y ~ x, data = example4, ylab = "Y", xlab = "X")
x <- example4$x
y <- fit.example4$fit
lines(x,y)


#####################################
##  Diagnostic plot                 #
#####################################

par(mfrow=c(2,2))
plot(fit.example4$resid, fit.example4$fit, xlab = "Residuals",  
       ylab = "Predicted values",main = "Predicted values versus residuals")
plot(example4$x, fit.example4$resid, xlab = "Predictor", 
   ylab = "Residuals", main = "Residuals versus predictor")
qqnorm(fit.example4$resid)

#####################################
##  Diagnostic plot                 #
#####################################

par(mfrow = c(2,2))
plot(fit.example4)

#####################################
##  Residual analysis               #
#####################################

shapiro.test(residuals(fit.example4))

library(lmtest)
bptest(fit.example4)

library(lmtest)
dwtest(fit.example4, alternative = "two.sided")


