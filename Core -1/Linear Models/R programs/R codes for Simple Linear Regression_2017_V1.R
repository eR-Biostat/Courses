##############################################################
### >eR-Biostat initaive                                   ###
### Linear Models                                          ###
### Simple Linear Regression                               ###
### Chapter XXX                                            ###
### Written by LD/ZS                                       ###
### Online from: April 2017                                ###
##############################################################
     

library(faraway)
data(gala)
attach(gala)

plot(Elevation,Species)
SLR.fit.1 <- lm(Species~Elevation)
summary(SLR.fit.1)