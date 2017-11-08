##############################################################
### >eR-Biostat initaive                                   ###
### Linear Models                                          ###
### Simple Linear Regression                               ###
### Chapter 1                                              ###
### Written by LD/ZS                                       ###
### Online from: April 2017                                ###
##############################################################

#######################################################
##### Example 1: Galapagos Islands Data   #############
#######################################################     

library(faraway)
data(gala)
attach(gala)

plot(Elevation,Species)
SLR.fit.1 <- lm(Species~Elevation)
summary(SLR.fit.1)



#######################################################
##### Example 2: airquality  Data (quadraric model) ###
#######################################################

help(airquality)
Ozone<-airquality$Ozone
length(na.omit(Ozone))
Ozone1<-na.omit(Ozone)
length(Ozone1)
Temp<-airquality$Temp
Temp1<-Temp[!is.na(Ozone)]
length(Temp1)
plot(Temp1,Ozone1)
Temp2<-Temp1^2

fit.air_q1<-lm(Ozone1~Temp1)
summary(fit.air_q1)
plot(Temp1,Ozone1)
lines(Temp1,fit.air_q1$fit)


#######################################################
##### Example 3: the cars data                      ###
#######################################################

help(cars)
plot(cars$speed,cars$dist)
fit.lm<-lm(cars$dist~cars$speed)
summary(fit.lm)
lines(cars$speed,fit.lm$fit)

library(rJava)
library(xlsx)


#######################################################
##### Example 3: the sat data (homwork)             ###
#######################################################

sat.dat<-read.table('C:\\projects\\eR-Biostat\\courses\\LinearModels\\data\\sat1.txt',
                 header=TRUE,na.strings="NA", dec=",")
head(sat.dat)

plot(sat.dat$high_GPA,sat.dat$univ_GPA)




