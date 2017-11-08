#######################################################
#######################################################
## Linear Models                                     ##
## Multiple linear regression                        ##
## >eR-Biostat                                       ##
## 2017                                              ##
## Legesse Kassa Debusho and Ziv Shkedy              ##
#######################################################

#######################################################
##### Figure 1 Flat surface               #############
#######################################################

x1 <- seq(0,1,length=10)
x2 <- seq(0,1,length=10)
f1 <- function(x1,x2) {l <- 1 -0.1 * x1 -0.01 * x2}

# A function that uses x1 and x2 to produce Y
y <- outer(x1,x2,f1)     # calculates the outer products of
persp(x1,x2,y,theta=30,phi=30,ltheta=120,zlab="y", 
      main = "Figure 1. Linear flat surface")
#draw perspective plots of a surface over the x–y plane with
#theta and phi defining the viewing direction

#######################################################
##### FIgure 2: Linear in the parameters  #############
#######################################################


f2 <- function(x1,x2) {l <- 0.1 * x1 * x1 + 0.05 * x2 * x2}
y <- outer(x1,x2,f2)
persp(x1,x2,y,theta=30,phi=30,ltheta=120,zlab="y",
      main = "Figure 2. Linear curved surface but linear in parameters")
# draw perspective plots of a curve surface over the x–y plane with
# theta and phi defining the viewing direction


#######################################################
##### Example 1: Galapagos Islands Data   #############
#######################################################

library(faraway)
data(gala)
attach(gala)
names(gala)

MLR.fit.1 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(MLR.fit.1)


#######################################################
##### Example 2: airquality  Data         #############
#######################################################

help(airquality)
fit.air_q<-lm(Ozone~Solar.R+Wind+Temp,data=airquality)
summary(fit.air_q)


#######################################################
##### Example 3: airquality  Data (quadraric model) ###
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
fit.air_q2<-lm(Ozone1~Temp1+Temp2)
summary(fit.air_q1)
summary(fit.air_q2)
plot(Temp1,Ozone1)
lines(Temp1,fit.air_q1$fit)
lines(sort(Temp1),fit.air_q2$fit[order(Temp1)],col=2,lwd=2)








