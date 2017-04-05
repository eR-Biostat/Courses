###--- Multiple linear regression ---###

##--- Flat surface ---##
x1 <- seq(0,1,length=10)
   #Generates a sequence of 10 random numbers between 0 and 1
x2 <- seq(0,1,length=10)
   #Generates a sequence of 10 random numbers between 0 and 1
f1 <- function(x1,x2) {l <- 1 -0.1 * x1 -0.01 * x2}
  # A function that uses x1 and x2 to produce Y
y <- outer(x1,x2,f1)     # calculates the outer products of
persp(x1,x2,y,theta=30,phi=30,ltheta=120,zlab="y", 
      main = "Figure 1. Linear flat surface")
  #draw perspective plots of a surface over the x–y plane with
  #theta and phi defining the viewing direction

f2 <- function(x1,x2) {l <- 0.1 * x1 * x1 + 0.05 * x2 * x2}
y <- outer(x1,x2,f2)
persp(x1,x2,y,theta=30,phi=30,ltheta=120,zlab="y",
      main = "Figure 2. Linear curved surface but linear in parameters")
  # draw perspective plots of a curve surface over the x–y plane with
  # theta and phi defining the viewing direction

##-- Example: Galapagos Islands Data --##
library(faraway)
data(gala)
attach(gala)
names(gala) #- to check names of the variables in the data

#- Multiple linear regression analysis -#
MLR.fit.1 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(MLR.fit.1)

