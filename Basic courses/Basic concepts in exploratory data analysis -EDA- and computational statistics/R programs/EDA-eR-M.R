#######################################################
#######################################################
## Basic Concept in Exploratory Data Analysis        ##
## >eR-Biostat                                       ##
## 2017                                              ##
## Dinberu Seyoum and Ziv Shkedy and Martin Otava    ##
#######################################################

#######################################################
##### chapter 1: Measure of location      #############
#######################################################

x<-seq(-3,3,0.1)
plot(x,dnorm(x, -2, 1),type="l")
lines(x,dnorm(x, 0, 1),col="blue")
lines(x,dnorm(x, 2, 1),col="red")

x<-c(2,4,7,1,2,9,6,4,10,18)
x

sort(x)
mean(x)
median(x)
mean(x,trim=0.1) # 10 * trimmed mean
mean(x,trim=0.2) # 20% trimmed mean

#######################################################
##### The singer dataset                  #############
#######################################################

library(lattice)
data(singer)
stripplot(voice.part~height,data=singer,cex=0.5,jitter=T)

attach(singer)
tapply(height, voice.part, mean)
height.m <- tapply(height,voice.part,mean) #calculate the mean for each group
dotplot(names(height.m)~height.m,cex=1.25)

#######################################################
##### The mtcars dataset                  #############
#######################################################

data()
str(mtcars) 
help(mtcars)

#######################################################
################ Simple Dotplot   #####################
#######################################################

dotchart(mtcars$mpg,labels=row.names(mtcars),cex=.7,
main="Gas Milage for Car Models",
xlab="Miles Per Gallon")


#######################################################
# Dotplot: Grouped Sorted and Colored                 #
# Sort by mpg, group and color by cylinder            #
#######################################################


x <- mtcars[order(mtcars$mpg),] # sort by mpg
 x$cyl <- factor(x$cyl) # it must be a factor
 x$color[x$cyl==4] <- "red"
 x$color[x$cyl==6] <- "blue"
 x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl,
    main="Gas Milage for Car Models\ngrouped by cylinder",
    xlab="Miles Per Gallon", gcolor="black", color=x$color)


#######################################################
################ Boxplot by group #####################
#######################################################


bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")


#######################################################
################ Shape            #####################
#######################################################


x1 <- rnorm(100,0,0.5)
x2 <- rnorm(100,2,0.5)


par(mfrow=c(2,1)) #put two figures in one page,
hist(x1)
hist(x2)

x1 <- rnorm(1000, 0, 2)
hist(x1)
hist(x2)
mean(x1)
mean(x1, trim = 0.1)
median(x1)
mean(x2)
mean(x2, trim = 0.1)
median(x2)

############## trimmed mean            ##############
x<-rnorm(10,0,1)
mean(x)
mean(x,trim=0.1)
x<-sort(x)
x[10]<-100*x[10]
mean(x)
mean(x,trim=0.1)

#######################################################
##### The fuel dataset                    #############
##### strip and boxplot                   #############
##### install: SemiPar                    ############# 
#######################################################


library(SemiPar)
data(fuel.frame)
attach(fuel.frame)
names(fuel.frame)
stripplot(Type~Mileage,data=fuel.frame)
bwplot(Type~Mileage,data=fuel.frame)


########################################################
##Chapter 2: Measure of spread                     #####   
########################################################

xi <- c(24,35,39,50,60,60,75,80)
median(xi)

xi-median(xi)
abs(xi-median(xi))
sort(abs(xi-median(xi)))
median(abs(xi-median(xi))) # the MAD
xi[8] <- 800 # change the maximum value to 800
xi
mad(xi,constant=1) # calculate the MAD

########################################################
## graphical display for spread and location       #####
## read the external dataset SAT                   #####   
########################################################

SAT <- read.table("C:/projects/eR-Biostat/courses/StatisticalComputing_Martin/Data/Sat.txt",header=T)
attach(SAT)
names(SAT)<-c("Place","Sat")
SAT
Urban.sat<-SAT[,2][SAT[,1]=="Urban"]
Rural.sat<-SAT[,2][SAT[,1]=="Rural"]
Place<-SAT[,1]
boxplot(Urban.sat,medcol=1)
quantile(Urban.sat)
quantile(Rural.sat)

diff(quantile(Urban.sat, probs = c(0.25, 0.75)))
diff(quantile(Rural.sat, probs = c(0.25, 0.75)))
boxplot(split(SAT[,2],Place),names=c("Rural","Urban"),medcol=1)
bwplot(Sat~Place,data=SAT)

########################################################
## graphical display for spread and location       #####
## read the life expectancy data of 44 countries   #####   
## life.expe.csv                                   #####
########################################################

tt<-read.csv('C:\\projects\\eR-Biostat\\courses\\StatisticalComputing_Martin\\Data\\life.expe.csv',header=T,dec=",",sep=";")
attach(tt)
boxplot(M.life.exp,F.life.exp,col=c("gold","darkgreen"),names=c("Male","Female"),outlier = TRUE,main="Life expectancy of 44 countries")
quantile(M.life.exp,probs = c(0.25, 0.75))
quantile(F.life.exp,probs = c(0.25, 0.75))
diff(quantile(M.life.exp, probs = c(0.25, 0.75)))
diff(quantile(F.life.exp, probs = c(0.25, 0.75)))

########################################################
## Spread in the SAT data                          #####
########################################################

Urban.sat<-SAT[,2][SAT[,1]=="Urban"]
Rural.sat<-SAT[,2][SAT[,1]=="Rural"]
var(Urban.sat)
mad(Urban.sat)
quantile(Urban.sat,prob=c(0.25,0.75))
diff(quantile(Urban.sat,prob=c(0.25,0.75)))

var(Rural.sat)
mad(Rural.sat)
quantile(Rural.sat,prob=c(0.25,0.75))
diff(quantile(Rural.sat,prob=c(0.25,0.75)))


########################################################
## Spread in Hypothetical example                  #####
########################################################

x1 <- c(24,35,39,50,60,60,75,80) # sample 1
x2 <- c(24,35,39,50,60,60,75,800) # sample 2
#spread.ratio 
x <- rnorm(100,0,1)
var(x)
mad(x)
quantile(x,prob=c(0.25,0.75))
diff(quantile(x,prob=c(0.25,0.75)))

########################################################
## Spread in Hypothetical example                  #####
## Interquartile range                             #####
########################################################


x <- c(x,50)
var(x)
mad(x)
quantile(x,prob=c(0.25,0.75))
diff(quantile(x,prob=c(0.25,0.75)))

########################################################
#### Exercise                                         ##
########################################################
quantile(x1)
quantile(x2)
boxplot(x1,x2)
par(mfrow=c(1,2))
boxplot(x1,x2)
qqplot(x1,x2)
abline(0,1)

########################################################
########################################################
########Chapter 3: Resistance                       ####
########################################################
########################################################

x<-c(14,20,20,20,20,23,25,30,30,30,35,35,35,40,40,
42,50,50,80,80)
mean(x)
median(x)

x<-c(14,20,20,20,20,23,25,30,30,30,35,35,35,40,40,42,50,50,80,80)
par(mfrow=c(2,2))
boxplot(x,ylab="x")
plot(density(x),xlab="x")
qqnorm(x,xlab="x");qqline(x,col=2)

x.1<-c(14,20,20,20,20,23,25,30,30,30,35,35,35,40,40,42,50,50,80)
mean(x.1)
median(x.1)
x.2<-c(20,20,20,20,23,25,30,30,30,35,35,35,40,40,42,50,50,80)
mean(x.2)
median(x.2)

########################################################
##  Trimmed mean                                     ###
########################################################

mean (x, trim =a)
mean (x, trim=0.05)
mean(x, trim=0)
mean(x, trim=0.5)

mean (x, trim=0.1)
mean (x, trim=0.2)


########################################################
##  L-estimators                                     ###
########################################################

pop.15 <- c(778,355,248,200,167,94,94,88,76,75,74,74,70,68,63)
sort(pop.15)
mean(pop.15)
median(pop.15)
mean(pop.15,trim=0.1)
mean(pop.15,trim=0.2)

mean(pop.15,trim=0)
mean(pop.15,trim=0.5)

sort(x)  # to sort the data set
quantile(x,probs = c(0.25, 0.37)) # first quartile, and 37th sample quantile.

#########################################################
#########################################################
##  Sensitivity curve of a L-estimator                 ##
#########################################################
#########################################################


x<-c(14,20,20,20,20,23,25,30,30,30,35,35,35,40,40,42,50,50,80,80)
mean(x)
median(x)
x.new <- c(12,x)
x.new
mean(x.new)
median(x.new)

#########################################################
##  Change in the mean and median                      ##
#########################################################
 
x<-c(14,20,20,20,20,23,25,30,30,30,35,35,35,40,40,42,50,50,80,80)
mean(x)
median(x)
x.new <- c(10,85,x)
x.new
mean(x.new)
median(x.new)


#########################################################
##  Sensitivity curve for a spesific eample            ##
#########################################################

x<-c(14,20,20,20,20,23,25,30,30,30,35,35,35,40,40,42,50,50,80,80)
mean(x)
median(x)
new.value <- seq(from=10,to=85,length=50)
mean.i <- c(1:50)
median.i <- c(1:50)
for(i in 1:50){
 x.new <- c(new.value[i],x) 
     # new.value[i] is the "new observation"
 mean.i[i] <- mean(x.new)-mean
 median.i[i] <- median(x.new)-median
}
par(mfrow=c(1,2))
plot(new.value,mean.i,type="l",xlab="New value",ylab="mean diff.")#mean difference
plot(new.value,median.i,type="l",xlab="New value",ylab="median diff.")#median difference

###########################################################
##############     Chapter 4: Robustness         ##########
###########################################################

x<-c(14,20,20,20,20,23,25,30,30,30,35,35,35,40,40,42,50,50,80,80)
par(mfrow=c(1,3))
hist(x)
plot(density(x),xlab="x")
qqnorm(x)
qqline(x,col=2)

###########################################################
##############     a sample from N(mu,sigma)     ##########
###########################################################

par(mfrow=c(1,2))
x <- rnorm(1000,0,1)
hist(x)
qqnorm(x)
qqline(x)

###########################################################
##############     a sample from  contaminated           ##
##############     Normal dist.                          ##
###########################################################

par(mfrow=c(1,2))
x1 <- rnorm(900,0,1)
x2 <- rnorm(100,0,10)
xx <- c(x1,x2)   # contaminated
hist(xx)
qqnorm(xx)
qqline(xx)

###########################################################
## Relative efficiency                                   ##
###########################################################

####  normal distribution               ###



x.mean <- x.mean.t <- x.med <- c(1:1000)
for(i in 1:1000){
x.1 <- rnorm(50,0,1)
x.mean[i] <- mean(x.1)
x.mean.t[i] <- mean(x.1,trim=0.2)
x.med[i] <- median(x.1)
}

var(x.mean)/var(x.med)
var(x.mean)/var(x.mean.t)
boxplot(x.mean,x.mean.t,x.med)

####  Contaminated normal distribution  ###

x.mean <- x.mean.t <- x.med <- c(1:1000)
for(i in 1:1000){
x.1 <- rnorm(45,0,1)
x.2<-rnorm(5,0,10)
xx=c(x.1,x.2)
x.mean[i] <- mean(xx)
x.mean.t[i] <- mean(xx,trim=0.2)
x.med[i] <- median(xx)
}
var(x.mean)/var(x.med)
var(x.mean)/var(x.mean.t)
boxplot(x.mean,x.mean.t,x.med)


### t(3) distribution       ######

x.mean <- x.mean.t <- x.med <- c(1:1000)
for(i in 1:1000) {
x.1 <- rt(50,3)
x.mean[i] <- mean(x.1)
x.mean.t[i] <- mean(x.1,trim=0.2)
x.med[i] <- median(x.1)
}
var(x.mean)/var(x.med)
var(x.mean)/var(x.mean.t)
boxplot(x.mean,x.mean.t,x.med)


