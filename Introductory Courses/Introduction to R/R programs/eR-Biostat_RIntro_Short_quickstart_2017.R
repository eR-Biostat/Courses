#########################################################################
#########################################################################
#                   An introduction to R                                #                             
#                       Developed by                                    #                             
#                  Ziv Shkedy and Dan Lin                               #
#                    Hasselt University                                 #
#                                                                       #
#                >eR-Biostat initaive (2017)                            #
#########################################################################
#########################################################################


#########################################################################
#########################################################################
# External datasets useed in this course:                               #
#########################################################################
#########################################################################

spwh3<-read.table('c:\\projects\\wseda\\spwh3.txt', header=FALSE,na.strings="NA", dec=".")
cashdat<-read.table('c:\\projects\\wseda\\Rintro\\cashdat.txt', header=FALSE,na.strings="NA", dec=".")
sero<-read.table('c:\\projects\\wseda\\Rintro\\sero1.txt', header=FALSE,na.strings="NA", dec=".")
spwh2<-read.table('c:\\projects\\wseda\\spwh2.txt', header=FALSE, 
                    na.strings="NA", dec=".")

  
##########################################################################
##########################################################################
## QUICKT START                                                         ##
##########################################################################
##########################################################################

###############################################
# Normal distribution                         #
###############################################

rnorm(100,0,1)
x<-rnorm(100,0,1)
x
mean(x)
var(x)
hist(x)


x<-rnorm(10000,0,1)
mean(x)
var(x)
hist(x,nclass=50)


x1<-rnorm(10000,0,1)
x2<-rnorm(10000,1,1)
par(mfrow=c(2,1))
hist(x1,nclass=50,xlim=c(-4,4))
hist(x2,nclass=50,xlim=c(-4,4))


x1<-rnorm(10000,0,1)
x2<-rnorm(10000,0,2)
par(mfrow=c(2,1))
hist(x1,nclass=50,xlim=c(-6,6))
hist(x2,nclass=100,xlim=c(-6,6))




###############################################
# The cars data: plot and summary stat        #
###############################################

cars

plot(cars$speed,cars$dist,pch="+",xlab="speed",ylab="stopping distance",col=3)
title("Speed Vs. Stopping diatance",col=2)

dim(cars)
cars[,1]
mean(cars$speed)
max(cars$speed)

cor(cars)




###############################################
# The sleep data: t-test                      #
###############################################
help(t.test)

sleep
help(sleep)

extra=sleep$extra
group=sleep$group
boxplot(split(extra,group))
t.test(extra~group,var.equal=TRUE)
t.obj=t.test(extra~group,var.equal=TRUE)
summary(t.obj)
print(t.obj)
t.obj$p.value
t.obj$statistic


###############################################
# The ToothGrowth data: practical session 2   #
###############################################


ToothGrowth 
names(ToothGrowth) 
t.test(len~supp,var.equal=TRUE,data=ToothGrowth)
boxplot(split(ToothGrowth$len,ToothGrowth$sup))




###############################################
# The faithful data: basic plot               #
###############################################

help(faithful)
faithful
faithful$eruption
mean(faithful$eruption)
x=faithful$eruption
mean(x)
median(x)
range(x)
min(x)
max(x)

hist(faithful$eruptions,main="eruptions time",col=2) 
hist(faithful$waiting) 
hist(faithful$eruptions,main="eruptions time") 
hist(faithful$eruptions,main="eruptions time",col=3,xlab="time of eruptions") 


par(mfrow=c(1,2))
hist(faithful$eruptions,main="eruptions time",col=2) 
hist(faithful$waiting,main="waiting time",col=3,xlab="waiting time") 





##########################################################################
##########################################################################
## CHAPTER 1                                                            ##
##########################################################################
##########################################################################

x <- 5
x
x^2
x + 6

x<-c("A","A","A","A","B","B","B","B")
x


y<-c(10,11,9,15,3,5,7,2)
y

ya<-y[x=="A"]
ya

tapply(y,x,mean)

z<-data.frame(x,y)
z

z$x
z$y

w<-c(1,2,40,2,3,9,200,4,6000)
matw<-matrix(w,3,3)
matw
diag(matw)
solve(matw)


x<-c(25,36,21)
gender<-c("M","M","F")
xdat<-data.frame(x,gender)
xdat



##########################################################################
##########################################################################
## CHAPTER 2: working with external file                                ##
##########################################################################
##########################################################################


spwh3<-read.table('c:\\projects\\wseda\\spwh3.txt', header=FALSE,na.strings="NA", dec=".")

dim(spwh3)
spwh3<-data.frame(spwh3)
names(spwh3)<-c("id","y","x1","gender")
y1<-spwh3$y[spwh3$gender==0]
y2<-spwh3$y[spwh3$gender==1]
t.test(y1,y2)


##########################################################################
##########################################################################
## CHAPTER 3: basic plot                                                ##
##########################################################################
##########################################################################


z<-rnorm(100,3,1)
mean(z)
median(z)
max(z)
min(z)

hist(z)
par(mfrow=c(1,2))
hist(z,col=4)
hist(z,col=5,nclass=25)


##########################################################################
##########################################################################
## CHAPTER 4: a for loop                                                ##
##########################################################################
##########################################################################

mx<-c(1:1000)
for(i in 1:1000)
{
x<-rnorm(10,2,1)
mx[i]<-mean(x)
}
hist(mx,nclass=25)



##########################################################################
##########################################################################
##  Chapter 5 Statistical modeling 1: Simple linear regression          ##
##########################################################################
##########################################################################
 
plot(cars$speed,cars$dist,pch="+",xlab="speed",ylab="stopping distance",col=3)
title("Speed Vs. Stopping diatance",col=2)
x<-cars$speed
y<-cars$dist
plot(x,y)
fit.1<-lm(y~x)
plot(x,y)
lines(x,fit.1$fit)
summary(fit.1)

par(mfrow=c(2,2))
plot(y,fit.1$fit,xlab="observed",
     ylab="predicted")
abline(0,1)
title("observed versus predicted values")
hist(fit.1$resid,col=0,main=" ")
title("histogram for residuals")
qqnorm(fit.1$resid)


##########################################################################
##########################################################################
##  Chapter 6- Statistical modeling 2: One way ANOVA                    ##
##########################################################################
##########################################################################

help(chickwts)
w<-chickwts[,1]
feed<-chickwts[,2]
boxplot(split(w,feed))
tapply(w,feed,mean)
lm.fit<-lm(w~feed)
summary(lm.fit)
anova(lm.fit)



cashdat<-read.table('c:\\projects\\wseda\\Rintro\\cashdat.txt', header=FALSE,na.strings="NA", dec=".")
dim(cashdat)
names(cashdat)<-c("cash","group")
attach(cashdat)
par(mfrow=c(2,2))
qqnorm(cash[group=="Elderly"])
qqnorm(cash[group=="Middle"])
qqnorm(cash[group=="Young"])
par(mfrow=c(1,2))
boxplot(split(cash,group))
Fit.aov<-aov(cash~group)
summary(Fit.aov)
par(mfrow=c(2,2))
qqnorm(Fit.aov$resid)
hist(Fit.aov$resid,col=0)
boxplot(split(Fit.aov$resid,group))


##########################################################################
##########################################################################
## CHAPTER 7: logistic regression                                       ##
##########################################################################
##########################################################################



sero<-read.table('c:\\projects\\wseda\\Rintro\\sero1.txt', header=FALSE,na.strings="NA", dec=".")
print(sero)
fit.glm<- glm(pos/ntot ~ age, family=binomial(link = "logit"))
summary(fit.glm)



##########################################################################
##########################################################################
## CHAPTER 8: user functions                                            ##
##########################################################################
##########################################################################

fch20<-function(x)
{
mean.x<-mean(x)
med.x<-median(x)
q.x<-quantile(x)
hist(x)
print(mean.x)
print(med.x)
print(q.x)
}


y<-na.omit(airquality$Ozone)
fch20(y)


##########################################################################
##########################################################################
## CHAPTER 9: two-way ANOVA                                             ##
##########################################################################
##########################################################################

################
## Example 1  ##
################


spwh3<-read.table('c:\\projects\\wseda\\spwh3.txt', header=FALSE,na.strings="NA", dec=".")
names(spwh3)<-c("id","y","x1","gender")

fit.1<-aov(y~as.factor(x1)+as.factor(gender))
anova(fit.1)
fit.2<-aov(y~as.factor(x1)+as.factor(gender)
           +as.factor(x1)*as.factor(gender))
anova(fit.1,fit.2)

################
## Example 2  ##
################

f1<-c("A1","A1","A1","A2","A2","A2","A1","A1","A1","A2","A2","A2")
f2<-c("B1","B1","B1","B1","B1","B1","B2","B2","B2","B2","B2","B2")
y<-c(10,11,12,9,7,6,11,13,14,7,5,8)
data.frame(y,f1,f2)

fit.1<-aov(y~f1+f2)
anova(fit.1)
fit.2<-aov(y~f1+f2+f1*f2)
anova(fit.2)
anova(fit.1,fit.2)


################
## Example 3  ##
################


f1<-c("A1","A1","A1","A2","A2","A2","A1","A1","A1","A2","A2","A2")
f2<-c("B1","B1","B1","B1","B1","B1","B2","B2","B2","B2","B2","B2")
y<-c(10,11,12,9,7,6,11,13,14,17,15,18)
data.frame(y,f1,f2)
fit.2<-aov(y~f1+f2+f1*f2)
anova(fit.2)


##########################################################################
##########################################################################
## CHAPTER 10: more about two-way ANOVA                                 ##
##########################################################################
##########################################################################

spwh3<-read.table('c:\\projects\\wseda\\spwh3.txt', header=FALSE,na.strings="NA", dec=".")
names(spwh3)<-c("id","y","x1","gender")
attach(spwh3)
fit.2<-aov(y~as.factor(x1)+as.factor(gender)+as.factor(x1)*as.factor(gender))
anova(fit.2)
slm1 <- step(fit.2)
summary(slm1)

##########################################################################
##########################################################################
## CHAPTER 11: more about linear regression                             ##
##########################################################################
##########################################################################

spwh2<-read.table('c:\\projects\\wseda\\spwh2.txt', header=FALSE, 
                    na.strings="NA", dec=".")
dim(spwh2)
names(spwh2)<-c("id","y","x1","x2","x3")
attach(spwh2)
fit.1<-lm(y~x1+x2)
fit.2<-lm(y~x1+x2+x3)
par(mfrow=c(2,2))
plot(fit.2)
drop1(fit.2, test="F")
AIC(fit.2)  

##########################################################################
##########################################################################
## CHAPTER 12: the for loop                                             ##
##########################################################################
##########################################################################

x <- c(11.201, 10.035, 11.118, 9.055, 9.434, 9.663, 10.403, 11.662, 9.285,8.84)
mean(x)
n<-length(x)
B<-1000
mx<-c(1:B)
for(i in 1:B){
cat(i)
boot.i<-sample(x,n,replace=T)
mx[i]<-mean(boot.i)
}
var(mx)
hist(mx)




##########################################################################
##########################################################################
## THE END                                                              ##
##########################################################################
##########################################################################
