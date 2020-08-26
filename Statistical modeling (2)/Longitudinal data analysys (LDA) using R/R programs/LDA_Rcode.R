###########################################################
#                                                         #
#     Longitudinal data analysis (LDA) using R            #
#                                                         #
#     Tadesse Awoke, Gondar Universuty, Ethiopia          #
#                                                         #
#     July, 2020                                          #
###########################################################


install.packages("foreign")
install.packages("nlme")

library(foreign)



infant<- read.dta(file="C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/infant.dta")
View(infant)

mydata1 <- infant[which(infant$ind<20),] # data only <100 days of follow up

infant$wt<-infant$weight/1000

infant$age2<-infant$age*infant$age

# print first 16 rows of mydata
head(mydata1, n=16)


library(ggplot2)
library(methods)
library(labeling)
p <- ggplot(data = mydata1, aes(x = age, y = weight, group = ind))
# simple scatter plot
p + geom_point()
# simple spaghetti plot
p + geom_line()

## facet (condition) the graph base on the male variable
library(reshape2)
p + geom_line() + facet_grid(. ~ sex)

par(mfrow=c(1,1))
## mean profile 
attach(infant)
mean1<-tapply(weight, age, mean)
age1<-as.numeric(unique(age))
plot(age1, mean1, type= "l", xlab="age", ylab=" The mean weight", lwd=1, main=" The mean profile")

## mean profile for sex group
interaction.plot(age,sex,weight,fun=mean, col=2:14, xlab= "age", ylab= " Weight", las=1)

## Linear Regression Model
library("nlme")
infant$wt<-infant$weight/1000
fit0.lm<-lmList(wt~age|ind, data=infant)
summary(fit0.lm)

pairs(fit0.lm, id=0.01, adj=-0.5)

plot(intervals(fit0.lm))

##########Variance plot ############
mydata$wt<-mydata$weight/1000
attach(mydata)


interaction.plot(age, sex,wt,fun=var, col=2:3, xlab= "age", ylab= " var[wt]", las=1)




##########Linear Mixed Effect Model ####
library(nlme)

infant.fit1 <- lme(wt ~ sex + age + age2 + sex*age, data = infant, method= "ML", random = ~ 1|ind)

print(infant.fit1)

coef(infant.fit1)

fixef(infant.fit1)

summary(infant.fit1)

infant.fit12 <- lme(wt ~ sex+ age + age2 + sex*age, data = infant, method= "ML", random = ~ 1+ age|ind)

summary(infant.fit12)


View(mydata22)
############ The Orthodontic Growth DataSet #############
mydata22<- read.csv(file="C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/growth2.csv")

mydata2<- read.csv(file="C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/growth.csv")
library(ggplot2)
library(methods)
library(labeling)
library(reshape2)
d <- ggplot(data = mydata22, aes(x = age, y = measure, group = ind))
# simple spaghetti plot
d + geom_line()
## facet (condition) the graph base on the male variable
d + geom_line() + facet_grid(. ~ sex)

View(mydata2)
##### Scatter plot matric #############
attach(mydata2)
d1<-response[age==6]
d2<-response[age==7]
d3<-response[age==8]
d4<-response[age==9]
d5<-response[age==10]
response1<-cbind(d1, d2, d3, d4, d5)
# correlation matrix
cor(response1)

#variance-covariance matrix
covmatrix = matrix(c(cov(response1)), nrow=5, ncol=5)

# Scatter plot matrix
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs(response1, panel=panel.smooth, cex = 1.5, pch = 16,
      bg="light green",diag.panel=panel.hist,
      cex.labels = 2, font.labels=2)


## Mean evolution profile ##
attach(mydata22)

mean1<-tapply(measure, age, mean)
age1<-as.numeric(unique(age))
sort(age1)
plot(sort(age1), mean1, type= "l",ylim=c(10,32), xlab="age", ylab=" The mean distance", lwd=3, main=" The mean profile of the growth data set")


####### Variance covariance Matrix ###
attach(mydata2)
varg<- tapply(response,age, var)
names(varg)
age1<-as.numeric(unique(age))
varg<-as.vector(varg)

#### overall observed variance plot 
plot(age1, varg, type="l",main =" Observed variance ",
     xlab="age", ylab="The variance of distance", lwd=1)


#### observed variance plot by group 
interaction.plot(age, group, response, lty=c(1, 2), fun=var,
                 ylab="distance from pituitary to pterygomaxillary fissure (mm) ",
                 xlab="Age", trace.label="Group")
title(main=" The variance of the growth data set by sex")




##########Linear Mixed Effect Model ####
fm.lis <-lmList(response ~ I(age-11) | child, data=mydata2)
summary(fm.lis)

pairs(fm.lis, id = 0.01, adj = -0.5)

intervals(fm.lis)

plot(intervals(fm.lis))

###### Linear mixed Model 
library(nlme)



growth.fit1 <- lme(fixed = measure ~ sex+ age + sex*age, data = mydata22, random = ~ 1|ind)
print(growth.fit1)
coef(growth.fit1)
fixef(growth.fit1)
growth.fit11 <- lme(fixed = measure ~ sex + age+ sex*age, method= "ML", data = mydata22, random = ~ 1|ind)
summary(growth.fit11)

#### Random slope model
growth.fit2 <- lme(fixed = measure ~ sex + age + sex*age, method= "ML", data = mydata22, random = ~ age|ind)
summary(growth.fit2)

VarCorr(growth.fit2)

# Get random effects with conditional variances
ranef(growth.fit2, condVar=TRUE)



########### Toenail Dataset #########
mydata3 <- read.csv("C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/Toenail_contineous.csv")
mydata33 <- mydata3[which(mydata3$id<101),] # data only <100 days of follow up

d <- ggplot(data = mydata3, aes(x = time, y = response, group = id))
# simple spaghetti plot
d + geom_line()
## facet (condition) the graph base on the male variable
library(reshape2)
d + geom_line() + facet_grid(. ~ treat)
beta0 <- m$estimate[1]

########### VL/HIV Dataset #########
mydata4 <- read.csv("C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/VL_HIV.csv")
d <- ggplot(data=mydata4, aes(x = Day, y = hgb, group = id))
# simple spaghetti plot
d + geom_line()
## facet (condition) the graph base on the male variable
library(reshape2)
d + geom_line() + facet_grid(. ~ treat)


########### TRat Dataset #########
mydata5 <- read.csv("C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/rat.csv")
d <- ggplot(data=mydata5, aes(x = age, y = response, group = rat))
# simple spaghetti plot
d + geom_line()
## facet (condition) the graph base on the male variable
library(reshape2)
d + geom_line() + facet_grid(. ~ treat)



##########Model for non-gaussian longitudinal data 
Toenail <- read_excel("C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/Toenail.xls")
library("MASS")
library("lme4")
library("minqa")
library("nloptr")
library("gee")
library("geepack")
library("wgeesel")
library("bindata")
library("e1071")
library("mvtnorm")
library("PoisNor")
library("corpcor")
library("CRTgeeDR")
library("MuMIn")
######### GEE in R 

fit1 <- geeglm(y ~ treatn + time + treatn*time, id = idnum, data = Toenail, family = binomial, corstr = "exchangeable", scale.fix = TRUE)
fit2 <- update(fit, corstr = "ar1")
fit3 <- update(fit2, corstr = "unstructured")
summary(fit1)

model.sel(fit1,fit2, fit3, rank = QIC) # QIC for the different model

sapply(list(fit1, fit2, fit3), QIC) # QIC for the different model
########## Generalized Linear Model ###########
model.fit <- glmer(y ~ treatn + time + treatn*time + (1|idnum), data = Toenail, family = binomial, 
                   control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(model.fit, corr = TRUE)
summary(model.fit)

View(Infant)
############## Jimma infant data ##############
library(readxl)
Infant <- read_excel("C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/Infant.xls")
############## GEE
fit21 <- geeglm(BMIBIN~ sex + age + sex*age, id = ind, data = Infant, family = binomial, corstr = "exchangeable", scale.fix = TRUE)
fit22 <- update(fit21, corstr = "ar1")
fit23 <- update(fit22, corstr = "unstructured")
summary(fit21)

model.sel(fit21,fit22, fit23, rank = QIC)

sapply(list(fit21, fit22, fit23), QIC)


############## GLMM
library(readxl)
Infant <- read_excel("C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/Infant.xls")

########### GLMM (random intercept)
fitGLMM <- glmer(BMIBIN~ sex + age + sex*age + (1|ind), data = Infant, family = binomial(link = "logit"), nAGQ = 25)
summary(fitGLMM)

exp(cbind(ODDS=coef(fitGLMM), confint(fitGLMM)))
exp(confint(fitGLMM, method="profile", oldNames=FALSE))

exp(coef(fitGLMM))
########### GLMM (random intercept & slope)
fitGLMMSlope <- glmer(BMIBIN~ sex + age + sex*age + (1+age|ind), data = Infant, family = binomial(link = "logit"))

summary(fitGLMMSlope)

############## Epilepsy Data ##############
epilepsy <- read_excel("C:/Users/lucp8319/Desktop/Biostat_2018/LDA/LDA_2018/1dataset/epilepsy.xls")
############## GEE
fitgee1 <- geeglm(nseizw~ trt + studywee + trt*studywee, id = id, data = epilepsy, family = poisson(link = "log"), corstr = "exchangeable", scale.fix = TRUE)
summary(fitgee1)
fitgee2 <- update(fitgee1, corstr = "ar1")
fitgee3 <- update(fitgee2, corstr = "unstructured")


model.sel(fitgee1,fitgee2, fitgee3, rank = QIC)

sapply(list(fitgee1,fitgee2, fitgee3), QIC)

########### GLMM (random intercept)
fitGLMMInt <- glmer(nseizw~ trt + studywee + trt*studywee + (1|id), data = epilepsy, family = poisson(link = "log"))

summary(fitGLMMInt)

########### GLMM (random intercept & slope)
fitGLMMSlope <- glmer(nseizw~ trt + studywee + trt*studywee + (1+studywee|id), data = epilepsy, family = poisson(link = "log"))

summary(fitGLMMSlope)


??alr
 



