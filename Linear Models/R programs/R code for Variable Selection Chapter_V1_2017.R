##############################################################
### >eR-Biostat initaive                                   ###
### Linear Models                                          ###
### Variable Selection                                     ###
### Chapter XXX                                            ###
### Written by LD/ZS                                       ###
### Online from: April 2017                                ###
##############################################################
     


###--- Variable Selection ---###
data(state)
statedata <- data.frame(state.x77, 
    row.names = state.abb, check.names = T)
MLR.fit.state <- lm(Life.Exp ~ ., data=statedata)
summary(MLR.fit.state)

#- Backward method -#
MLR.fit.state.1 <- update(MLR.fit.state, . ~ . - Area)
summary(MLR.fit.state.1)

MLR.fit.state.2 <- update(MLR.fit.state.1, . ~ . - Illiteracy)
summary(MLR.fit.state.2)

MLR.fit.state.3 <- update(MLR.fit.state.2, . ~ . - Income)
summary(MLR.fit.state.3)

MLR.fit.state.4 <- update(MLR.fit.state.3, . ~ . - Population)
summary(MLR.fit.state.4)

#- AIC -#
MLR.fit.state <- lm(Life.Exp ~ ., data=statedata)
step(MLR.fit.state)

#- C_p -#
library(leaps)
MLR.fit.state.Cp <- regsubsets(Life.Exp ~ ., 
       nbest=3, data=statedata)
par(mfrow=c(1,1))
summary(MLR.fit.state.Cp)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(MLR.fit.state.Cp)


#- R^2_a -#
x <- model.matrix(MLR.fit.state)[, -1]
y <- statedata$Life
leaps(x, y, wt=rep(1, NROW(x)), int=TRUE, method=c("adjr2"))

#- Effects of outliers and influential points -#
h <- hat(x)
names(h) <- state.abb
Lev <- round(rev(sort(h)), 3)
Lev[1:8]

#- Exclude Alaska -#
MLR.exc.Alaska <- leaps(x[-2,],y[-2],method="adjr2")
mod.sum <- cbind(MLR.exc.Alaska$which,MLR.exc.Alaska$adjr2)
mod.sum[order(MLR.exc.Alaska$adjr2),]

#- Boxplot of predictors -#
par(mfrow=c(3,3))
for(i in 1:8) boxplot(state.x77[,i],
       main=dimnames(state.x77)[[2]][i])

#- Transform skewed variables -#
x.tra <- cbind(log(x[,1]),x[,2],log(x[,3]),
            x[,4:6],log(x[,7]))

par(mfrow=c(3,3))
apply(x.tra,2, boxplot)

#- R^2_a after transformation -#
MLR.trans <- leaps(x.tra, y, method="adjr2")
mod.sum <- cbind(MLR.trans$which,MLR.trans$adjr2)
mod.sum[order(MLR.trans$adjr2, decreasing = TRUE),][1:8,]

#================================================================
#Relative Importance
#The relaimpo package provides measures of relative importance 
#for each of the predictors in the model. See help(calc.relimp) 
#for details on the four measures of relative importance provided. 

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
   rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 1000, type = c("lmg", 
  "last", "first", "pratt"), rank = TRUE, 
  diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 
