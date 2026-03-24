library(mgcv)

data(trees)

#fitting the model
modelA <- gam(Volume~s(Girth, bs="cr") + s(Height, bs="cr"), family=Gamma(link="log"), data=trees)

#checking the model and the output
modelA
plot(modelA, pages=1)

#increasing the number of knots
modelB <- gam(Volume~s(Girth, bs="cr", k=20) + s(Height, bs="cr", k=20), family=Gamma(link="log"), data=trees)
modelB
plot(modelB, pages=1)

#fitting the model with p-splines
modelC <- gam(Volume~s(Girth, bs="ps") + s(Height, bs="ps"), family=Gamma(link="log"), data=trees)

#checking the model and the output
modelC
plot(modelC, pages=1)
