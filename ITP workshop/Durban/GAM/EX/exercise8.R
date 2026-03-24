library(mgcv)

data(trees)

#fitting the model with isotropic smoothers
modelA <- gam(Volume~s(Girth, Height, bs="tp", k=5), family=Gamma(link="log"), data=trees)
modelA
plot(modelA, pages=1)

#fitting the model with tensor product smoothers
modelB <- gam(Volume~te(Girth, Height, bs="cr", k=5), family=Gamma(link="log"), data=trees)
modelB
plot(modelB, pages=1)
