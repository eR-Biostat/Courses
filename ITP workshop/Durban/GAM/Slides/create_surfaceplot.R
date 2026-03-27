library(mgcv)

data(trees)

#fitting the model with p-splines
modelC <- gam(Volume~s(Girth, bs="ps") + s(Height, bs="ps"), family=Gamma(link="log"), data=trees)
newData <- data.frame("Girth"=rep(seq(min(trees$Girth),max(trees$Girth),length.out=25),each=25), "Height"=rep(seq(min(trees$Height),max(trees$Height),length.out=25),25))
newData$results <- predict.gam(modelC, newData, type="response")
plot_ly(z = ~xtabs(results ~ Girth + Height, data = newData)) %>% add_surface()
