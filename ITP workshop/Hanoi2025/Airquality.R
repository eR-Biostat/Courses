wind<-airquality$Wind
M.wind<-mean(wind)
SD.wind<-sqrt(var(wind))
M.wind
SD.wind
wind=na.omit(airquality$Wind)
library(TeachingDemos)
z.test(wind,sd=SD.wind)
z.test(wind, SD.wind, mu=9)