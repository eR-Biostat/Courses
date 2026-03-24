library(MASS)
data(whiteside)


#PLOT
par(mfrow=c(1,2))
plot(whiteside$Temp[whiteside$Insul=="Before"], whiteside$Gas[whiteside$Insul=="Before"], xlab="Temperature", ylab="Gas", pch=16, main="Before Insulation", ylim=c(0,8))
plot(whiteside$Temp[whiteside$Insul=="After"], whiteside$Gas[whiteside$Insul=="After"], xlab="Temperature", ylab="Gas", pch=16, main="After Insulation", ylim=c(0,8))

#LINEAR MODEL
modelExercise1 <- lm(Gas~Temp + Insul + Insul*Temp, data=whiteside)

#ASSUMPTIONS
plot(modelExercise1)

#CONFIDENCE INTERVAL
predExercise1 <- predict(modelExercise1, interval="confidence", level=0.95)

plot(whiteside$Temp[whiteside$Insul=="Before"], whiteside$Gas[whiteside$Insul=="Before"], xlab="Temperature", ylab="Gas", pch=16, main="Before Insulation", ylim=c(0,8))
lines(whiteside$Temp[whiteside$Insul=="Before"],predExercise1[whiteside$Insul=="Before",1], lwd=2, col=2)
lines(whiteside$Temp[whiteside$Insul=="Before"],predExercise1[whiteside$Insul=="Before",2], lty=2, col=2)
lines(whiteside$Temp[whiteside$Insul=="Before"],predExercise1[whiteside$Insul=="Before",3], lty=2, col=2)

plot(whiteside$Temp[whiteside$Insul=="After"], whiteside$Gas[whiteside$Insul=="After"], xlab="Temperature", ylab="Gas", pch=16, main="After Insulation", ylim=c(0,8))
lines(whiteside$Temp[whiteside$Insul=="After"],predExercise1[whiteside$Insul=="After",1], lwd=2, col=2)
lines(whiteside$Temp[whiteside$Insul=="After"],predExercise1[whiteside$Insul=="After",2], lty=2, col=2)
lines(whiteside$Temp[whiteside$Insul=="After"],predExercise1[whiteside$Insul=="After",3], lty=2, col=2)

