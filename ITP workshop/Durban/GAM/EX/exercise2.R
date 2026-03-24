library(MASS)
data(Rubber)

#PLOT
plot(Rubber$tens, Rubber$loss, pch=16, xlab="Tensile Strength", ylab="Tyre wear")

#model
modelExercise2A <- lm(loss~tens, data=Rubber)
plot(modelExercise2A)

modelExercise2B <- lm(loss~poly(tens,degree=2), data=Rubber)
plot(modelExercise2B)

modelExercise2C <- lm(loss~poly(tens,degree=3), data=Rubber)
plot(modelExercise2C)

#
plot(Rubber$tens, Rubber$loss, pch=16, xlab="Tensile Strength", ylab="Tyre wear")
