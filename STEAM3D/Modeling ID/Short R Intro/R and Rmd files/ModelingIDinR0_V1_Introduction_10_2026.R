#########################################################
#                                                       #
# PRACTICAL SESSION 1                                   #
# MODELING INFECTIOUS DISEAES USING R                   #
# ZIv Shkedy, Rashider Aloni, Leyla Kodalci             #
# Hasselt University, Belgium                           #
# 4th PhD week, Gondar University, Ethiopia             #
# JKUAT, Kenya                                          #
# SUSAN-SSACAB 2019 Conference, South Africa            #
# MID course, Gondar University, Ethiopia               #
# STEAM-3D , Cape Town, 2026                            #
#########################################################



#########################################################
#                                                       #
#   PART 1: objects                                     #
#                                                       #
#########################################################


x<-c(1.9,1.2,0.7,2.7,1.2,3.1,2.3,2.1,2.1,1.4)
x
y<-c(1.8,1.1,0.6,2.7,1.4,3.0,2.5,1.9,1.8,1.4)
y

#########################################################
#                                                       #
#   PART 2: calculate the mean                          #
#                                                       #
#########################################################


mean(x)
mean(y)

#########################################################
#                                                       #
#   PART 3: correlation and plot                        #
#                                                       #
#########################################################

cor(x,y)
plot(x,y)

#########################################################
#                                                       #
#   PART 4a: run in a function                          #
#                                                       #
#########################################################



discript<-function(x,y)
{
print(mean(x))
print(mean(y))
print(cor(x,y))
plot(x,y)
}

#########################################################
#                                                       #
#   PART 4b: apply to the data                          #
#                                                       #
#########################################################

discript(x,y)


#########################################################
#                                                       #
#   PART 5: apply to the cars data                      #
#                                                       #
#########################################################


help(cars)
head(cars)
discript(cars[,1],cars[,2])

