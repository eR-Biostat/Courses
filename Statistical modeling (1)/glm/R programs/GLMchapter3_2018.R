################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 3                                                    #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################

#################Example 2: Beetle mortality######
beetle<-read.table("C:/projects/GLM/data4glm/beetle.txt", header = TRUE)
attach(beetle)
dim(beetle)

p<-killed/beetles
unkilled<-beetles-killed
Proportionkilled<-p
par(mfrow=c(1,1))
plot(Dose,Proportionkilled, main="Proportion of the killed beetles")

################Example 3: stress data##############

stress <- read.table("C:/projects/GLM/data4glm/stress.txt", sep=",",header=TRUE)
attach(stress)
names(stress) 

respGLM <- glm(respondents ~ month, family=poisson, data=stress)
summary(respGLM)

plot(respondents ~ month, xlab = "Months",ylab = "Subjects", xlim=c(0,20), ylim=c(0,20))






################Example 4: number of deaths##############


age<-c(32,37,42,47,52,57,62,67)
deaths<-c(1,5,5,12,25,38,54,65)
pop<-c(17742,16554,16059,13083,10784,9645,10706,9933)
data.frame(age,deaths,pop,(deaths/pop)*100000)

plot(age,log((deaths/pop)*100000))


#########################################################
# END OF CHAPTER 3                                      #
#########################################################




