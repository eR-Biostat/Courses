
#########################################################
#                                                       #
#                                                       #
# PRACTICAL SESSION 3                                   #
# MODELING INFECTIOUS DISEAES USING R                   #
# ZIv Shkedy (Hasselt University, Belgium)              #
# 4th PhD week, Gondar University, Ethiopia             #
# JKUAT, Kenya                                          #
# SUSAN-SSACAB 2019 Conference, South Africa            #
#                                                       #
#                                                       #
#########################################################


library(deSolve)


#########################################################
## Example I two interacting pop                       ##
## model: capasso                                      ##
## two interacting populations : diagonal transmission ##
## gonorrhea model                                     ## 
#########################################################

parameters <- c(beta1=0.000003,beta2=0.000006,v1=0.007,v2=0.05,N1=10000,N2=15000)
state <- c(Y1=1,Y2=0)
state
times<-seq(0,3000,by=0.1)
Gonorrhea<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dY1 <- beta1*(N1-Y1)*Y2-v1*Y1
dY2 <- beta2*(N2-Y2)*Y1-v2*Y2
list(c(dY1,dY2))
}) 
}

require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=Gonorrhea,parms=parameters))


par(mfrow=c(1,1))
plot(times,out$Y1 ,type="l",main="X", xlab="time",ylab="-",
      ylim=c(0,max(c(out$Y1,out$Y2))))
lines(times,out$Y2,lty=2)
legend(2000,2000,c("Male","Female"),lty=c(1,2))


out1<-out

parameters <- c(beta1=0.000003*5,beta2=0.000006,v1=0.007,v2=0.05,N1=10000,N2=15000)
state <- c(Y1=1,Y2=0)
times<-seq(0,3000,by=0.1)

require(deSolve)
out2 <- as.data.frame(ode(y=state,times=times,func=Gonorrhea,parms=parameters))


par(mfrow=c(1,1))
plot(times,out1$Y1 ,type="l",main="Female", xlab="time",ylab="-",
      ylim=c(0,max(c(out1$Y1,out2$Y1))))
lines(times,out2$Y1,lty=2)
legend(2000,2000,c("beta_1","beta_1*5"),lty=c(1,2))



par(mfrow=c(1,1))
plot(times,out1$Y2 ,type="l",main="Male", xlab="time",ylab="-",
      ylim=c(0,max(c(out1$Y2,out2$Y2))))
lines(times,out2$Y2,lty=2)
legend(2000,2000,c("beta_1","Beta_1*5"),lty=c(1,2))





#########################################################
## Example I two interacting pop                       ##
## model: capasso                                      ##
## two interacting populations : diagonal transmission ##
## gonorrhea model                                     ## 
#########################################################

parameters <- c(beta1=0.000003,beta2=0.000006,v1=0.007,v2=0.05,N1=10000,N2=15000)
state <- c(Y1=1,Y2=0)
Gonorrhea<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dY1 <- beta1*(N1-Y1)*Y2-v1*Y1
dY2 <- beta2*(N2-Y2)*Y1-v2*Y2
list(c(dY1,dY2))
}) 
}
times<-seq(0,3000,by=0.1)

beta2.i<-0.000006*c(0.25,0.5,1)
beta2.i
y1mat<-y2mat<-matrix(0,length(times),length(beta2.i))
for(i in 1:length(beta2.i))
{
parameters <- c(beta1=0.000003,beta2=beta2.i[i],v1=0.007,v2=0.05,N1=10000,N2=15000)
require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=Gonorrhea,parms=parameters))
y1mat[,i]<-out$Y1
y2mat[,i]<-out$Y2
}
lty.i<-c(1,2,4)


#########################################################
## THE FIGURE FOR THE BOOK                             ##
#########################################################



par(mfrow=c(1,2))
plot(times,y1mat[,1],type="l",ylim=c(0,8000),ylab="I_female")
for(i in 2:length(beta2.i))
{
lines(times,y1mat[,i],lty=lty.i[i])
}
title("female")

plot(times,y2mat[,1],type="l",ylim=c(0,8000),ylab="I_male")
for(i in 2:length(beta2.i))
{
lines(times,y2mat[,i],lty=lty.i[i])
}
title("male")
text(2500,y2mat[,1][30001]*1.1,"R0_m=0.45",cex=0.75)
text(2500,y2mat[,2][30001]*1.05,"R0_m=0.90",cex=0.75)
text(2500,y2mat[,3][30001]*1.025,"R0_m=1.80",cex=0.75)




###########################################################
# r01 and r01*r02                                         #
###########################################################

N1<-10000
N2<-15000
beta1<-0.000003
beta2.i<-0.000006*c(0.25,0.5,1)
v2<-0.05
v1<-0.007
R02<-c(1:length(beta2.i))
R01<-N1*beta1/v1
for(i in 1:length(beta2.i))
{
R02[i]<-N2*beta2.i[i]/v2
}
data.frame(R02,R01*R02)


###########################################################
# extra figures                                           #
###########################################################

plot(R02,R01*R02)

###########################################################
# same plot at matlab (not for the book)                  #
###########################################################

par(mfrow=c(1,1))
plot(times,out$Y1 ,type="l",main="X", xlab="time",ylab="-",
      ylim=c(0,max(c(out$Y1,out$Y2))))
lines(times,out$Y2,col=2)


#########################################################
## Example II two interacting pop                      ##
## model: capasso                                      ##
#########################################################


#parameters <- c(beta11=0.05,beta12=0.075,beta21=0.075,beta22=0.05,v1=1/30,v2=1/30,mu=0.001) #basic setting
#parameters <- c(beta11=0.05,beta12=0.05,beta21=0.075,beta22=0.05,v1=1/30,v2=1/30,mu=0.001)  #beta12=0.05 
parameters <- c(beta11=0.05,beta12=0.0,beta21=0.075,beta22=0.05,v1=1/30,v2=1/30,mu=0.001)  #beta12=0.00 

state <- c(Y1=0.8,Y2=0.2,Y3=0,Y4=0.8,Y5=0.2,Y6=0)
state
times<-seq(0,4000,by=0.01)


SIRtwo<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dY1 <- -(beta11*Y2+beta12*Y5)*Y1+mu-mu*Y1
dY2 <- (beta11*Y2+beta12*Y5)*Y1-v1*Y2-mu*Y2
dY3 <- v1*Y2 - mu*Y3
dY4 <- -(beta21*Y2+beta22*Y5)*Y4+mu-mu*Y4
dY5 <-  (beta21*Y2+beta22*Y5)*Y4-v2*Y5-mu*Y5
dY6 <- v2*Y5-mu*Y6
list(c(dY1,dY2,dY3,dY4,dY5,dY6))
}) 
}

times<-seq(0,4000,by=0.01)
require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=SIRtwo,parms=parameters))
head(out)
out1<-out
out2<-out


#outbeta120=out # beta12=0
#outbeta12025=out
#outbeta1205=out  #beta12=0.05
#outbeta12075=out #beta12=0.075 (basic setting (beta12=0.075))



### setting with beta_11=0 ##
plot(times,out1$Y1,type="l")
lines(times,out2$Y1,lty=2,col=2)

plot(times,out1$Y4,type="l")
lines(times,out2$Y4,lty=2,col=2)

## EXample 1: beta12=0.075,0.05 ##

plot(times,outbeta12075$Y1,type="l")
lines(times,outbeta1205$Y1,lty=2,col=2)
legend(1000,0.8,c("beta_12=0.075","beta_12=0.05"),lty=c(1,2),col=c(1,2))
title("susceptible in population 1")

plot(times,outbeta12075$Y4,type="l")
lines(times,outbeta1205$Y4,lty=2,col=2)
legend(1000,0.8,c("beta_12=0.075","beta_12=0.05"),lty=c(1,2),col=c(1,2))
title("susceptible in population 2")


## EXample 2: beta12=0.00,0.075 ##


plot(times,outbeta12075$Y1,type="l",ylim=c(0,1))
lines(times,outbeta120$Y1,lty=2,col=2)
legend(0,0.95,c("beta_12=0.075","beta_12=0.05"),lty=c(1,2),col=c(1,2))
title("susceptible in population 1")



plot(times,outbeta12075$Y4,type="l",ylim=c(0,1))
lines(times,outbeta120$Y4,lty=2,col=2)
legend(0,0.95,c("beta_12=0.075","beta_12=0.05"),lty=c(1,2),col=c(1,2))
title("susceptible in population 2")



#########################################################
## Example III age structure                           ##
## model: capasso                                      ##
#########################################################


#0.0002*25000/(1/91+1/75)
#parameters <- c(mu=1/75,beta=0.0002, v=4,N=25000)
#parameters <- c(mu=1/75,beta=0.0001, v=4,N=25000)
#parameters <- c(mu=1/75,beta=0.0000025, v=121.67,N=50000)
#parameters
#parameters <- c(mu=1/75000,beta=0.00121, v=121,N=500000)

#3*0.000121*500000

#state <- c(X=499900,Y=10,Z=0)
#state <- c(X=24999,Y=1,Z=0)

#SIR<-function(t,state,parameters)
#{
#with(as.list(c(state, parameters)),
#{
#dX <- N*mu-beta*Y*X - mu*X
#dY <- beta*Y*X - v*Y - mu*Y
#dZ <- v*Y -mu*Z
#list(c(dX, dY, dZ))
#}) 
#}
#times<-seq(0,800,by=0.1)
#require(deSolve)
#out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))
#head(out)

#################################################
## THE FIGURE FOR THE BOOK                     ##
#################################################


#par(mfrow=c(2,1))
#plot(times,out$X ,type="l",main=" ", xlab="time", ylab="susceptible")
#plot(times,out$Y ,type="l",main=" ", xlab="time", ylab="susceptible")


#########################################################
## Example III age structure                           ##
## model: capasso                                      ##
#########################################################



0.0005*266665/(4+1/75)/20
0.0005*733334/(4+1/75)/55

N<-1000000
(20/75)*N
N-(20/75)*N

(20/75)*1000000

26665+1+73333

## Example (1) ###

parameters <- c(beta11=0.0001,beta12=0.0000075,beta21=0.0000075,beta22=0.0001,
                v1=4,v2=4,mu=1/75,mu2=1/20,N=1000000)
parameters

## Example (2) ###

parameters <- c(beta11=0.000001,beta12=0.0000075,beta21=0.0000075,beta22=0.0001,
                v1=4,v2=4,mu=1/75,mu2=1/20,N=1000000)
parameters


state <- c(Y1=266665,Y2=1,Y3=0,Y4=733334,Y5=0.0,Y6=0)
state

SIRtwo<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dY1 <- -(beta11*Y2+beta12*Y5)*Y1+N*mu-mu*Y1-mu2*Y1
dY2 <- (beta11*Y2+beta12*Y5)*Y1-v1*Y2-mu*Y2-mu2*Y2
dY3 <- v1*Y2 - mu*Y3-mu2*Y3
dY4 <- -(beta21*Y2+beta22*Y5)*Y4-mu*Y4+mu2*Y1
dY5 <-  (beta21*Y2+beta22*Y5)*Y4-v2*Y5-mu*Y5+mu2*Y2
dY6 <- v2*Y5-mu*Y6+mu2*Y3
list(c(dY1,dY2,dY3,dY4,dY5,dY6))
}) 
}

times<-seq(0,60,by=0.01)
require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=SIRtwo,parms=parameters))
#out1<-out #Example 1
out05<-out #Example 2


dim(out)[1]
head(out)
((out$Y1[10001]+out$Y4[10001])/N)^(-1)

((out$Y1[10001])/((20/75)*N))
((out$Y4[10001])/(N-((20/75)*N)))

#########################################################
## Example 1 (slide 196-197)                           ##
#########################################################


par(mfrow=c(1,1))
plot(times,out1$Y1+out1$Y4,ylim=c(0,500000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out1$Y1,lty=2)
lines(times,out1$Y4,lty=3)
legend(10,500000,c("total","age group I","age group II"),lty=c(1,2,3))


plot(times,out1$Y2+out1$Y5,ylim=c(0,50000),type="l",main=" ",xlab="time",ylab="I")
lines(times,out1$Y2,lty=2)
lines(times,out1$Y5,lty=3)
legend(10,50000,c("total","age group I","age group II"),lty=c(1,2,3))


#########################################################
## Example 2 (198-199)                                 ##
#########################################################


par(mfrow=c(1,1))
plot(times,out05$Y1+out05$Y4,ylim=c(0,500000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out05$Y1,lty=2)
lines(times,out05$Y4,lty=3)
legend(10,500000,c("total","age group I","age group II"),lty=c(1,2,3))


plot(times,out05$Y2+out05$Y5,ylim=c(0,50000),type="l",main=" ",xlab="time",ylab="I")
lines(times,out05$Y2,lty=2)
lines(times,out05$Y5,lty=3)
legend(10,50000,c("total","age group I","age group II"),lty=c(1,2,3))



#########################################################
## Example 1 Vs. Example 2                             ##
#########################################################


par(mfrow=c(1,1))
plot(times,out1$Y1+out1$Y4,ylim=c(0,500000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out05$Y1+out05$Y4,lty=2)
legend(0,500000,c("Example 1","Example 2"),lty=c(1,2))



#########################################################
## End of program                                      ##
#########################################################

par(mfrow=c(2,1))
plot(times,out1$Y1,ylim=c(0,500000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out05$Y1,lty=2)
#legend(0,500000,c("total","age group I","age group II"),lty=c(1,2,3))
plot(times,out1$Y4,ylim=c(0,500000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out05$Y4,lty=2)
#legend(0,500000,c("total","age group I","age group II"),lty=c(1,2,3))


par(mfrow=c(2,1))
plot(times,out1$Y5,ylim=c(0,20000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out05$Y5,lty=2)
#legend(0,500000,c("total","age group I","age group II"),lty=c(1,2,3))
plot(times,out1$Y2,ylim=c(0,20000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out05$Y2,lty=2)
#legend(0,500000,c("total","age group I","age group II"),lty=c(1,2,3))


par(mfrow=c(1,1))
plot(times,out$Y1+out$Y4,ylim=c(0,500000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out$Y1,lty=2)
lines(times,out$Y4,lty=3)
#legend(0,500000,c("total","age group I","age group II"),lty=c(1,2,3))


plot(times,out$Y2+out$Y5,ylim=c(0,50000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out$Y2,lty=2)
lines(times,out$Y5,lty=3)
#legend(0,50000,c("total","age group I","age group II"),lty=c(1,2,3))


plot(out$Y1,out$Y2,type="l")
lines(out$Y4,out$Y5,lty=2)


plot(times,out$Y3+out$Y6,ylim=c(0,2000000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out$Y3,lty=2)
lines(times,out$Y6,lty=3)



plot(times,out$Y2+out$Y5+out$Y1+out$Y4+out$Y3+out$Y6,ylim=c(0,2000000),type="l",main=" ",xlab="time",ylab="S")
lines(times,out$Y2,lty=2)
lines(times,out$Y5,lty=3)









