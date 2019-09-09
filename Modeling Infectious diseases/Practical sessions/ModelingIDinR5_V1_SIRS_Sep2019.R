#########################################################
#                                                       #
#                                                       #
# PRACTICAL SESSION 1                                   #
# MODELING INFECTIOUS DISEAES USING R                   #
# ZIv Shkedy (Hasselt University, Belgium)              #
# 4th PhD week, Gondar University, Ethiopia             #
# JKUAT, Kenya                                          #
# SUSAN-SSACAB 2019 Conference, South Africa            #
#                                                       #
#                                                       #
#########################################################



##########################################
## model: SIRS Capasso page 9 P=0       ##
##########################################

parameters <- c(mu=1/75,beta=0.001,v=1,alpha=0.01)
times<-seq(0,400,by=0.01)
p<-0.0
N<-5000
state <- c(X=N-1,Y=1,Z=0)
SIR<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dX <- N*mu*(1-p)-beta*Y*X - mu*X+alpha*Z
dY <- beta*Y*X - v*Y - mu*Y
dZ <- v*Y -mu*Z+N*mu*p-alpha*Z
list(c(dX, dY, dZ))
}) 
}

require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))
#outp0a0<-out  # alpha=0
outp0a1<-out  # alpha=0.01
#outp0a2<-out


## basic setting ##

par(mfrow=c(2,1))
plot(times,outp0a0$X,type="l",main="S", xlab="time", ylab="Number of susceptible")
plot(times,outp0a0$Y,type="l",main="I", xlab="time", ylab="Number of susceptible")


par(mfrow=c(1,1))
plot(times,outp0a0$X,type="l",main="S", xlab="time", ylab="Number of susceptible",xlim=c(0,200))
lines(times,outp0a1$X,lty=1,col=2)


plot(times,outp0a0$Y,type="l",main="I", xlab="time", ylab="Number of susceptible",xlim=c(0,200))
lines(times,outp0a1$Y,lty=1,col=2)


plot(outp0a0$X,outp0a0$Y,type="l",xlab="S",ylab="I")
lines(outp0a1$X,outp0a1$Y,lty=1,col=2)


plot(outp0a0$X,outp0a0$Y,type="l",xlab="S",ylab="I",xlim=c(0,2000),ylim=c(0,500))
lines(outp0a1$X,outp0a1$Y,lty=1,col=2)



plot(times,outp0a0$Z,type="l",main="S", xlab="time", ylab="Number of susceptible",ylim=c(0,5000))
lines(times,outp0a1$Z,lty=1,col=2)











#############################################################
## END OF PRACTICAL SESSION                                ##
#############################################################






