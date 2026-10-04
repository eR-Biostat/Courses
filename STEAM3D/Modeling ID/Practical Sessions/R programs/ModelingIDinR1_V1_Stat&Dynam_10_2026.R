#########################################################
#                                                       #
# PRACTICAL SESSION 1                                   #
# MODELING INFECTIOUS DISEAES USING R                   #
# ZIv Shkedy, Rahider Aloni, Leyla Kodalci              #
# Hasselt University, Belgium                           #
# 4th PhD week, Gondar University, Ethiopia             #
# JKUAT, Kenya                                          #
# SUSAN-SSACAB 2019 Conference, South Africa            #
# MID course, Gondar University, Ethiopia               #
# STEAM-3D, Cape Town, South AFrica, 2026               #
#                                                       #
#########################################################


# SETP 1: install the deSolve package           
library(deSolve)


#########################################################
#                                                       #
#                                                       #
#   PART 1: STATIC MODEL (Closed Population)            #
#                                                       #
#                                                       #
#########################################################

######################################
### SIR with lambda =0.2 D=10 days ###
######################################

parameters <- c(lambda = 0.2, v=36.5)
parameters

state <- c(X=4999,Y=1,Z=0)
state

SIR<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dX <- -lambda*X 
dY <- lambda*X - v*Y
dZ <- v*Y
list(c(dX, dY, dZ))
}) 
}


times<-seq(0,40,by=0.01)
times


require(deSolve)

out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))

head(out)


par(mfrow=c(1,2), oma=c(0,0,3,0))
plot (times,out$X ,type="l",main="S and R", xlab="age", ylab="-",lwd=2)
lines(times,out$Z,col=3,lwd=2)
plot (times,out$Y ,type="l",main="I", xlab="time", ylab="-",lwd=2)
mtext(outer=TRUE,side=3,"SIR model, D=10 days",cex=1.5)



#########################################
### SIR with lambda =0.2 D=two months ###
#########################################

60/365
1/(60/365)

parameters <- c(lambda = 0.2, v=6.083333)
parameters
state <- c(X=4999,Y=1,Z=0)
state


require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))
head(out)


par(mfrow=c(1,2), oma=c(0,0,3,0))
plot (times,out$X ,type="l",main="S and R", xlab="age", ylab="-",lwd=2)
lines(times,out$Z,col=3,lwd=2)
plot (times,out$Y ,type="l",main="Y", xlab="time", ylab="-",lwd=2)
mtext(outer=TRUE,side=3,"SIR model D=two months", cex=1.5)


#########################################
## lambda=Y*beta                       ##
#########################################


parameters <- c(beta=0.0085, v=36.5)
state <- c(X=4999,Y=1,Z=0)
parameters
state

SIR<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dX <- -beta*Y*X 
dY <- beta*Y*X - v*Y
dZ <- v*Y
list(c(dX, dY, dZ))
}) 
}
times<-seq(0,10,by=0.01)
require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))
head(out)
#out
par(mfrow=c(2,2), oma=c(0,0,3,0))
plot (times,out$X ,type="l",main="X", xlab="time", ylab="-")
plot (times,out$Y ,type="l",main="Y", xlab="time", ylab="-")
plot (times,out$Z ,type="l",main="Y", xlab="time", ylab="-")
mtext(outer=TRUE,side=3,"SIR model, lambda=beta*I",cex=1.5)






##########################################
###                                    ###
###                                    ###
### PART 2: DYNAMIC MODEL (OPEN POP.)  ###
###                                    ###
###                                    ###
##########################################

# SETP 1: install the deSolve package           
library(deSolve)

parameters <- c(mu=1/75,beta=0.001,v=1)

print(parameters)

state <- c(X=4999,Y=1,Z=0)
print(state)

times<-seq(0,400,by=0.01)
print(times)

p<-0.0
N<-5000

state <- c(X=N-1,Y=1,Z=0)
print(state)

SIR<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dX <- N*mu*(1-p)-beta*Y*X - mu*X
dY <- beta*Y*X - v*Y - mu*Y
dZ <- v*Y -mu*Z+N*mu*p
list(c(dX, dY, dZ))
}) 
}

require(deSolve)

########################
##    basic setting, beta=0.001   ##
########################


out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))

head(out)

outp02<-out #beta=0.001#

par(mfrow=c(2,2))
plot(times, out$X, type="l", main="S",xlab="time", ylab="Number of susceptible")
plot(times, out$Y, type="l", main="I",xlab="time", ylab="Number of infected")
plot(times, out$Z, type="l", main="R",xlab="time", ylab="Number of recovered")
plot(out$X, out$Y, type="l",main="Equilibrium", xlab="S", ylab="I")
mtext(outer=TRUE, side=3, "SIR model", cex=1.5)



########################
## beta=0.00005       ##
########################



parameters <- c(mu=1/75,beta=0.001,v=1)
parameters <- c(mu=1/75,beta=0.0005,v=1)
parameters
out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))

#outp02<-out #beta=0.001#
outp04<-out #beta=0.0005#

par(mfrow=c(1,1))
plot(times,outp02$X,type="l",main="S", xlab="time", ylab="Number of susceptible")
lines(times,outp04$X,col=2)


par(mfrow=c(1,1))
plot(times,outp02$Y,type="l",main="I", xlab="time", ylab="Number of infected")
lines(times,outp04$Y,col=2)


plot(outp02$X, outp02$Y,type="l",main="Effect of transmission rate on equilibrium",
     xlab="Susceptible (S)",
     ylab="Infected (I)")
lines(outp04$X, outp04$Y, col=2)

########################
## N=2500             ##
########################


parameters <- c(mu=1/75,beta=0.001,v=1)
print(parameters)
state <- c(X=4999,Y=1,Z=0)
times<-seq(0,400,by=0.01)

p<-0.0
N<-5000
parameters <- c(mu=1/75,beta=0.001,v=1)
out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))

outp02<-out #N=5000#

N<-2500
state <- c(X=N-1,Y=1,Z=0)
parameters <- c(mu=1/75,beta=0.001,v=1)
out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))
outp04<-out # N=2500


par(mfrow=c(1,1))
plot(times,outp02$X,type="l",main="S", xlab="time", ylab="Number of susceptible")
lines(times,outp04$X,col=2)


par(mfrow=c(1,1))
plot(times,outp02$Y,type="l",main="I",xlab="time", ylab="Number of infected")
lines(times,outp04$Y,col=2)

plot(outp02$X,outp02$Y,type="l",main="Effect of population size on equilibrium",xlab="S",ylab="I")
lines(outp04$X,outp04$Y,col=2)
