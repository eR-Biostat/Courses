#########################################################
#                                                       #
# PRACTICAL SESSION: Vaccination                        #
# MODELING INFECTIOUS DISEAES USING R                   #
# ZIv Shkedy (Hasselt University, Belgium)              #
# 4th PhD week, Gondar University, Ethiopia             #
# JKUAT, Kenya                                          #
# SUSAN-SSACAB 2019 Conference, South Africa            #
#                                                       #
#                                                       #
#########################################################


# SETP 1: install the deSolve package           
library(deSolve)

#########################################################
#                                                       #
#                                                       #
#   SIR with vaccination                                #
#   N=5000                                              #
#                                                       #
#########################################################

library(deSolve)
require(deSolve)

parameters <- c(mu=1/75,beta=0.001/2, v=1, P=0.0)
parameters

state <- c(X=4999,Y=1,Z=0)
state

SIR<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dX <- 5000*mu*(1-P)-beta*Y*X - mu*X
dY <- beta*Y*X - v*Y - mu*Y
dZ <- v*Y -mu*Z+5000*mu*P
list(c(dX, dY, dZ))
}) 
}

times<-seq(0,400,by=0.01)

require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))
head(out)



par(mfrow=c(2,2))
plot (times,out$X ,type="l",main="S", xlab="time", ylab="-")
plot (times,out$Y ,type="l",main="I", xlab="time", ylab="-")
plot (times,out$Z ,type="l",main="R", xlab="time", ylab="-")
plot(out$X,out$Y,type="l")
mtext(outer=TRUE,side=3,"SIR model",cex=1.5)



par(mfrow=c(2,1))
plot (times,out$X ,type="l",main="S", xlab="time", ylab="-")
plot (times,out$Y ,type="l",main="I", xlab="time", ylab="-")
mtext(outer=TRUE,side=3,"SIR model",cex=1.5)

par(mfrow=c(1,1))
plot(out$X,out$Y,type="l",xlab="S",ylab="I")


############################################
### P=40%                                 ##
############################################

parameters <- c(mu=1/75,beta=0.001/2, v=1, P=0.40)
parameters


state <- c(X=4999,Y=1,Z=0)
state

require(deSolve)
outp40 <- as.data.frame(ode(y=state,times=times,func=SIR,parms=parameters))
head(out)
par(mfrow=c(2,2))
plot (times,outp40$X ,type="l",main="S", xlab="time", ylab="-")
lines(times,out$X,col=2)
plot (times,outp40$Y ,type="l",main="I", xlab="time", ylab="-")
lines(times,out$Y,col=2)
plot (times,outp40$Z ,type="l",main="R", xlab="time", ylab="-")
lines(times,out$Z,col=2)


plot(outp40$X,outp40$Y,type="l")
lines(out$X,out$Y,col=2)
mtext(outer=TRUE,side=3,"SIR model",cex=1.5)




