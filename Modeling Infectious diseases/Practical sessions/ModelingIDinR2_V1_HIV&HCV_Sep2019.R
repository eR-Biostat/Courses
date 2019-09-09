#########################################################
#                                                       #
#                                                       #
# PRACTICAL SESSION 2                                   #
# MODELING INFECTIOUS DISEAES USING R                   #
# ZIv Shkedy (Hasselt University, Belgium)              #
# 4th PhD week, Gondar University, Ethiopia             #
# JKUAT, Kenya                                          #
# SUSAN-SSACAB 2019 Conference, South Africa            #
#                                                       #
#                                                       #
#########################################################



library(deSolve)

#################################################
## EXAMPLE:AIDS                                ##
## model: anderson & may , capasso             ##
#################################################

parameters <- c(B=10000,mu=1/75,v1=1/8,v2=1/8,f=0.2,alpha=1)
state <- c(y1=9995,y2=5,y3=0,y4=0,y5=0)
times<-seq(0,20,by=0.01)
parameters
state
times

AIDS<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dy1 = B*mu-(mu+((y2+y3)/10000))*y1
dy2 = f*((y2+y3)/10000)*y1-(mu+v1)*y2
dy3 = (1-f)*((y2+y3)/10000)*y1-(mu+v2)*y3
dy4 = v1*y2-(mu+alpha)*y4
dy5 = v2*y3-mu*y5
list(c(dy1,dy2,dy3,dy4,dy5))
}) 
}

args(AIDS)

require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=AIDS,parms=parameters))
head(out)


#################################################
## THE FIGURE FOR THE BOOK                     ##
#################################################


par(mfrow=c(1,1))
plot(times,out$y2+out$y3+out$y4+out$y5,type="l",main=" ", xlab="time", ylab="-")
lines(times,out$y4,lty=2)
lines(times,out$y5,lty=4)
legend(0,8500,c("Infected","Clinical AIDS (A)","Infected not clinical (Z)"),lty=c(1,2,4))


plot(times,out$y2+out$y3,type="l",main="X", xlab="time", ylab="-",ylim=c(0,10000))

plot(times,((out$y2+out$y3)/10000)*out$y1,type="l",main="X", xlab="time", ylab="-",ylim=c(0,3000))
lines(times,((out$y2+out$y3)/10000)*out$y1,col=2,lwd=3)


#################################################
## EXTRA FIGURE             #####################
#################################################

par(mfrow=c(1,2))
plot(times,out$y2+out$y3+out$y4+out$y5,type="l",main="X", xlab="time", ylab="-",ylim=c(0,10000))
lines(times,out$y5 ,type="l",main="a", xlab="time", ylab="-")
plot(times,out$y4,type="l",main="b", xlab="time", ylab="-")

mtext(outer=TRUE,side=3,"Extra figure",cex=1.5)


#################################################
##                                             ##
##                                             ##
## EXAMPLE: HCV in IDU population              ##
##                                             ##
##                                             ##
#################################################

parameters <- c(B=0.00,mu=0.00,k=15,ba=0.3,bc=0.05,sigma1=5,sigma2=0.25,rho=0.7)
parameters
state <- c(y1=0.99,y2=0.01,y3=0,y4=0)
state
times<-seq(0,20,by=0.001)
times

HCVIDU<-function(t,state,parameters)
{
with(as.list(c(state, parameters)),
{
dy1 = B-k*ba*y1*y2-k*bc*y3*y1-mu*y1
dy2 = k*ba*y1*y2+k*bc*y3*y1-sigma1*y2-mu*y2
dy3 = rho*sigma1*y2-sigma2*y3-mu*y3
dy4= (1-rho)*sigma1*y2+sigma2*y3-mu*y4;
list(c(dy1,dy2,dy3,dy4))
}) 
}

require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=HCVIDU,parms=parameters))
head(out)
out15 <-out

parameters <- c(B=0.00,mu=0.00,k=25,ba=0.3,bc=0.05,sigma1=5,sigma2=0.25,rho=0.7)
parameters
require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=HCVIDU,parms=parameters))
head(out)
out25 <-out


par(mfrow=c(1,1))
plot(times,out15$y1,type="l",main=" ",xlab="exposure time", ylab="-",ylim=c(0,1))
lines(times,out15$y2,col=2)
lines(times,out15$y3,col=3)
lines(times,out15$y4,col=4)


#################################################
# k=25                                          #
#################################################


parameters <- c(B=0.00,mu=0.00,k=25,ba=0.3,bc=0.05,sigma1=5,sigma2=0.25,rho=0.7)
parameters
require(deSolve)
out <- as.data.frame(ode(y=state,times=times,func=HCVIDU,parms=parameters))
head(out)
out25 <-out


par(mfrow=c(1,1))
plot(times,out25$y1,type="l",main=" ",xlab="exposure time", ylab="-",ylim=c(0,1))
lines(times,out25$y2,col=2)
lines(times,out25$y3,col=3)
lines(times,out25$y4,col=4)

par(mfrow=c(1,1))
plot(times,out15$y1,type="l",main=" ",xlab="exposure time", ylab="-",ylim=c(0,1))
lines(times,out25$y1,col=2)

plot(times,out15$y2,type="l",main=" ",xlab="exposure time", ylab="-",ylim=c(0,1))
lines(times,out25$y2,col=2)

plot(times,out15$y3,type="l",main=" ",xlab="exposure time", ylab="-",ylim=c(0,1))
lines(times,out25$y3,col=2)

plot(times,out15$y4,type="l",main=" ",xlab="exposure time", ylab="-",ylim=c(0,1))
lines(times,out25$y4,col=2)

plot(times,1-out15$y1,type="l",main=" ",xlab="exposure time", ylab="-",ylim=c(0,1))
lines(times,1-out25$y1,col=2)

