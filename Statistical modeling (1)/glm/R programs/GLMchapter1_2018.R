################################################################
# >eR-BioStat                                                  #
#                                                              #
# GLM                                                          # 
# CAHPTER 1                                                    #
# ANOVA and Linear regression                                  #
#                                                              #
# 2018                                                         #
# Ziv Shkedy &  Fetene Tekle                                   #
################################################################

####################################################
# Example 1: pharmacutical exp.                    #
####################################################

dist<-c(186.6145,103.3529,191.3850,334.9845,89.2831,
        345.5070,169.5161,173.1491,130.9634,363.4392,
        76.5340,202.1145,12.8458,44.3092,41.3581,24.5560,
        61.5525,38.8464,27.0107,45.9960,13.7927,42.4009,17.5861,
        11.7937)
gr<-c(rep(0,12),rep(1,12))
boxplot(split(dist,gr))
tapply(dist,as.factor(gr),mean)
tapply(dist,as.factor(gr),median)
stripchart(dist ~ gr, pch=1, xlab="Response Level", col=4)#ger
fit.2<-aov(dist~as.factor(gr))
summary(fit.2)# Anlysis of example 1#

##########################
#simple example          #
##########################

resp<-c(2,3,4,5,6,7,1,2,3)
gr<-c(1,1,1,2,2,2,3,3,3)
data.frame(resp,gr)

plot(gr,resp)
fit.1<-aov(resp~as.factor(gr))
summary(fit.1)

x<-seq(from=-3,to=6,length=1000)
dx1<-dnorm(x,0,1)
dx2<-dnorm(x,2,1)
plot(x,dx1,type="l",xaxt="n",xlab=" ",ylab=" ",yaxt="n")
lines(x,dx2,lty=1)
####################################################
# Example 2:  phosphate concentraion               #
####################################################

con<-c(2.3,4.1,4.2,4.0,4.6,4.6,3.8,5.2,3.1,3.7,3.8,
       3.0,4.1,3.9,3.1,3.3,2.9,3.3,3.9,
       3.0,2.6,3.1,2.2,2.1,2.4,2.8,3.5,2.9,2.6,3.1,3.2)
gr<-c(rep(1,11),rep(2,8),rep(3,12))
plot(gr,con)
boxplot(split(con,gr))

fit.1<-aov(con~as.factor(gr))
summary(fit.1)

x<-seq(from=-3,to=6,length=1000)
dx1<-dnorm(x,0,1)
dx2<-dnorm(x,2,1)
dx3<-dnorm(x,3,1)
plot(x,dx1,type="l",xaxt="n",xlab=" ",ylab=" ",yaxt="n")
lines(x,dx2,lty=1)
lines(x,dx3,lty=1)


#########################################################
# END OF CHAPTER 1                                      #
#########################################################
