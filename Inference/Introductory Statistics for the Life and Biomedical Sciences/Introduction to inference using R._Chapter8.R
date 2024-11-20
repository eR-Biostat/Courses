##########################################################
#                                                        #
#                                                        #
# Introduction to Statistical inference using R          #
#                                                        #
# 2018                                                   #
# Ziv Shkedy                                             #
# >eR-BioStat                                            #
# Chapter 8                                              #
#                                                        #
#                                                        #
##########################################################





###########################################################
################# Chapter 8    ############################
###########################################################


#### Example: point estimation of proportion ##############
#help(HairEyeColor)
malefemale=apply(HairEyeColor,3,sum)
n=sum(malefemale) 
pbar=malefemale[2]/n                   
pbar

#### C.I estimation of proportion ########################

malefemale=apply(HairEyeColor,3,sum)
n=sum(malefemale) 
pbar=malefemale[2]/n       
SE=sqrt(pbar*(1-pbar)/n) 
E=qnorm(0.975)*SE          
pbar+c(-E,E)

#### C.I with prop.test()  ###############################

library(TeachingDemos)
malefemale=apply(HairEyeColor,3,sum)
prop.test(malefemale[2],sum(malefemale), correct=F)



####  Testing of hypotheses for population proportion  ### 
pbar = 0.47
prop = 0.4
n = 100
z=(pbar-prop)/sqrt((prop*(1-prop))/n)
z        
alpha=0.05
crit.point=qnorm(1-alpha)#p=0.05 one tailed (upper)
crit.point


#### p value  ###########################################

pbar = 0.47
prop = 0.4
n = 100
z=(pbar-prop)/sqrt((prop*(1-prop))/n)
z        
alpha=0.1
crit.point1=qnorm(1-alpha)#p=0.1 one tailed (upper)
crit.point1
pval = 1-pnorm(z, lower.tail=TRUE)  # upper tail 
pval 



#### two sided test   ###################################

pbar1 = 0.04           # sample proportion 
prop1 = 0.02                # hypothesized value 
n = 1000                 # sample size 
z1=(pbar1-prop1)/sqrt((prop1*(1-prop1))/n)
z1                      # test statistic 
pval = 2*pnorm(z1, lower.tail=FALSE)  # upper tail 
pval 


#### Example: the number of hours of sleep for each   ####
#### of 24 students in class                          ####

sleep=c(7.75,8.5,8,6,8,6.33,8.17,7.75,7,6.5,8.75,8,7.5,3,6.25,8.5,9,6.5,9,9.5,9,8,8,9.5)
nine.hrs=ifelse(sleep>=9,"yes","no")
table(nine.hrs)
nine.hrs 
y=5;n=24
test=prop.test(y,n,p=0.5,alternative="two.sided",correct=FALSE)
test



####  Comparing two population proportions ###############

library(MASS)
prop.test(c(7,13),c(10000,5000), correct = F)


#### Analysis of IXJ table  ##############################

Anemic<-as.factor(c(rep("Yes",101),rep("No",99),rep("Yes",83),rep("No",117)
,rep("Yes",112),rep("No",89),rep("Yes",74),rep("No",126)))
Areas<-as.factor(c(rep("A",101),rep("A",99),rep("B",83),rep("B" ,117),rep("C",112),rep("C" ,89),rep("D",74),rep("D" ,126)))

areaAnemic<-table(Anemic,Areas)
chiArea <-chisq.test(areaAnemic,correct = FALSE)
chiArea 


#### Analysis of 2X3 table  ##############################

Anemic<-as.factor(c(rep("Yes",259),rep("No",169),rep("Yes",310),rep("No",469)))
Age<-as.factor(c(rep("06-23",259),rep("06-23",169),rep("24-59",310),rep("24-59",469)))
xx<-table(Anemic,Age)
chisq.test(xx,correct = FALSE)






