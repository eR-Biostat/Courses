


phi <- 0.3
phi1 <- 0.6*phi
phi2 <- 1.2*phi
nj <- 10
pi1j=pi0j=pi2j=1/3
lambda1 <- (log((1-phi1)/(1-phi))+(nj^-1)*log(pi1j/pi0j))/
            log((phi*(1-phi1))/(phi1*(1-phi)))

lambda2 <- (log((1-phi)/(1-phi2))+(nj^-1)*log(pi0j/pi2j))/
  log((phi2*(1-phi))/(phi*(1-phi2)))

out<- matrix(NA,nrow=length(lambda1),ncol=length(lambda2))

for(i in 1:length(lambda1)){
  for(j in 1:length(lambda2)) {
    error_H0_Lambda1 <-  pbinom(nj*lambda1[i],nj,phi) + pbinom((nj*lambda2[j]-1),nj,phi,lower.tail=FALSE)
    error_H1_Lambda1 <-  pbinom((nj*lambda1[i]-1),nj,phi1,lower.tail=FALSE)                       
    error_H2_Lambda1 <- pbinom(nj*lambda2[j],nj,phi2)    
    out[i,j] <- error_H0_Lambda1 + error_H1_Lambda1 + error_H2_Lambda1
    
  }
} 

min_index <- which.min(out)

# Convert the flat index to row and column indices
min_row <- (min_index - 1) %/% ncol(out) + 1
min_col <- (min_index - 1) %% ncol(out) + 1

# Extract the corresponding lambda1 and lambda2 values
lambda1_min <- lambda1[min_row]
lambda2_min <- lambda2[min_col]

out[min_row,min_col]


out2 <- data.frame(out)
row.names(out2) <- lambda1
names(out2) <- lambda2

min_position <- which(out2 == min(out), arr.ind = TRUE)

out2[26,86]
lambda1[26]
apply(out,2,min)
## Hi

plot(lambda1,out[3,])


error_H0_Lambda1 <-  pbinom(nj*0.236,nj,phi) + pbinom((nj*0.330-1),nj,phi,lower.tail=FALSE)
error_H1_Lambda1 <-  pbinom((nj*0.236-1),nj,phi1,lower.tail=FALSE)                       
error_H2_Lambda1 <- pbinom(nj*0.330,nj,phi2)    
out[i,j] <- error_H0_Lambda1 + error_H1_Lambda1 + error_H2_Lambda1




#######################################################################

# Load necessary library
if (!require(BOIN)) {
  install.packages("BOIN")
}
library(BOIN)


bound <- get.boundary(target=0.3, ncohort=18, cohortsize=3)

write.csv(bound$full_boundary_tab,file="C:/Users/ask64240/OneDrive - GSK/South Africa Course//boundary.csv")

# Define the dose levels and number of patients/DLTs

patients <- c(3, 4,11)
dlt <- c(0, 0,1)


selmtd <- select.mtd(target=0.3, npts=patients, ntox=dlt)
summary(selmtd)
plot(selmtd)

# Create a data frame for the BOIN design
data <- data.frame(dose = dose_levels, n = patients, y = dlt)

# Define the target toxicity rate
target_toxicity <- 0.3

# Run the BOIN design to determine the MTD
boin_result <- boin(data, target = target_toxicity, p_zero = 0.3, p_five = 0.7)

# Extract the MTD and other information from the result
mtd <- boin_result$mtd
mtd_info <- boin_result$mtd_info

##
library(BOIN)

### Starting Dose = Dose 1
target <- 0.25
p.true <-  c(0.02,0.05,0.08,0.1,0.15)
ncohort  <- 12
cohortsize <- 3
startdose <- 1
scen1_Dose1 <- get.oc(target, p.true, ncohort, cohortsize,
                      startdose,ntrial=1000, seed=6)



out_Dose1 <- data.frame(Dose=scen1_Dose1$simu.setup$dose,Target=scen1_Dose1$simu.setup$target,P.True=scen1_Dose1$simu.setup$p.true,selpercent= scen1_Dose1$selpercent,npatients=scen1_Dose1$npatients,ntox=scen1_Dose1$ntox)
write.csv(out_Dose1,file="C:/Users/ask64240/OneDrive - GSK/South Africa Course/out_Dose1.csv")



### Starting Dose = Dose 2
target <- 0.25
p.true <-  c(0.02,0.05,0.08,0.1,0.15)
ncohort  <- 12
cohortsize <- 3
startdose <- 2
scen1_Dose2 <- get.oc(target, p.true, ncohort, cohortsize,
                      startdose,ntrial=1000, seed=6)




out_Dose2 <- data.frame(Dose=scen1_Dose2$simu.setup$dose,Target=scen1_Dose2$simu.setup$target,P.True=scen1_Dose2$simu.setup$p.true,selpercent= scen1_Dose2$selpercent,npatients=scen1_Dose2$npatients,ntox=scen1_Dose2$ntox)
write.csv(out_Dose2,file="C:/Users/ask64240/OneDrive - GSK/South Africa Course/out_Dose2.csv")



### Starting Dose = Dose 2
target <- 0.25
p.true <-  c(0.02,0.05,0.08,0.1,0.15)
ncohort  <- 12
cohortsize <- 3
startdose <- 3
scen1_Dose3 <- get.oc(target, p.true, ncohort, cohortsize,
                      startdose,ntrial=1000, seed=6)




out_Dose3 <- data.frame(Dose=scen1_Dose3$simu.setup$dose,Target=scen1_Dose3$simu.setup$target,P.True=scen1_Dose3$simu.setup$p.true,selpercent= scen1_Dose3$selpercent,
                        npatients=scen1_Dose3$npatients,ntox=scen1_Dose3$ntox)

write.csv(out_Dose3,file="C:/Users/ask64240/OneDrive - GSK/South Africa Course/out_Dose3.csv")





### Starting Dose = Dose 2
target <- 0.25
p.true <-  c(0.02,0.05,0.08,0.1,0.15)
ncohort  <- 12
cohortsize <- 3
startdose <- 2
n.earlystop=9
scen1_Dose2 <- get.oc(target, p.true, ncohort, cohortsize,n.earlystop,
                      startdose,ntrial=1000, seed=6)




out_Dose2 <- data.frame(Dose=scen1_Dose2$simu.setup$dose,Target=scen1_Dose2$simu.setup$target,P.True=scen1_Dose2$simu.setup$p.true,selpercent= scen1_Dose2$selpercent,npatients=scen1_Dose2$npatients,ntox=scen1_Dose2$ntox)
write.csv(out_Dose2,file="C:/Users/ask64240/OneDrive - GSK/South Africa Course/out_Dose2.csv")



### Starting Dose = Dose 2 + early stopping
target <- 0.25
p.true <-  c(0.02,0.05,0.08,0.1,0.15)
ncohort  <- 12
cohortsize <- 3
startdose <- 3
n.earlystop=9
scen1_Dose3 <- get.oc(target, p.true, ncohort, cohortsize,n.earlystop,
                      startdose,ntrial=1000, seed=6)




out_Dose3 <- data.frame(Dose=scen1_Dose3$simu.setup$dose,Target=scen1_Dose3$simu.setup$target,P.True=scen1_Dose3$simu.setup$p.true,selpercent= scen1_Dose3$selpercent,
                        npatients=scen1_Dose3$npatients,ntox=scen1_Dose3$ntox)

write.csv(out_Dose3,file="C:/Users/ask64240/OneDrive - GSK/South Africa Course/out_Dose3.csv")
