library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)

ResultStCaculator <- function(SimResultTb, year, level){
  
  SimMean <- mean(SimResultTb[SimResultTb$Yr == year, level])
  SimSd <- sd(SimResultTb[SimResultTb$Yr == year, level])
  SimLowb <- SimMean - 1.96*SimSd
  SimUpb <- SimMean + 1.96*SimSd
  result  <- list(year, level, SimMean, SimSd, SimLowb, SimUpb)
  
  return(result)
}

EmpTargetTable <- as.data.frame(read_xlsx("./data/TargetStaff.xlsx"))
SimNum <- 5000
SimYear <- nrow(EmpTargetTable) - 1

SDP <- 0.1

# Initiate an empty data frame to store results
SimResultTb <- data.frame(Yr = NA, Level_1 = NA, Level_2 = NA, Level_3 = NA
                        ,Total = NA, SimRd = NA)
                        
# Initial number of employees by levels                        
EmpLevel <- c("Yr", "Level_1", "Level_2", "Level_3", "Total")
InitEmpNum <- data.frame(matrix(ncol = 5, nrow = 1)) # initial table
colnames(InitEmpNum) <- EmpLevel
InitEmpNum[1,] <- list(1, 500, 200, 50, 750)

#Probability of getting new hire
EmpNewhireProb <- c(0.6, 0.3, 0.1, 1)

#Probability of internal transition between diff levels
TransTable <- as.data.frame(cbind(c(0.8,0.05,0),c(0.1,0.75,0.15)
                            ,c(0.05,0.1,0.7),c(0.05,0.10,0.15)))
colnames(TransTable) <- c("Level_1","Level_2","Level_3","LeaveFired")

set.seed(1314520)
for (r in 1:SimNum){
  
  EmpNumTb <- data.frame(matrix(ncol = 6, nrow = SimYear+1))
  colnames(EmpNumTb) <- colnames(SimResultTb)
  EmpNumTb[1,1:5] <- InitEmpNum[1,]
  EmpNumTb[ ,6] <- r
  
  for (y in 1 : SimYear){
  L1toL1 <- rbinom(1, EmpNumTb[y, 2], TransTable[1,1])
  L1toL2 <- rbinom(1, EmpNumTb[y, 2] - L1toL1, TransTable[1,2]/(1 - TransTable[1,1]))
  L1toL3 <- rbinom(1, EmpNumTb[y, 2] - L1toL1 - L1toL2, TransTable[1,3]/(1 - TransTable[1,2] - TransTable[1,1]))
  L1Fired <- EmpNumTb[y, 2] - L1toL1 - L1toL2 - L1toL3
  
  L2toL1 <- rbinom(1, EmpNumTb[y, 3], TransTable[2,1])
  L2toL2 <- rbinom(1, EmpNumTb[y, 3] - L2toL1, TransTable[2,2]/(1 - TransTable[2,1]))
  L2toL3 <- rbinom(1, EmpNumTb[y, 3] - L2toL1 - L2toL2, TransTable[3,2]/(1 - TransTable[2,2] - TransTable[2,1]))
  L2Fired <- EmpNumTb[y, 3] - L2toL1 - L2toL2 - L2toL3
  
  L3toL1 <- rbinom(1, EmpNumTb[y, 4], TransTable[3,1])
  L3toL2 <- rbinom(1, EmpNumTb[y, 4] - L3toL1, TransTable[3,2]/(1 - TransTable[3,1]))
  L3toL3 <- rbinom(1, EmpNumTb[y, 4] - L3toL1 - L3toL2, TransTable[3,3]/(1 - TransTable[3,2] - TransTable[3,1]))
  L3Fired <- EmpNumTb[y, 4] - L3toL1 - L3toL2 - L3toL3
  
  TotalCurrent <- EmpNumTb[y, "Total"] - (L1Fired + L2Fired + L3Fired)
  
  NewHPool <- rnorm(1, (EmpTargetTable[y+1,"Total"]-TotalCurrent), ((EmpTargetTable[y+1,"Total"]-TotalCurrent)*SDP))
  NewHPool <- round(NewHPool, digits=0)
  NewL1H <- rbinom(1, NewHPool, EmpNewhireProb[1])
  NewL2H <- rbinom(1, NewHPool - NewL1H, EmpNewhireProb[2]/(1 - EmpNewhireProb[1]))
  NewL3H <- NewHPool - NewL1H - NewL2H
  
  L1Current <- L1toL1 + L2toL1 + L3toL1 + NewL1H
  L2Current <- L1toL2 + L2toL2 + L3toL2 + NewL2H
  L3Current <- L1toL3 + L2toL3 + L3toL3 + NewL3H  
  TotalCurrent <- TotalCurrent + NewL1H + NewL2H + NewL3H
  
  EmpNumTb[y+1, ] <- list(y+1, L1Current, L2Current, L3Current, TotalCurrent, r)
  } 
  
  SimResultTb <- rbind(SimResultTb, EmpNumTb)
}

SimResultTb <- SimResultTb[2:nrow(SimResultTb),] # remove first row with NAs

ResultStTable <- data.frame(Yr = NA, Level = NA, Sim_Mean = NA, Sim_Sd = NA
                            ,Sim_LowerB = NA, Sim_UpperB = NA)
for (y in 2:11){
  for (l in EmpLevel[2:5]){
    r <- ResultStCaculator(SimResultTb, y, l)
    ResultStTable <- rbind(ResultStTable, r)
  }
}

ResultStTable <- ResultStTable[2:nrow(ResultStTable),]

write.csv(SimResultTb, "output/WorkForce/SimResults.csv", row.names=FALSE)
write.csv(ResultStTable, "output/WorkForce/ResultSummary.csv", row.names=FALSE)
