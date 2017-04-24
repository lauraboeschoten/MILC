## SIMULATION EXECUTION FUNCTION

#Set global workspace
setwd("D:\\Surfdrive\\Project 1\\datapackage\\4_one_indicator_used")

#load required packages
require(brew)
require(psych)
require(testthat)
require(plyr)

#Set options for sci notation
options(scipen=999)

#Functions for simulations


#set global simulation parameters 
nsim  <- 1000    # number of simulations
nboot <- 5      # number of bootstrap samples
ncomb <- 32     # number of possible combinations of scores                
nvar  <- 5      # number of variables

#input parameters for scenarios (CHANGE IN ALL TOP SCRIPTS)
scens <- list(
  ss     =c(1000, 10000),
  c.prob =c(0.70, 0.80, 0.90, 0.95, 0.99),
  imp    =c(0.01, 0.05, 0.10, 0.20),
  co.coef=c(0.50, 0.45, 0.55, 0.65))

#Expand grid for scenarios  
scen.mat     <- expand.grid(scens)  

for (i in 1:nrow(scen.mat)){
  cat(i)
  ssize <- scen.mat[i,1]
  
  setwd("D:\\Surfdrive\\Project 1\\datapackage\\0_createdata")
  
  dat.fil.name <- paste0("dat_",scen.mat[i,1],"_",
                         substring(scen.mat[i,2], 3),"_",
                         substring(scen.mat[i,3], 3),"_",
                         substring(scen.mat[i,4], 3),".txt")
  
  
  samples        <- read.delim(dat.fil.name, header=T, sep="\t")
  list_samples   <- split(samples, samples$simulation)
  
  # create empty frame with population values for Z 
  totals_Z       <- array(NA, dim=c(nsim,9,4)) 
  totals_Z[,7,1] <- 0.50
  totals_Z[,7,2] <- 0.50-scen.mat[i,3]
  totals_Z[,7,3] <- 0.00
  totals_Z[,7,4] <- scen.mat[i,3]
  
  # create empty frame with population values for Q
  totals_Q       <- array(NA, dim=c(nsim, 9, 2))
  totals_Q[,7,1] <- 0.00
  totals_Q[,7,2] <- log((1-scen.mat[i,4])/scen.mat[i,4])
  
  
  for (n in 1:nsim){
    #add cellproportions of Y1xZ 
    if (nrow(table(list_samples[[n]]$pregnant,list_samples[[n]]$gender1))==1){
      totals_Z[n,1,1]  <- table(list_samples[[n]]$pregnant,list_samples[[n]]$gender1)[1,1]/ssize
      totals_Z[n,1,2]  <- table(list_samples[[n]]$pregnant,list_samples[[n]]$gender1)[1,2]/ssize
      totals_Z[n,1,3]  <- 0
      totals_Z[n,1,4]  <- 0
    } else {
      totals_Z[n,1,1]  <- table(list_samples[[n]]$pregnant,list_samples[[n]]$gender1)[1,1]/ssize
      totals_Z[n,1,2]  <- table(list_samples[[n]]$pregnant,list_samples[[n]]$gender1)[1,2]/ssize
      totals_Z[n,1,3]  <- table(list_samples[[n]]$pregnant,list_samples[[n]]$gender1)[2,1]/ssize
      totals_Z[n,1,4]  <- table(list_samples[[n]]$pregnant,list_samples[[n]]$gender1)[2,2]/ssize
    }
    
    #add int and coef of Y1 on Q and corresponding se's
    list_samples[[n]]$sport <- ifelse(list_samples[[n]]$sport==2,0,1)
    totals_Q[n,1,1:2] <- summary(glm(sport ~ gender1, "binomial", list_samples[[n]], control = list(maxit = 50)))$coefficients[1:2,1]
    totals_Q[n,2,1:2] <- summary(glm(sport ~ gender1, "binomial", list_samples[[n]], control = list(maxit = 50)))$coefficients[1:2,2]^2

  
    #add var,se,ci,bias and cov to totals_Z
    for (j in 1:4){
      totals_Z[n,2,j] <- (totals_Z[n,1,j]*(1-totals_Z[n,1,j]))/ssize                           #var
      totals_Z[n,3,j] <- sqrt(totals_Z[n,2,j])                                                 #se
      totals_Z[n,4,j] <- totals_Z[n,1,j]-qt(.975, ssize-1)*(totals_Z[n,3,j])                   #ul
      totals_Z[n,5,j] <- totals_Z[n,1,j]+qt(.975, ssize-1)*(totals_Z[n,3,j])                   #ll
      totals_Z[n,6,j] <- totals_Z[n,5,j]-totals_Z[n,4,j]                                       #ci
      totals_Z[n,8,j] <- totals_Z[n,1,j]-totals_Z[n,7,j]                                       #bias
      totals_Z[n,9,j] <- totals_Z[n,4,j] < totals_Z[n,7,j] & totals_Z[n,7,j] < totals_Z[n,5,j] #cov
    }
    
    #add se,ci,bias and cov to totals_Q
    for (j in 1:2){
      totals_Q[n,3,j] <- sqrt(totals_Q[n,2,j])                                                 #se
      totals_Q[n,4,j] <- totals_Q[n,1,j]-qt(.975, ssize-1)*(totals_Q[n,3,j])                   #ul
      totals_Q[n,5,j] <- totals_Q[n,1,j]+qt(.975, ssize-1)*(totals_Q[n,3,j])                   #ll
      totals_Q[n,6,j] <- totals_Q[n,5,j]-totals_Q[n,4,j]                                       #ci
      totals_Q[n,8,j] <- totals_Q[n,1,j]-totals_Q[n,7,j]                                       #bias
      totals_Q[n,9,j] <- totals_Q[n,4,j] < totals_Q[n,7,j] & totals_Q[n,7,j] < totals_Q[n,5,j] #cov
    }
    
  }
  
  #create matrix for final results of Z
  results_Z <- matrix(NA,4,9)
  bias_varZ <- matrix(NA,4,1)
  
  #create matrix for final results of Q
  results_Q <- matrix(NA,2,9)
  bias_varQ <- matrix(NA,2,1)
  
  #fill results_Z matrix
  for(j in 1:4)
  {
    results_Z[j,] <- apply(totals_Z[,,j],2,mean)
    bias_varZ[j,] <- results_Z[j,3]/sd(totals_Z[,1,j])
  }
  
  #fill results_Q matrix
  for(j in 1:2)
  {
    results_Q[j,] <- apply(totals_Q[,,j],2,mean)
    bias_varQ[j,] <- results_Q[j,3]/sd(totals_Q[,1,j])
  }
  
  #finalize results_Z
  results_Z           <- cbind(results_Z,bias_varZ)
  colnames(results_Z) <- c("Qbar","var","se","ll","ul","ci","pop","bias","cov","sesd")
  
  #finalize results_Q
  results_Q           <- cbind(results_Q,bias_varQ)
  colnames(results_Q) <- c("Qbar","var","se","ll","ul","ci","pop","bias","cov","sesd")
  
  
  write.table(results_Z, paste0("D:\\Surfdrive\\Project 1\\datapackage\\4_one_indicator_used\\results_z",
                                scen.mat[i,1],"_",
                                substring(scen.mat[i,2], 3),"_",
                                substring(scen.mat[i,3], 3),"_",
                                substring(scen.mat[i,4], 3),".txt"))
  
  write.table(results_Q, paste0("D:\\Surfdrive\\Project 1\\datapackage\\4_one_indicator_used\\results_Q",
                                scen.mat[i,1],"_",
                                substring(scen.mat[i,2], 3),"_",
                                substring(scen.mat[i,3], 3),"_",
                                substring(scen.mat[i,4], 3),".txt"))
  
  
}