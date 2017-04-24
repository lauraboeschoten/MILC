## EVALUATE SCRIPT

#Set global workspace
setwd("D:\\Surfdrive\\Project 1\\datapackage\\6_nboot\\nboot_40")

#load required packages
require(brew)
require(psych)
require(testthat)
require(plyr)

#Set options for sci notation
options(scipen=999)

#Functions for simulations
source("3_evaluation.R")

#set global simulation parameters 
nsim  <- 1000   # number of simulations
nboot <- 40      # number of bootstrap samples
ncomb <- 32     # number of possible combinations of scores                
nvar  <- 5      # number of variables

#input parameters for scenarios (CHANGE IN ALL TOP SCRIPTS)
scens <- list(
  ss     =c(1000),
  c.prob =c(0.70, 0.80, 0.90, 0.95, 0.99),
  imp    =c(0.10))

#Expand grid for scenarios  
scen.mat     <- expand.grid(scens)  

#Make pop_Z matrix based on all scenarios in scen.mat
pop_Z          <- matrix(NA,nrow=4,ncol=nrow(scen.mat))
pop_Z[c(1,3),] <- rep(c(0.5,0),times=nrow(scen.mat)) 
pop_Z[c(2,4),] <- rbind(0.50-scen.mat$imp,scen.mat$imp)

#Make pop_Q which is identical for all scenarios
pop_Q       <- matrix(0,2,nrow(scen.mat))

#Make list for evaluation output
list.out <- vector("list",nrow(scen.mat))

#Loop for evaluation over all scenarios
for(a in 1:nrow(scen.mat)){

#Name of file to load from TOP_executesim.R
nam.load <- paste0("sim_res_",scen.mat[a,1],"_",
                   substring(scen.mat[a,2], 3),"_",
                   substring(scen.mat[a,3], 3),".Rdata")  

#Load output data
nam.out.data <- load(paste0(nam.load))  

#Save loaded list with sim output to new variable name for easy application in loop
var.out.data <- eval(parse(text = nam.out.data))

#evaluate sim list with evaluation function
eval.out     <- evaluation(object=var.out.data,pop_Z=as.matrix(pop_Z[,a]),ssize=scen.mat[a,1])

#Save evaluation output to list
list.out[[a]] <- eval.out

#Edit list name
names(list.out)[a] <- nam.out.data

write.table(list.out[[a]][[2]],paste0(nam.out.data,"Z.txt"))
}

