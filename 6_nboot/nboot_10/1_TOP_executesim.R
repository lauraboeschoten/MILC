## SIMULATION EXECUTION FUNCTION

#Set global workspace

setwd("D:\\Surfdrive\\Project 1\\datapackage\\6_nboot\\nboot_10")

#load required packages
require(brew)
require(psych)
require(testthat)
require(plyr)

#Set options for sci notation
options(scipen=999)

#Functions for simulations
source("2_preparesim.R")
source("2_simulate.R")
source("2_bootstrap.R")
source("2_conditional.R")
source("2_posterior.R")
source("2_impute.R")
source("2_runtemplate.R")

#set global simulation parameters 
nsim  <- 1000   # number of simulations 
nboot <- 10     # number of bootstrap samples                       
ncomb <- 32     # number of possible combinations of scores                
nvar  <- 5      # number of variables

#input parameters for scenarios (CHANGE IN ALL TOP SCRIPTS)
scens <- list(
ss     =c(1000),
c.prob =c(0.70,0.80,0.90,0.95,0.99),
imp    =c(0.10))

#Expand grid for scenarios  
scen.mat     <- expand.grid(scens)  

#Name brew
brew.file    <- "brew_model.brew"

#start timing
st.global    <- Sys.time()

#Run simulations for all scenarios
prep_sim(scen.mat=scen.mat,nsim=nsim,nboot=nboot,
         ncomb=ncomb,brew.file=brew.file,seed=123)

#runtime
rt.global <- Sys.time() - st.global
cat(rt.global)

