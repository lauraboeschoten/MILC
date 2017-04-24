## DATA SAMPLE CREATION USING LG SCRIPT

#Set global workspace
setwd("D:\\Surfdrive\\Project 1\\datapackage\\createdata")

#load required packages
require(brew)
require(psych)
require(testthat)
require(plyr)

#Set options for sci notation
options(scipen=999)

#Load Functions for making data samples
source("preparedata.R")
source("preparedata_template.R")

#set global simulation parameters 
nsim  <- 1000   # number of simulations
nboot <- 5      # number of bootstrap samples
ncomb <- 32     # number of possible combinations of scores                
nvar  <- 5      # number of variables

#input parameters for scenarios
scens <- list(
ss     =c(1000, 10000),
c.prob =c(0.70,0.80,0.90,0.95,0.99),
imp    =c(0.01,0.05,0.10,0.20),
co.coef=c(0.45,0.50,0.55,0.65))

#Expand grid for scenarios  
scen.mat <- expand.grid(scens)  

#Make data files using LG
inp.make(scen.mat,nsim)

