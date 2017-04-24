evaluation <- function(object, pop_Z, ssize){
  
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  
  # table totals for constrained covariate Z and W 
  
  totals_Z <- matrix(NA, nsim, 80) # CHANGE
  
  for (i in 1:nsim)
  {
    totals_Z[i,1]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[1,1]
    totals_Z[i,2]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[1,1]
    totals_Z[i,3]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[1,1]
    totals_Z[i,4]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[1,1]
    totals_Z[i,5]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[1,1]
    totals_Z[i,6]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[1,1]
    totals_Z[i,7]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[1,1]
    totals_Z[i,8]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[1,1]
    totals_Z[i,9]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[1,1]
    totals_Z[i,10] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[1,1]
    totals_Z[i,11] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[1,1]
    totals_Z[i,12] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[1,1]
    totals_Z[i,13] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[1,1]
    totals_Z[i,14] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[1,1]
    totals_Z[i,15] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[1,1]
    totals_Z[i,16] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[1,1]
    totals_Z[i,17] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[1,1]
    totals_Z[i,18] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[1,1]
    totals_Z[i,19] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[1,1]
    totals_Z[i,20] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[1,1]
    
    totals_Z[i,21] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[1,2]
    totals_Z[i,22] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[1,2]
    totals_Z[i,23] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[1,2]
    totals_Z[i,24] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[1,2]
    totals_Z[i,25] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[1,2]
    totals_Z[i,26] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[1,2]
    totals_Z[i,27] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[1,2]
    totals_Z[i,28] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[1,2]
    totals_Z[i,29] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[1,2]
    totals_Z[i,30] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[1,2]
    totals_Z[i,31] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[1,2]
    totals_Z[i,32] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[1,2]
    totals_Z[i,33] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[1,2]
    totals_Z[i,34] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[1,2]
    totals_Z[i,35] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[1,2]
    totals_Z[i,36] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[1,2]
    totals_Z[i,37] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[1,2]
    totals_Z[i,38] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[1,2]
    totals_Z[i,39] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[1,2]
    totals_Z[i,40] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[1,2]
    

    totals_Z[i,41]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[2,1]
    totals_Z[i,42]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[2,1]
    totals_Z[i,43]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[2,1]
    totals_Z[i,44]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[2,1]
    totals_Z[i,45]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[2,1]
    totals_Z[i,46]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[2,1]
    totals_Z[i,47]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[2,1]
    totals_Z[i,48]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[2,1]
    totals_Z[i,49]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[2,1]
    totals_Z[i,50]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[2,1]
    totals_Z[i,51]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[2,1]
    totals_Z[i,52]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[2,1]
    totals_Z[i,53]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[2,1]
    totals_Z[i,54]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[2,1]
    totals_Z[i,55]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[2,1]
    totals_Z[i,56]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[2,1]
    totals_Z[i,57]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[2,1]
    totals_Z[i,58]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[2,1]
    totals_Z[i,59]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[2,1]
    totals_Z[i,60]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[2,1]
    
    totals_Z[i,61] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[2,2]
    totals_Z[i,62] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[2,2]
    totals_Z[i,63] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[2,2]
    totals_Z[i,64] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[2,2]
    totals_Z[i,65] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[2,2]
    totals_Z[i,66] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[2,2]
    totals_Z[i,67] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[2,2]
    totals_Z[i,68] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[2,2]
    totals_Z[i,69] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[2,2]
    totals_Z[i,70] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[2,2]
    totals_Z[i,71] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[2,2]
    totals_Z[i,72] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[2,2]
    totals_Z[i,73] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[2,2]
    totals_Z[i,74] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[2,2]
    totals_Z[i,75] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[2,2]
    totals_Z[i,76] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[2,2]
    totals_Z[i,77] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[2,2]
    totals_Z[i,78] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[2,2]
    totals_Z[i,79] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[2,2]
    totals_Z[i,80] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[2,2]
  }
  
  # Pool cellproportions for constraind covariate Z and W 
  
  pool_Z           <- array(NA, dim=c(nsim, 50, 4))                                                                            
  dimnames(pool_Z) <- list(NULL, c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
                                   "Qhat6","Qhat7","Qhat8","Qhat9","Qhat10",
                                   "Qhat11","Qhat12","Qhat13","Qhat14","Qhat15",
                                   "Qhat16","Qhat17","Qhat18","Qhat19","Qhat20",
                                   "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",
                                   "Uhat6","Uhat7","Uhat8","Uhat9","Uhat10",
                                   "Uhat11","Uhat12","Uhat13","Uhat14","Uhat15",
                                   "Uhat16","Uhat17","Uhat18","Uhat19","Uhat20",
                                   "Qbar","Ubar","B","T","sqrtT","ll","ul",
                                   "ci","bias","cov"), 
                           c("Z1W1","Z1W2","Z2W1","Z2W2"))
  
  for (h in 1:20)
  {
    pool_Z[,h,1] <- totals_Z[,h   ]/ssize        
    pool_Z[,h,2] <- totals_Z[,h+20]/ssize                 
    pool_Z[,h,3] <- totals_Z[,h+40]/ssize
    pool_Z[,h,4] <- totals_Z[,h+60]/ssize
  }
  
  
  for(i in 1:nsim)
  {
    for(k in 1:4)
    {
      for(j in 21:40)
      {
        pool_Z[i,j,k] <- (pool_Z[i,j-20,k]*(1-pool_Z[i,j-20,k]))/ssize
      }
      pool_Z[i,"Qbar",k]   <- mean(pool_Z[i,1:20,k])                                   
      pool_Z[i,"Ubar",k]   <- mean(pool_Z[i,21:40,k])                                  
      pool_Z[i,"B",k]      <- var(pool_Z[i,1:20,k])           
      pool_Z[i,"T",k]      <- pool_Z[i,"Ubar",k]+(nboot+1)*(pool_Z[i,"B",k]/nboot)          
      pool_Z[i,"sqrtT",k]  <- sqrt(pool_Z[i,"T",k])                                     
      pool_Z[i,"ll",k]     <- pool_Z[i,"Qbar",k]-qt(.975, ssize-1)*(pool_Z[i,"sqrtT",k])              
      pool_Z[i,"ul",k]     <- pool_Z[i,"Qbar",k]+qt(.975, ssize-1)*(pool_Z[i,"sqrtT",k])         
      pool_Z[i,"ci",k]     <- pool_Z[i,"ul",k]-pool_Z[i,"ll",k]
    }
    for(l in 1:4)
    {
      pool_Z[i,"bias",l] <- pool_Z[i,"Qbar",l] - pop_Z[l,]
      pool_Z[i,"cov",l] <- pool_Z[i,"ll",l] < pop_Z[l,] & pop_Z[l,] < pool_Z[i,"ul",l]
    } 
  }
  
  # Combine pooled simulation results
  
  results_Z <- matrix(NA,4,50)
  dimnames(results_Z) <- list(c("Z1W1","Z1W2","Z2W1","Z2W2"),
                              c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
                                "Qhat6","Qhat7","Qhat8","Qhat9","Qhat10",
                                "Qhat11","Qhat12","Qhat13","Qhat14","Qhat15",
                                "Qhat16","Qhat17","Qhat18","Qhat19","Qhat20",
                                "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",
                                "Uhat6","Uhat7","Uhat8","Uhat9","Uhat10",
                                "Uhat11","Uhat12","Uhat13","Uhat14","Uhat15",
                                "Uhat16","Uhat17","Uhat18","Uhat19","Uhat20",  
                                "Qbar","Ubar","B","T","sqrtT","ll","ul",
                                "ci","bias","cov"))
  bias_var <- matrix(NA,4,1)
  
  for(m in 1:4)
  {
    results_Z[m,] <- apply(pool_Z[,,m],2,mean)
    bias_var[m,] <- results_Z[m,"sqrtT"]/sd(pool_Z[,"Qbar",m])
  }
  
  results_Z <- cbind(results_Z,bias_var)
  
  setTxtProgressBar(pb, i)
  close(pb)
  
  return(list(pool_Z = pool_Z,
              results_Z = results_Z))
}