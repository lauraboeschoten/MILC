evaluation <- function(object, pop_Z, pop_Q, ssize){
  
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  
  totals_Z <- matrix(NA, nsim, 20)
  
  for (i in 1:nsim)
  {
    if(ncol(table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)) == 1) {
      totals_Z[i,1]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[1,1]
      totals_Z[i,6]  <- 0
      totals_Z[i,11] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[2,1]
      totals_Z[i,16] <- 0
    } else {
      totals_Z[i,1]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[1,1]
      totals_Z[i,6]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[1,2]
      totals_Z[i,11] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[2,1]
      totals_Z[i,16] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[2,2]
    }
    
    if(ncol(table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)) == 1) {
      totals_Z[i,2]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[1,1]
      totals_Z[i,7]  <- 0
      totals_Z[i,12] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[2,1]
      totals_Z[i,17] <- 0
    } else {
      totals_Z[i,2]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[1,1]
      totals_Z[i,7]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[1,2]
      totals_Z[i,12] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[2,1]
      totals_Z[i,17] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[2,2]
    }
    
    if(ncol(table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)) == 1) {
      totals_Z[i,3]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[1,1]
      totals_Z[i,8]  <- 0
      totals_Z[i,13] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[2,1]
      totals_Z[i,18] <- 0
    } else {
      totals_Z[i,3]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[1,1]
      totals_Z[i,8]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[1,2]
      totals_Z[i,13] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[2,1]
      totals_Z[i,18] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[2,2]
    }
    
    if(ncol(table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)) == 1) {
      totals_Z[i,4]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[1,1]
      totals_Z[i,9]  <- 0
      totals_Z[i,14] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[2,1]
      totals_Z[i,19] <- 0
    } else {
      totals_Z[i,4]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[1,1]
      totals_Z[i,9]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[1,2]
      totals_Z[i,14] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[2,1]
      totals_Z[i,19] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[2,2]
    }
    
    if(ncol(table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)) == 1) {
      totals_Z[i,5]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[1,1]
      totals_Z[i,10] <- 0
      totals_Z[i,15] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[2,1]
      totals_Z[i,20] <- 0
    } else {
      totals_Z[i,5]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[1,1]
      totals_Z[i,10]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[1,2]
      totals_Z[i,15] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[2,1]
      totals_Z[i,20] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[2,2]
    }
    
  }
  
  # Pool cellproportions for constraind covariate Z and W 
  
  pool_Z           <- array(NA, dim=c(nsim, 20, 4))                                                                            
  dimnames(pool_Z) <- list(NULL, c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
                                   "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",    
                                   "Qbar","Ubar","B","T","sqrtT","ll","ul",
                                   "ci","bias","cov"), 
                           c("Z1W1","Z1W2","Z2W1","Z2W2"))
  
  for (h in 1:5)
  {
    pool_Z[,h,1] <- totals_Z[,h   ]/ssize        
    pool_Z[,h,2] <- totals_Z[,h+5 ]/ssize                 
    pool_Z[,h,3] <- totals_Z[,h+10]/ssize
    pool_Z[,h,4] <- totals_Z[,h+15]/ssize
  }
  
  
  for(i in 1:nsim)
  {
    for(k in 1:4)
    {
      for(j in 6:10)
      {
        pool_Z[i,j,k] <- (pool_Z[i,j-5,k]*(1-pool_Z[i,j-5,k]))/ssize
      }
      pool_Z[i,"Qbar",k]   <- mean(pool_Z[i,1:5,k])                                   
      pool_Z[i,"Ubar",k]   <- mean(pool_Z[i,6:10,k])                                  
      pool_Z[i,"B",k]      <- var(pool_Z[i,1:5,k])           
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
    setTxtProgressBar(pb, i)
  }
  
  # Combine pooled simulation results
  
  results_Z <- matrix(NA,4,20)
  dimnames(results_Z) <- list(c("Z1W1","Z1W2","Z2W1","Z2W2"),
                              c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
                                "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",    
                                "Qbar","Ubar","B","T","sqrtT","ll","ul",
                                "ci","bias","cov"))
  bias_var <- matrix(NA,4,1)
  
  for(m in 1:4)
  {
    results_Z[m,] <- apply(pool_Z[,,m],2,mean)
    bias_var[m,] <- results_Z[m,"sqrtT"]/sd(pool_Z[,"Qbar",m])
  }
  
  results_Z <- cbind(results_Z,bias_var)
  
  ## GLM
  pool_Q           <- array(NA, dim=c(nsim, 20, 2))                                                                            
  dimnames(pool_Q) <- list(NULL, c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
                                   "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",    
                                   "Qbar","Ubar","B","T","sqrtT","ll","ul",
                                   "ci","bias","cov"), 
                           c("intercept","coefficient"))
  
  for(i in 1:nsim)
  {
    object[[i]][[3]]$sport <- ifelse(object[[i]][[3]]$sport==2,0,1)
    
    gl1 <- glm(formula = sport ~ imputation.1, family="binomial", object[[i]][[3]])
    gl2 <- glm(formula = sport ~ imputation.2, family="binomial", object[[i]][[3]])
    gl3 <- glm(formula = sport ~ imputation.3, family="binomial", object[[i]][[3]])
    gl4 <- glm(formula = sport ~ imputation.4, family="binomial", object[[i]][[3]])
    gl5 <- glm(formula = sport ~ imputation.5, family="binomial", object[[i]][[3]])
    
    pool_Q[i,"Qhat1",1:2] <- summary(gl1)$coefficients[1:2,1]
    pool_Q[i,"Qhat2",1:2] <- summary(gl2)$coefficients[1:2,1]
    pool_Q[i,"Qhat3",1:2] <- summary(gl3)$coefficients[1:2,1]
    pool_Q[i,"Qhat4",1:2] <- summary(gl4)$coefficients[1:2,1]
    pool_Q[i,"Qhat5",1:2] <- summary(gl5)$coefficients[1:2,1]
    
    pool_Q[i,"Uhat1",1:2] <- summary(gl1)$coefficients[1:2,2]
    pool_Q[i,"Uhat2",1:2] <- summary(gl2)$coefficients[1:2,2]
    pool_Q[i,"Uhat3",1:2] <- summary(gl3)$coefficients[1:2,2]
    pool_Q[i,"Uhat4",1:2] <- summary(gl4)$coefficients[1:2,2]
    pool_Q[i,"Uhat5",1:2] <- summary(gl5)$coefficients[1:2,2]
    
    for(j in 1:2){
      pool_Q[i,"Qbar",j]   <- mean(pool_Q[i,1:5,j])                                   
      pool_Q[i,"Ubar",j]   <- mean(pool_Q[i,6:10,j]^2)                                  
      pool_Q[i,"B",j]      <- var(pool_Q[i,1:5,j])           
      pool_Q[i,"T",j]      <- pool_Q[i,"Ubar",j]+(nboot+1)*(pool_Q[i,"B",j]/nboot)          
      pool_Q[i,"sqrtT",j]  <- sqrt(pool_Q[i,"T",j])
      pool_Q[i,"ll",j]     <- pool_Q[i,"Qbar",j]-qt(.975,ssize-1)*(pool_Q[i,"sqrtT",j])              
      pool_Q[i,"ul",j]     <- pool_Q[i,"Qbar",j]+qt(.975,ssize-1)*(pool_Q[i,"sqrtT",j])         
      pool_Q[i,"ci",j]     <- pool_Q[i,"ul",j]-pool_Q[i,"ll",j]                  
    }
    
    for (k in 1:2)
    {
      pool_Q[i,"bias",k] <- pool_Q[i,"Qbar",k] - pop_Q[k,]
      pool_Q[i,"cov",k] <- pool_Q[i,"ll",k] < pop_Q[k,] & pop_Q[k,] < pool_Q[i,"ul",k]
    }
  }
  
  results_Q <- matrix(NA,2,20)
  
  dimnames(results_Q) <- list(c("intercept","coefficient"),
                              c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
                                "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",    
                                "Qbar","Ubar","B","T","sqrtT","ll","ul",
                                "ci","bias","cov"))
  bias_var <- matrix(NA,2,1)
  
  for(m in 1:2)
  {
    results_Q[m,] <- apply(pool_Q[,,m],2,mean)
    bias_var[m,] <- results_Q[m,"sqrtT"]/sd(pool_Q[,"Qbar",m])
  }
  results_Q <- cbind(results_Q,bias_var)
  
  close(pb)
  
  
  return(list(pool_Z = pool_Z, 
              pool_Q = pool_Q,
              results_Z = results_Z,
              results_Q=results_Q))
}