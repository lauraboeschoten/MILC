evaluation <- function(object, pop_Z, ssize){
  
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  
  # table totals for constrained covariate Z and W 
  
  totals_Z <- matrix(NA, nsim, 160) # CHANGE
  
  for (i in 1:nsim)
  {
    totals_Z[i,1]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[1,1]
    totals_Z[i,2]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[1,1]
    totals_Z[i,3]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[1,1]
    totals_Z[i,4]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[1,1]
    totals_Z[i,5]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[1,1]
    totals_Z[i,6]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[1,1]
    totals_Z[i,7]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[1,1]
    totals_Z[i,8]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[1,1]
    totals_Z[i,9]   <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[1,1]
    totals_Z[i,10]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[1,1]
    totals_Z[i,11]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[1,1]
    totals_Z[i,12]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[1,1]
    totals_Z[i,13]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[1,1]
    totals_Z[i,14]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[1,1]
    totals_Z[i,15]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[1,1]
    totals_Z[i,16]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[1,1]
    totals_Z[i,17]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[1,1]
    totals_Z[i,18]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[1,1]
    totals_Z[i,19]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[1,1]
    totals_Z[i,20]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[1,1]
    totals_Z[i,21]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.21)[1,1]
    totals_Z[i,22]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.22)[1,1]
    totals_Z[i,23]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.23)[1,1]
    totals_Z[i,24]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.24)[1,1]
    totals_Z[i,25]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.25)[1,1]
    totals_Z[i,26]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.26)[1,1]
    totals_Z[i,27]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.27)[1,1]
    totals_Z[i,28]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.28)[1,1]
    totals_Z[i,29]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.29)[1,1]
    totals_Z[i,30]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.30)[1,1]
    totals_Z[i,31]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.31)[1,1]
    totals_Z[i,32]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.32)[1,1]
    totals_Z[i,33]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.33)[1,1]
    totals_Z[i,34]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.34)[1,1]
    totals_Z[i,35]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.35)[1,1]
    totals_Z[i,36]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.36)[1,1]
    totals_Z[i,37]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.37)[1,1]
    totals_Z[i,38]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.38)[1,1]
    totals_Z[i,39]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.39)[1,1]
    totals_Z[i,40]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.40)[1,1]
    totals_Z[i,41]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[1,2]
    totals_Z[i,42]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[1,2]
    totals_Z[i,43]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[1,2]
    totals_Z[i,44]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[1,2]
    totals_Z[i,45]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[1,2]
    totals_Z[i,46]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[1,2]
    totals_Z[i,47]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[1,2]
    totals_Z[i,48]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[1,2]
    totals_Z[i,49]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[1,2]
    totals_Z[i,50]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[1,2]
    totals_Z[i,51]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[1,2]
    totals_Z[i,52]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[1,2]
    totals_Z[i,53]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[1,2]
    totals_Z[i,54]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[1,2]
    totals_Z[i,55]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[1,2]
    totals_Z[i,56]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[1,2]
    totals_Z[i,57]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[1,2]
    totals_Z[i,58]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[1,2]
    totals_Z[i,59]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[1,2]
    totals_Z[i,60]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[1,2]
    totals_Z[i,61]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.21)[1,2]
    totals_Z[i,62]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.22)[1,2]
    totals_Z[i,63]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.23)[1,2]
    totals_Z[i,64]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.24)[1,2]
    totals_Z[i,65]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.25)[1,2]
    totals_Z[i,66]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.26)[1,2]
    totals_Z[i,67]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.27)[1,2]
    totals_Z[i,68]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.28)[1,2]
    totals_Z[i,69]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.29)[1,2]
    totals_Z[i,70]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.30)[1,2]
    totals_Z[i,71]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.31)[1,2]
    totals_Z[i,72]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.32)[1,2]
    totals_Z[i,73]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.33)[1,2]
    totals_Z[i,74]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.34)[1,2]
    totals_Z[i,75]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.35)[1,2]
    totals_Z[i,76]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.36)[1,2]
    totals_Z[i,77]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.37)[1,2]
    totals_Z[i,78]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.38)[1,2]
    totals_Z[i,79]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.39)[1,2]
    totals_Z[i,80]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.40)[1,2]
    totals_Z[i,81]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[2,1]
    totals_Z[i,82]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[2,1]
    totals_Z[i,83]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[2,1]
    totals_Z[i,84]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[2,1]
    totals_Z[i,85]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[2,1]
    totals_Z[i,86]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[2,1]
    totals_Z[i,87]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[2,1]
    totals_Z[i,88]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[2,1]
    totals_Z[i,89]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[2,1]
    totals_Z[i,90]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[2,1]
    totals_Z[i,91]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[2,1]
    totals_Z[i,92]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[2,1]
    totals_Z[i,93]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[2,1]
    totals_Z[i,94]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[2,1]
    totals_Z[i,95]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[2,1]
    totals_Z[i,96]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[2,1]
    totals_Z[i,97]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[2,1]
    totals_Z[i,98]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[2,1]
    totals_Z[i,99]  <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[2,1]
    totals_Z[i,100] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[2,1]
    totals_Z[i,101] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.21)[2,1]
    totals_Z[i,102] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.22)[2,1]
    totals_Z[i,103] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.23)[2,1]
    totals_Z[i,104] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.24)[2,1]
    totals_Z[i,105] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.25)[2,1]
    totals_Z[i,106] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.26)[2,1]
    totals_Z[i,107] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.27)[2,1]
    totals_Z[i,108] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.28)[2,1]
    totals_Z[i,109] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.29)[2,1]
    totals_Z[i,110] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.30)[2,1]
    totals_Z[i,111] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.31)[2,1]
    totals_Z[i,112] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.32)[2,1]
    totals_Z[i,113] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.33)[2,1]
    totals_Z[i,114] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.34)[2,1]
    totals_Z[i,115] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.35)[2,1]
    totals_Z[i,116] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.36)[2,1]
    totals_Z[i,117] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.37)[2,1]
    totals_Z[i,118] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.38)[2,1]
    totals_Z[i,119] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.39)[2,1]
    totals_Z[i,120] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.40)[2,1]
    totals_Z[i,121] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.1)[2,2]
    totals_Z[i,122] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.2)[2,2]
    totals_Z[i,123] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.3)[2,2]
    totals_Z[i,124] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.4)[2,2]
    totals_Z[i,125] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.5)[2,2]
    totals_Z[i,126] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.6)[2,2]
    totals_Z[i,127] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.7)[2,2]
    totals_Z[i,128] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.8)[2,2]
    totals_Z[i,129] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.9)[2,2]
    totals_Z[i,130] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.10)[2,2]
    totals_Z[i,131] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.11)[2,2]
    totals_Z[i,132] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.12)[2,2]
    totals_Z[i,133] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.13)[2,2]
    totals_Z[i,134] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.14)[2,2]
    totals_Z[i,135] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.15)[2,2]
    totals_Z[i,136] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.16)[2,2]
    totals_Z[i,137] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.17)[2,2]
    totals_Z[i,138] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.18)[2,2]
    totals_Z[i,139] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.19)[2,2]
    totals_Z[i,140] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.20)[2,2]
    totals_Z[i,141] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.21)[2,2]
    totals_Z[i,142] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.22)[2,2]
    totals_Z[i,143] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.23)[2,2]
    totals_Z[i,144] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.24)[2,2]
    totals_Z[i,145] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.25)[2,2]
    totals_Z[i,146] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.26)[2,2]
    totals_Z[i,147] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.27)[2,2]
    totals_Z[i,148] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.28)[2,2]
    totals_Z[i,149] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.29)[2,2]
    totals_Z[i,150] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.30)[2,2]
    totals_Z[i,151] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.31)[2,2]
    totals_Z[i,152] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.32)[2,2]
    totals_Z[i,153] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.33)[2,2]
    totals_Z[i,154] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.34)[2,2]
    totals_Z[i,155] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.35)[2,2]
    totals_Z[i,156] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.36)[2,2]
    totals_Z[i,157] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.37)[2,2]
    totals_Z[i,158] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.38)[2,2]
    totals_Z[i,159] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.39)[2,2]
    totals_Z[i,160] <- table(object[[i]][[3]]$pregnant,object[[i]][[3]]$imputation.40)[2,2]
  }
  
  # Pool cellproportions for constraind covariate Z and W 
  
  pool_Z           <- array(NA, dim=c(nsim, 90, 4))                                                                            
  dimnames(pool_Z) <- list(NULL, c("Qhat1" ,"Qhat2" ,"Qhat3" ,"Qhat4" ,"Qhat5" ,"Qhat6" ,"Qhat7" ,"Qhat8" ,"Qhat9" ,"Qhat10",
                                   "Qhat11","Qhat12","Qhat13","Qhat14","Qhat15","Qhat16","Qhat17","Qhat18","Qhat19","Qhat20",
                                   "Qhat21","Qhat22","Qhat23","Qhat24","Qhat25","Qhat26","Qhat27","Qhat28","Qhat29","Qhat30",
                                   "Qhat31","Qhat32","Qhat33","Qhat34","Qhat35","Qhat36","Qhat37","Qhat38","Qhat39","Qhat40",
                                   "Uhat1", "Uhat2" ,"Uhat3" ,"Uhat4" ,"Uhat5" ,"Uhat6" ,"Uhat7" ,"Uhat8" ,"Uhat9" ,"Uhat10",
                                   "Uhat11","Uhat12","Uhat13","Uhat14","Uhat15","Uhat16","Uhat17","Uhat18","Uhat19","Uhat20",
                                   "Uhat21","Uhat22","Uhat23","Uhat24","Uhat25","Uhat26","Uhat27","Uhat28","Uhat29","Uhat30",
                                   "Uhat31","Uhat32","Uhat33","Uhat34","Uhat35","Uhat36","Uhat37","Uhat38","Uhat39","Uhat40",
                                   "Qbar","Ubar","B","T","sqrtT","ll","ul",
                                   "ci","bias","cov"), 
                           c("Z1W1","Z1W2","Z2W1","Z2W2"))
  
  for (h in 1:40)
  {
    pool_Z[,h,1] <- totals_Z[,h   ]/ssize        
    pool_Z[,h,2] <- totals_Z[,h+40 ]/ssize                 
    pool_Z[,h,3] <- totals_Z[,h+80]/ssize
    pool_Z[,h,4] <- totals_Z[,h+120]/ssize
  }
  
  
  for(i in 1:nsim)
  {
    for(k in 1:4)
    {
      for(j in 41:80)
      {
        pool_Z[i,j,k] <- (pool_Z[i,j-40,k]*(1-pool_Z[i,j-40,k]))/ssize
      }
      pool_Z[i,"Qbar",k]   <- mean(pool_Z[i,1:40,k])                                   
      pool_Z[i,"Ubar",k]   <- mean(pool_Z[i,41:80,k])                                  
      pool_Z[i,"B",k]      <- var(pool_Z[i,1:40,k])           
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
  
  results_Z <- matrix(NA,4,90)
  dimnames(results_Z) <- list(c("Z1W1","Z1W2","Z2W1","Z2W2"),
                              c("Qhat1" ,"Qhat2" ,"Qhat3" ,"Qhat4" ,"Qhat5" ,"Qhat6" ,"Qhat7" ,"Qhat8" ,"Qhat9" ,"Qhat10",
                                "Qhat11","Qhat12","Qhat13","Qhat14","Qhat15","Qhat16","Qhat17","Qhat18","Qhat19","Qhat20",
                                "Qhat21","Qhat22","Qhat23","Qhat24","Qhat25","Qhat26","Qhat27","Qhat28","Qhat29","Qhat30",
                                "Qhat31","Qhat32","Qhat33","Qhat34","Qhat35","Qhat36","Qhat37","Qhat38","Qhat39","Qhat40",
                                "Uhat1", "Uhat2" ,"Uhat3" ,"Uhat4" ,"Uhat5" ,"Uhat6" ,"Uhat7" ,"Uhat8" ,"Uhat9" ,"Uhat10",
                                "Uhat11","Uhat12","Uhat13","Uhat14","Uhat15","Uhat16","Uhat17","Uhat18","Uhat19","Uhat20",
                                "Uhat21","Uhat22","Uhat23","Uhat24","Uhat25","Uhat26","Uhat27","Uhat28","Uhat29","Uhat30",
                                "Uhat31","Uhat32","Uhat33","Uhat34","Uhat35","Uhat36","Uhat37","Uhat38","Uhat39","Uhat40",
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
  #pool_Q           <- array(NA, dim=c(nsim, 30, 2))                                                                            
  #dimnames(pool_Q) <- list(NULL, c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
  #                                 "Qhat6","Qhat7","Qhat8","Qhat9","Qhat10",
  #                                 "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",
  #                                 "Uhat6","Uhat7","Uhat8","Uhat9","Uhat10",
  #                                 "Qbar","Ubar","B","T","sqrtT","ll","ul",
  #                                 "ci","bias","cov"), 
  #                         c("intercept","coefficient"))
  #
  #for(i in 1:nsim)
  #{
  #  object[[i]][[3]]$sport <- ifelse(object[[i]][[3]]$sport==2,0,1)
  #  
  #  gl1 <- glm(formula = sport ~ imputation.1, family="binomial", object[[i]][[3]])
  #  gl2 <- glm(formula = sport ~ imputation.2, family="binomial", object[[i]][[3]])
  #  gl3 <- glm(formula = sport ~ imputation.3, family="binomial", object[[i]][[3]])
  #  gl4 <- glm(formula = sport ~ imputation.4, family="binomial", object[[i]][[3]])
  #  gl5 <- glm(formula = sport ~ imputation.5, family="binomial", object[[i]][[3]])
  #  gl6 <- glm(formula = sport ~ imputation.6, family="binomial", object[[i]][[3]])
  #  gl7 <- glm(formula = sport ~ imputation.7, family="binomial", object[[i]][[3]])
  #  gl8 <- glm(formula = sport ~ imputation.8, family="binomial", object[[i]][[3]])
  #  gl9 <- glm(formula = sport ~ imputation.9, family="binomial", object[[i]][[3]])
  #  gl10 <- glm(formula = sport ~ imputation.10, family="binomial", object[[i]][[3]])
  #  
  #  pool_Q[i,"Qhat1",1:2] <- summary(gl1)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat2",1:2] <- summary(gl2)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat3",1:2] <- summary(gl3)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat4",1:2] <- summary(gl4)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat5",1:2] <- summary(gl5)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat6",1:2] <- summary(gl6)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat7",1:2] <- summary(gl7)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat8",1:2] <- summary(gl8)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat9",1:2] <- summary(gl9)$coefficients[1:2,1]
  #  pool_Q[i,"Qhat10",1:2] <- summary(gl10)$coefficients[1:2,1]
  #  
  #  pool_Q[i,"Uhat1",1:2] <- summary(gl1)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat2",1:2] <- summary(gl2)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat3",1:2] <- summary(gl3)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat4",1:2] <- summary(gl4)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat5",1:2] <- summary(gl5)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat6",1:2] <- summary(gl6)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat7",1:2] <- summary(gl7)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat8",1:2] <- summary(gl8)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat9",1:2] <- summary(gl9)$coefficients[1:2,2]
  #  pool_Q[i,"Uhat10",1:2] <- summary(gl10)$coefficients[1:2,2]
  #  
  #  for(j in 1:2){
  #    pool_Q[i,"Qbar",j]   <- mean(pool_Q[i,1:10,j])                                   
  #    pool_Q[i,"Ubar",j]   <- mean(pool_Q[i,11:20,j]^2)                                  
  #    pool_Q[i,"B",j]      <- var(pool_Q[i,1:10,j])           
  #    pool_Q[i,"T",j]      <- pool_Q[i,"Ubar",j]+(nboot+1)*(pool_Q[i,"B",j]/nboot)          
  #    pool_Q[i,"sqrtT",j]  <- sqrt(pool_Q[i,"T",j])
  #    pool_Q[i,"ll",j]     <- pool_Q[i,"Qbar",j]-qt(.975,ssize-1)*(pool_Q[i,"sqrtT",j])              
  #    pool_Q[i,"ul",j]     <- pool_Q[i,"Qbar",j]+qt(.975,ssize-1)*(pool_Q[i,"sqrtT",j])         
  #    pool_Q[i,"ci",j]     <- pool_Q[i,"ul",j]-pool_Q[i,"ll",j]                  
  #  }
  #  
  #  for (k in 1:2)
  #  {
  #    pool_Q[i,"bias",k] <- pool_Q[i,"Qbar",k] - pop_Q[k,]
  #    pool_Q[i,"cov",k] <- pool_Q[i,"ll",k] < pop_Q[k,] & pop_Q[k,] < pool_Q[i,"ul",k]
  #  }
  #}
  
  #results_Q <- matrix(NA,2,30)
  
  #dimnames(results_Q) <- list(c("intercept","coefficient"),
  #                            c("Qhat1","Qhat2","Qhat3","Qhat4","Qhat5",
  #                              "Qhat6","Qhat7","Qhat8","Qhat9","Qhat10",
  #                              "Uhat1","Uhat2","Uhat3","Uhat4","Uhat5",
  #                              "Uhat6","Uhat7","Uhat8","Uhat9","Uhat10",
  #                              "Qbar","Ubar","B","T","sqrtT","ll","ul",
  #                              "ci","bias","cov"))
  #bias_var <- matrix(NA,2,1)
  #
  #for(m in 1:2)
  #{
  #  results_Q[m,] <- apply(pool_Q[,,m],2,mean)
  #  bias_var[m,] <- results_Q[m,"sqrtT"]/sd(pool_Q[,"Qbar",m])
  #}
  #results_Q <- cbind(results_Q,bias_var)
  
  
  
  setTxtProgressBar(pb, i)
  close(pb)
  
  return(list(pool_Z = pool_Z, 
              results_Z = results_Z))
}