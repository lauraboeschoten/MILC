create.bootstrap.samples <- function(nsim, nboot, ssize, list_short_samples) {
  
  #Amount of columns in list_short_samples
  len.lss <- length(list_short_samples[[1]])
  
  #Make array for bootstrap collection
  bootstrap.samples           <- array(NA, dim=c(nrow(list_short_samples[[1]]),
                                                 len.lss+nboot-1, nsim))                                    
  
  #Add names for array (based on list_short_samples and amount of n.boot)
  colnames(bootstrap.samples) <- c(names(list_short_samples[[1]])[1:(len.lss-1)],
                                   paste0("b",seq(1:nboot)))    
  
  for (k in 1:nsim){   
    #Add whole dataset in one go to each layer. Based on ncol of list_short_samples-1.
    bootstrap.samples[,1:(len.lss-1),k] <- as.matrix(list_short_samples[[k]][,-len.lss])
    
    #Take bootstrap samples (no matter wat ncol of dataset)
    bootstrap.samples[,(len.lss:(len.lss+nboot-1)),k] <- rmultinom(nboot, ssize, 
                                                                    list_short_samples[[k]][,len.lss]/
                                                                      sum(list_short_samples[[k]][,len.lss]))           
  }                                                                             
  
  return(bootstrap.samples)
}
