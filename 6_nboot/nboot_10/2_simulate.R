simulation           <- function(samples, nsim, ssize, nboot, ncomb, brew.file,
                                 cp.min, cp.plus, imp.comp) {
  
  OUT                <- list(NA)
  list_samples       <- split(samples, samples$simulation)
  pb                 <- txtProgressBar(min = 0, max = nsim, style = 3)
  list_short_samples <- list(NA)
  
  for (s in 1:nsim) {
    list_short_samples[[s]] <- ddply(list_samples[[s]],
                              .(sport,pregnant,gender1,gender2,gender3),nrow, .drop=FALSE)
  }
  
  bootstrap.samples <- create.bootstrap.samples(nsim, nboot, ssize, list_short_samples)
  
  for(i in 1:nsim){
    write.table(bootstrap.samples[,,i], file=paste0("samples\\bootstrap_sample_",i,".txt"), 
                row.names=FALSE, quote=FALSE)
    
    #make outfile names
    outfiles           <- paste0("model.",1:nboot)
    
    #Get bootstrap colnames from bootstrap.samples
    bootstrap.colnames <-tail(colnames(bootstrap.samples),nboot)                                                 
    
    #Loop (generalized for no matter how many columns)
    for (j in 1:nboot){
      envir       <- new.env()
      run.template(template.path=brew.file, envir=envir, temp.filename.base=outfiles[j], 
                   i=i, j=j, bootstrap.colnames=bootstrap.colnames[j], cp.min=cp.min, 
                   cp.plus=cp.plus, imp.comp=imp.comp)
    }
    
    #HIER GEBLEVEN (kijken of name van write table nog van invloed)
    conditionals <- conditional.probabilities.2()
    
    expect_that(sum(conditionals[[1]][1,]), equals(1))                                        
    expect_that(sum(conditionals[[1]][6,1],conditionals[[1]][7,1]), equals(1))            
    expect_that(sum(conditionals[[1]][6,2],conditionals[[1]][7,2]), equals(1))            
    expect_that(sum(conditionals[[1]][8,1],conditionals[[1]][9,1]), equals(1))            
    expect_that(sum(conditionals[[1]][8,2],conditionals[[1]][9,2]), equals(1))            
    
    posteriors   <- posterior.probabilities(conditionals)
    imputations  <- impute.samples(posteriors, list_short_samples, i,ssize)                                               
    
    OUT[[i]]  <- list(conditionals, posteriors, imputations)
    setTxtProgressBar(pb, i)
  }
  close(pb)  
  return(OUT)
}

