impute.samples <- function(posteriors, list_short_samples, i,ssize) {
  
  new.matrix <- NULL
  
  for (j in 1:32){
    
    old.matrix <- matrix(rep(posteriors[j,1:10],list_short_samples[[i]][j,6]),list_short_samples[[i]][j,6],10, byrow=T)
    new.matrix <- rbind(new.matrix, old.matrix)
  }
  
  write.table(new.matrix, file ="posterior.probabilities.txt", row.names = FALSE, col.names = TRUE, quote=FALSE)
  posterior.probabilities <- read.table("posterior.probabilities.txt", header=TRUE)
  
  for (k in 1:ssize){
    for (m in 1:5){
      posterior.probabilities[k,m+10] <- rbinom(1, 1, posterior.probabilities[k,m+5])                                                          
    }
  }
  
  colnames(posterior.probabilities) <- c("sport","pregnant","g1","g2","g3","posterior.1","posterior.2","posterior.3","posterior.4","posterior.5",
                                         "imputation.1","imputation.2","imputation.3","imputation.4","imputation.5")
  
  return(posterior.probabilities)
}


