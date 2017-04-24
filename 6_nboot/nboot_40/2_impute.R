impute.samples <- function(posteriors, list_short_samples, i,ssize) {
  
  new.matrix <- NULL
  
  for (j in 1:32){  #CHANGE
    
    old.matrix <- matrix(rep(posteriors[j,1:45],list_short_samples[[i]][j,6]),list_short_samples[[i]][j,6],45, byrow=T)
    new.matrix <- rbind(new.matrix, old.matrix)
  }
  
  write.table(new.matrix, file ="posterior.probabilities.txt", row.names = FALSE, col.names = TRUE, quote=FALSE)
  posterior.probabilities <- read.table("posterior.probabilities.txt", header=TRUE)
  
  for (k in 1:ssize){
    for (m in 1:40){
      posterior.probabilities[k,m+45] <- rbinom(1, 1, posterior.probabilities[k,m+5])                                                          
    }
  }
  
  colnames(posterior.probabilities) <- c("sport","pregnant","g1","g2","g3",
                                         "posterior.1","posterior.2","posterior.3","posterior.4","posterior.5",
                                         "posterior.6","posterior.7","posterior.8","posterior.9","posterior.10",
                                         "posterior.11","posterior.12","posterior.13","posterior.14","posterior.15",
                                         "posterior.16","posterior.17","posterior.18","posterior.19","posterior.20",
                                         "posterior.21","posterior.22","posterior.23","posterior.24","posterior.25",
                                         "posterior.26","posterior.27","posterior.28","posterior.29","posterior.30",
                                         "posterior.31","posterior.32","posterior.33","posterior.34","posterior.35",
                                         "posterior.36","posterior.37","posterior.38","posterior.39","posterior.40",
                                         "imputation.1","imputation.2","imputation.3","imputation.4","imputation.5",
                                         "imputation.6","imputation.7","imputation.8","imputation.9","imputation.10",
                                         "imputation.11","imputation.12","imputation.13","imputation.14","imputation.15",
                                         "imputation.16","imputation.17","imputation.18","imputation.19","imputation.20",
                                         "imputation.21","imputation.22","imputation.23","imputation.24","imputation.25",
                                         "imputation.26","imputation.27","imputation.28","imputation.29","imputation.30",
                                         "imputation.31","imputation.32","imputation.33","imputation.34","imputation.35",
                                         "imputation.36","imputation.37","imputation.38","imputation.39","imputation.40")
  
  return(posterior.probabilities)
}


