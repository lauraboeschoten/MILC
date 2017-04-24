impute.samples <- function(posteriors, list_short_samples, i,ssize) {
  
  new.matrix <- NULL
  
  
  if(nrow(list_short_samples[[i]])!=nrow(posteriors)){
    add.on <- matrix(rep(0,(nrow(posteriors)-nrow(list_short_samples[[i]]))*ncol(list_short_samples[[i]])),
                     nrow=nrow(posteriors)-nrow(list_short_samples[[i]]))
    
    colnames(add.on) <- names(list_short_samples[[i]])
    
    list_short_samples[[i]] <- rbind(list_short_samples[[i]],add.on)}
  
  
  for (j in 1:nrow(posteriors)){
    
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