posterior.probabilities <- function(conditionals) {
  
  v1 <- v2 <- v3 <- v4 <- v5 <- c(1,2)
  
  combinations           <- expand.grid(v1=v1,v2=v2,v3=v3,v4=v4,v5=v5)
  combinations           <- combinations[, rev(seq_len(ncol(combinations)))]
  colnames(combinations) <- c("sport","pregnant","g1","g2","g3")
  
  combinations$posterior.1 <-   NA   #CHANGE
  combinations$posterior.2 <-   NA
  combinations$posterior.3 <-   NA
  combinations$posterior.4 <-   NA
  combinations$posterior.5 <-   NA
  combinations$posterior.6 <-   NA
  combinations$posterior.7 <-   NA
  combinations$posterior.8 <-   NA
  combinations$posterior.9 <-   NA
  combinations$posterior.10 <-  NA
  combinations$posterior.11 <-   NA   #CHANGE
  combinations$posterior.12 <-   NA
  combinations$posterior.13 <-   NA
  combinations$posterior.14 <-   NA
  combinations$posterior.15 <-   NA
  combinations$posterior.16 <-   NA
  combinations$posterior.17 <-   NA
  combinations$posterior.18 <-   NA
  combinations$posterior.19 <-   NA
  combinations$posterior.20 <-  NA
  combinations$posterior.21 <-   NA   #CHANGE
  combinations$posterior.22 <-   NA
  combinations$posterior.23 <-   NA
  combinations$posterior.24 <-   NA
  combinations$posterior.25 <-   NA
  combinations$posterior.26 <-   NA
  combinations$posterior.27 <-   NA
  combinations$posterior.28 <-   NA
  combinations$posterior.29 <-   NA
  combinations$posterior.30 <-  NA
  combinations$posterior.31 <-   NA   #CHANGE
  combinations$posterior.32 <-   NA
  combinations$posterior.33 <-   NA
  combinations$posterior.34 <-   NA
  combinations$posterior.35 <-   NA
  combinations$posterior.36 <-   NA
  combinations$posterior.37 <-   NA
  combinations$posterior.38 <-   NA
  combinations$posterior.39 <-   NA
  combinations$posterior.40 <-  NA
  
  for (a in 1:nboot) {
    for (b in 1:ncomb) {
      pr1a  <- conditionals[[a]][1,1]
      if(combinations[b,1]==1) {cov1a <- conditionals[[a]][2,1]} else {cov1a <- conditionals[[a]][3,1]}
      if(combinations[b,2]==1) {cov2a <- conditionals[[a]][4,1]} else {cov2a <- conditionals[[a]][5,1]}
      if(combinations[b,3]==1) {ind1a <- conditionals[[a]][6,1]} else {ind1a <- conditionals[[a]][7,1]}  
      if(combinations[b,4]==1) {ind2a <- conditionals[[a]][8,1]} else {ind2a <- conditionals[[a]][9,1]}  
      if(combinations[b,5]==1) {ind3a <- conditionals[[a]][10,1]} else {ind3a <- conditionals[[a]][11,1]}  
      pr1b  <- conditionals[[a]][1,2]
      if(combinations[b,1]==1) {cov1b <- conditionals[[a]][2,2]} else {cov1b <- conditionals[[a]][3,2]}
      if(combinations[b,2]==1) {cov2b <- conditionals[[a]][4,2]} else {cov2b <- conditionals[[a]][5,2]}
      if(combinations[b,3]==1) {ind1b <- conditionals[[a]][6,2]} else {ind1b <- conditionals[[a]][7,2]}  
      if(combinations[b,4]==1) {ind2b <- conditionals[[a]][8,2]} else {ind2b <- conditionals[[a]][9,2]}  
      if(combinations[b,5]==1) {ind3b <- conditionals[[a]][10,2]} else {ind3b <- conditionals[[a]][11,2]} 
      
      combinations[b,a+5] <- (pr1a*cov1a*cov2a*ind1a*ind2a*ind3a)/((pr1a*cov1a*cov2a*ind1a*ind2a*ind3a)+(pr1b*cov1b*cov2b*ind1b*ind2b*ind3b))
    }
  }
  
  return(combinations)
}
