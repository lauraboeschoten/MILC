conditional.probabilities  <- function(model){
  
  #Make collection matrix for cond.probs
  cond.probs           <- matrix(NA, 11, 2)
  
  ##Read file
  scan.mod <- readLines(model)
  
  #Find row where only appearance of Size appears in the data
  lin.size  <- grep('Size',scan.mod)
  
  # Grab row where 8th occurence of sport is in model file
  lin.sport  <- grep('sport',scan.mod)[8]
  
  ## Grab relevant values from scan.mod using lines identified above
  
  cond.probs[1,1:2]  <- strsplit(scan.mod[lin.size], "\t")[[1]][c(2,4)]
  cond.probs[2:3,1]  <- strsplit(scan.mod[lin.sport+1], "\t")[[1]][2:3]
  cond.probs[2:3,2]  <- strsplit(scan.mod[lin.sport+2], "\t")[[1]][2:3]
  cond.probs[4:5,1]  <- strsplit(scan.mod[lin.sport+4], "\t")[[1]][2:3]
  
  #If condition for if 2nd row of pregnant is missing in the file
  if(strsplit(scan.mod[lin.sport+5], "\t")[[1]][1]==2){
  cond.probs[4:5,2]  <- strsplit(scan.mod[lin.sport+5], "\t")[[1]][2:3]
  } else {cond.probs[4:5,2] <- c(1,1)}
  
  cond.probs[6,1:2]  <- strsplit(scan.mod[lin.size+2], "\t")[[1]][c(2,4)]
  cond.probs[7,1:2]  <- strsplit(scan.mod[lin.size+3], "\t")[[1]][c(2,4)]
  cond.probs[8,1:2]  <- strsplit(scan.mod[lin.size+5], "\t")[[1]][c(2,4)]
  cond.probs[9,1:2]  <- strsplit(scan.mod[lin.size+6], "\t")[[1]][c(2,4)]
  cond.probs[10,1:2] <- strsplit(scan.mod[lin.size+8], "\t")[[1]][c(2,4)]
  cond.probs[11,1:2] <- strsplit(scan.mod[lin.size+9], "\t")[[1]][c(2,4)]
  
  #Make as numeric and give names
  cond.probs <- data.frame(cl1=as.numeric(cond.probs[,1]),cl2=as.numeric(cond.probs[,2]),
                           row.names=c("size","s1","s2","p1","p2","1g1","1g2","2g1","2g2","3g1","3g2"))
  return(cond.probs)
}

conditional.probabilities.2 <- function(){
  
  profiles <- vector("list",nboot) 
  
  for(b in 1:nboot){
    nam.p <- paste0("model.",b,".lst")
    profiles[[b]] <- conditional.probabilities(nam.p)
  }
  
  return(profiles)
}
