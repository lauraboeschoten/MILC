setwd("D:\\Surfdrive\\Project 1\\datapackage\\5_application\\3_restricted_conditional_model")
set.seed(123)

lc1 <- read.table("LC1.txt", comment.char="", header=TRUE)
lc2 <- read.table("LC2.txt", comment.char="", header=TRUE)
lc3 <- read.table("LC3.txt", comment.char="", header=TRUE)
lc4 <- read.table("LC4.txt", comment.char="", header=TRUE)
lc5 <- read.table("LC5.txt", comment.char="", header=TRUE)
samples <- read.table("samples.txt", comment.char="", header=TRUE)

samples$LC1 <- lc1[,"Cluster.1"]
samples$LC2 <- lc2[,"Cluster.1"]
samples$LC3 <- lc3[,"Cluster.1"]
samples$LC4 <- lc4[,"Cluster.1"]
samples$LC5 <- lc5[,"Cluster.1"]

samples       <- as.data.frame(samples)

write.table(samples, "LCsamples.txt", na=".")
samples <- read.table("LCsamples.txt", na=".", header=TRUE)
samples <- as.data.frame(samples)

#trekken met de clusterkansen
new.samples   <- NULL
sampp         <- as.matrix(samples)

for (i in 1:nrow(samples)){
  old.samples <- matrix(rep(sampp[i,1:16],samples[i,6]), ncol=16, nrow=samples[i,6], byrow=T)
  new.samples <- rbind(new.samples,old.samples)
}

new.samples <- as.data.frame(new.samples)
colnames(new.samples) <- c("woningREG","woningBACK","woningHOUSE","burgstat","toeslag","original",
                           "b1","b2","b3","b4","b5","p1","p2","p3","p4","p5")
new.samples$p1   <- as.numeric(as.character(new.samples$p1))
new.samples$p2   <- as.numeric(as.character(new.samples$p2))
new.samples$p3   <- as.numeric(as.character(new.samples$p3))
new.samples$p4   <- as.numeric(as.character(new.samples$p4))
new.samples$p5   <- as.numeric(as.character(new.samples$p5))

for (i in 1:nrow(new.samples)){
  for (j in 1:5){
    new.samples[i,j+16] <- rbinom(1, 1, new.samples[i,j+11])
  }
}

colnames(new.samples) <- c("woningREG","woningBACK","woningHOUSE","burgstat","toeslag","original",
                           "b1","b2","b3","b4","b5","p1","p2","p3","p4","p5","i1","i2","i3","i4","i5")



new.samples$i1 <- as.factor(new.samples$i1)
new.samples$i2 <- as.factor(new.samples$i2)
new.samples$i3 <- as.factor(new.samples$i3)
new.samples$i4 <- as.factor(new.samples$i4)
new.samples$i5 <- as.factor(new.samples$i5)

levels(new.samples$i1) <- c("koop","huur")
levels(new.samples$i2) <- c("koop","huur")
levels(new.samples$i3) <- c("koop","huur")
levels(new.samples$i4) <- c("koop","huur")
levels(new.samples$i5) <- c("koop","huur")

write.table(new.samples, "imp.samples.txt")
