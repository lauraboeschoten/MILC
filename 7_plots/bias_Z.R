library(ggplot2)
options(scipen=999)

scens <- list(
  ss     =c(1000),
  c.prob =c(0.70, 0.80, 0.90, 0.95, 0.99),
  imp    =c(0.01, 0.05, 0.10, 0.20),
  co.coef=c(0.50))

scen.mat <- expand.grid(scens) 
list.out <- vector("list",nrow(scen.mat))

# model 1
setwd("D:\\Surfdrive\\Project 1\\datapackage\\4_one_indicator_used")

for(a in 1:nrow(scen.mat)){
  nam.load <- paste0("results_Z",scen.mat[a,1],"_",
                     substring(scen.mat[a,2], 3),"_",
                     substring(scen.mat[a,3], 3),"_",
                     substring(scen.mat[a,4], 3),".txt") 
  nam.out.data <- read.table(paste0(nam.load)) 
  list.out[[a]] <- nam.out.data
  names(list.out)[a] <- nam.load
}

tabel1bias1 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
tabel1bias2 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
tabel1bias3 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
tabel1bias4 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
cells <- rep(c("1","2","3","4"), each=length(list.out))

for(i in 1:length(list.out)){
  tabel1bias1[i,5] <- list.out[[i]][1,8]
  tabel1bias2[i,5] <- list.out[[i]][2,8]
  tabel1bias3[i,5] <- list.out[[i]][3,8]
  tabel1bias4[i,5] <- list.out[[i]][4,8]
}

tabelbias1 <- rbind(tabel1bias1,tabel1bias2,tabel1bias3,tabel1bias4)
tabelbias1 <- cbind(tabelbias1, cells)


# model 3
setwd("D:\\Surfdrive\\Project 1\\datapackage\\3_restricted_conditional_model")

for(a in 1:nrow(scen.mat)){
  nam.load <- paste0("sim_res_",scen.mat[a,1],"_",
                     substring(scen.mat[a,2], 3),"_",
                     substring(scen.mat[a,3], 3),"_",
                     substring(scen.mat[a,4], 3),"poolZ.txt") 
  nam.out.data <- read.table(paste0(nam.load)) 
  list.out[[a]] <- nam.out.data
  names(list.out)[a] <- nam.load
}

tabel3bias1 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
tabel3bias2 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
tabel3bias3 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
tabel3bias4 <- cbind(scen.mat,bias=rep(NA,length(list.out)))
cells <- rep(c("1","2","3","4"), each=length(list.out))

for(i in 1:length(list.out)){
  tabel3bias1[i,5] <- list.out[[i]][1,19]
  tabel3bias2[i,5] <- list.out[[i]][2,19]
  tabel3bias3[i,5] <- list.out[[i]][3,19]
  tabel3bias4[i,5] <- list.out[[i]][4,19]
}

tabelbias3 <- rbind(tabel3bias1,tabel3bias2,tabel3bias3,tabel3bias4)
tabelbias3 <- cbind(tabelbias3, cells)

##

models    <- rep(c("one indicator", "restricted conditional model"), each=4*length(list.out))
tabelbias <- rbind(tabelbias1,tabelbias3)
tabelbias <- cbind(tabelbias, models)

tabelbias$models2 <- factor(tabelbias$models, 
                            as.character(tabelbias$models))

tabelbias$imp <- factor(tabelbias$imp, labels = c("P(Z=2)=0.01", 
                                                  "P(Z=2)=0.05", 
                                                  "P(Z=2)=0.10", 
                                                  "P(Z=2)=0.20"))
##


tabelbias$ss <- factor(tabelbias$ss, labels = c("sample size 1,000",
                                                "sample size 10,000"))

tabelbias$cells <- factor(tabelbias$cells, labels = c("W1 * Z1 (restricted)",
                                                      "W2 * Z1 (restricted)",
                                                      "W1 * Z2 (restricted)",
                                                      "W2 * Z2 (restricted)"))

onein <- subset(tabelbias, models=="one indicator")
restr <- subset(tabelbias, models=="restricted conditional model")
 
onein$cells <- factor(onein$cells, labels = c("Y1 1 * Z1",
                                              "Y1 2 * Z1",
                                              "Y1 1 * Z2",
                                              "Y1 2 * Z2"))

tabelbias <- rbind(onein, restr)

# GREY

p <- ggplot(tabelbias, aes(factor(c.prob), bias))
p + facet_wrap(~imp) + 
  theme_bw() + 
  geom_point(aes(shape=factor(cells)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,18,0,1,2,5)) +
  guides(shape = guide_legend("cells")) +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=palette) +
  xlab("classification probabilities") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))
