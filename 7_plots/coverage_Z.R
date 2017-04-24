library(ggplot2)
options(scipen=999)

scens <- list(
  ss     =c(1000, 10000),
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

tabel1cov1 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
tabel1cov2 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
tabel1cov3 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
tabel1cov4 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
cells <- rep(c("1","2","3","4"), each=length(list.out))

for(i in 1:length(list.out)){
  tabel1cov1[i,5] <- list.out[[i]][1,9]
  tabel1cov2[i,5] <- list.out[[i]][2,9]
  tabel1cov3[i,5] <- list.out[[i]][3,9]
  tabel1cov4[i,5] <- list.out[[i]][4,9]
}

tabelcov1 <- rbind(tabel1cov1,tabel1cov2,tabel1cov3,tabel1cov4)
tabelcov1 <- cbind(tabelcov1, cells)


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

tabel3cov1 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
tabel3cov2 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
tabel3cov3 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
tabel3cov4 <- cbind(scen.mat,cov=rep(NA,length(list.out)))
cells <- rep(c("1","2","3","4"), each=length(list.out))

for(i in 1:length(list.out)){
  tabel3cov1[i,5] <- list.out[[i]][1,20]
  tabel3cov2[i,5] <- list.out[[i]][2,20]
  tabel3cov3[i,5] <- NA
  tabel3cov4[i,5] <- list.out[[i]][4,20]
}

tabelcov3 <- rbind(tabel3cov1,tabel3cov2,tabel3cov3,tabel3cov4)
tabelcov3 <- cbind(tabelcov3, cells)

##

models    <- rep(c("one indicator",
                   "restricted conditional model"), each=4*length(list.out))

tabelcov <- rbind(tabelcov1,tabelcov3)
tabelcov <- cbind(tabelcov, models)

tabelcov$models2 <- factor(tabelcov$models, 
                            as.character(tabelcov$models))

tabelcov$imp <- factor(tabelcov$imp, labels = c("P(Z=2)=0.01", 
                                                  "P(Z=2)=0.05", 
                                                  "P(Z=2)=0.10", 
                                                  "P(Z=2)=0.20"))
##


tabelcov$ss <- factor(tabelcov$ss, labels = c("sample size 1,000",
                                                "sample size 10,000"))

tabelcov$cells <- factor(tabelcov$cells, labels = c("W1 * Z1 (restricted)",
                                                      "W2 * Z1 (restricted)",
                                                      "W1 * Z2 (restricted)",
                                                      "W2 * Z2 (restricted)"))


onein <- subset(tabelcov, models=="one indicator")
restr <- subset(tabelcov, models=="restricted conditional model")

onein$cells <- factor(onein$cells, labels = c("Y1 1 * Z1",
                                              "Y1 2 * Z1",
                                              "Y1 1 * Z2",
                                              "Y1 2 * Z2"))

tabelcov <- rbind(onein, restr)

# GREY
p <- ggplot(tabelcov, aes(factor(c.prob), cov))
p + facet_grid(imp~ss) + 
  theme_bw() + 
  geom_point(aes(shape=factor(cells)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,18,0,1,2,5)) +
  guides(shape = guide_legend("cells")) +
  geom_hline(yintercept = 0.95) +
  scale_colour_manual(values=palette) +
  xlab("classification probabilities") +
  ylab("coverage of 95 percent confidence interval")
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))






