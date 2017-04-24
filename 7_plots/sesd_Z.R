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

tabel1sesd1 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
tabel1sesd2 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
tabel1sesd3 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
tabel1sesd4 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
cells <- rep(c("1","2","3","4"), each=length(list.out))

for(i in 1:length(list.out)){
  tabel1sesd1[i,5] <- list.out[[i]][1,10]
  tabel1sesd2[i,5] <- list.out[[i]][2,10]
  tabel1sesd3[i,5] <- list.out[[i]][3,10]
  tabel1sesd4[i,5] <- list.out[[i]][4,10]
}

tabelsesd1 <- rbind(tabel1sesd1,tabel1sesd2,tabel1sesd3,tabel1sesd4)
tabelsesd1 <- cbind(tabelsesd1, cells)

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

tabel3sesd1 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
tabel3sesd2 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
tabel3sesd3 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
tabel3sesd4 <- cbind(scen.mat,sesd=rep(NA,length(list.out)))
cells <- rep(c("1","2","3","4"), each=length(list.out))

for(i in 1:length(list.out)){
  tabel3sesd1[i,5] <- list.out[[i]][1,21]
  tabel3sesd2[i,5] <- list.out[[i]][2,21]
  tabel3sesd3[i,5] <- NA
  tabel3sesd4[i,5] <- list.out[[i]][4,21]
}

tabelsesd3 <- rbind(tabel3sesd1,tabel3sesd2,tabel3sesd3,tabel3sesd4)
tabelsesd3 <- cbind(tabelsesd3, cells)

##

models    <- rep(c("one indicator",
                   "restricted conditional model"), each=4*length(list.out))

tabelsesd <- rbind(tabelsesd1,tabelsesd3)
tabelsesd <- cbind(tabelsesd, models)

tabelsesd$models2 <- factor(tabelsesd$models, 
                           as.character(tabelsesd$models))

tabelsesd$imp <- factor(tabelsesd$imp, labels = c("P(Z=2)=0.01", 
                                                "P(Z=2)=0.05", 
                                                "P(Z=2)=0.10", 
                                                "P(Z=2)=0.20"))
##


tabelsesd$ss <- factor(tabelsesd$ss, labels = c("sample size 1,000",
                                                "sample size 10,000"))

tabelsesd$cells <- factor(tabelsesd$cells, labels = c("W1 * Z1 (restricted)",
                                                    "W2 * Z1 (restricted)",
                                                    "W1 * Z2 (restricted)",
                                                    "W2 * Z2 (restricted)"))

onein <- subset(tabelsesd, models=="one indicator")
restr <- subset(tabelsesd, models=="restricted conditional model")

onein$cells <- factor(onein$cells, labels = c("Y1 1 * Z1",
                                              "Y1 2 * Z1",
                                              "Y1 1 * Z2",
                                              "Y1 2 * Z2"))

tabelsesd <- rbind(onein, restr)

# GREY
palette <- c( "#252525", "#737373", "#bdbdbd")
p <- ggplot(tabelsesd, aes(factor(c.prob), sesd))
p + facet_wrap(~imp) + 
  theme_bw() + 
  geom_point(aes(shape=factor(cells)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,18,0,1,2,5)) +
  guides(shape = guide_legend("cells")) +
  geom_hline(yintercept = 1) +
  scale_colour_manual(values=palette) +
  xlab("classification probabilities") +
  ylab(expression(paste("se/sd(", theta,")"))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))






