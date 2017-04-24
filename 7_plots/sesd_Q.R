library(ggplot2)
options(scipen=999)

scens <- list(
  ss     =c(1000, 10000),
  c.prob =c(0.70, 0.80, 0.90, 0.95, 0.99),
  imp    =c(0.01, 0.05, 0.10, 0.20),
  co.coef=c(0.45, 0.55, 0.65))

scen.mat <- expand.grid(scens) 
list.out <- vector("list",nrow(scen.mat))

# model 1
setwd("D:\\Surfdrive\\Project 1\\datapackage\\4_one_indicator_used")

for(a in 1:nrow(scen.mat)){
  nam.load <- paste0("results_Q",scen.mat[a,1],"_",
                     substring(scen.mat[a,2], 3),"_",
                     substring(scen.mat[a,3], 3),"_",
                     substring(scen.mat[a,4], 3),".txt") 
  nam.out.data <- read.table(paste0(nam.load)) 
  list.out[[a]] <- nam.out.data
  names(list.out)[a] <- nam.load
}

# obtain sesd of coefficient 
tabelsesd1 <- cbind(scen.mat, sesd=rep(NA,length(list.out)))

for(i in 1:length(list.out)){
  tabelsesd1[i,5] <- list.out[[i]][2,10]  
}


# model 3
setwd("D:\\Surfdrive\\Project 1\\datapackage\\3_restricted_conditional_model")

for(a in 1:nrow(scen.mat)){
  nam.load <- paste0("sim_res_",scen.mat[a,1],"_",
                     substring(scen.mat[a,2], 3),"_",
                     substring(scen.mat[a,3], 3),"_",
                     substring(scen.mat[a,4], 3),"poolQ.txt") 
  nam.out.data <- read.table(paste0(nam.load)) 
  list.out[[a]] <- nam.out.data
  names(list.out)[a] <- nam.load
}

# obtain coverage of coefficient 
tabelsesd3 <- cbind(scen.mat, sesd=rep(NA,length(list.out)))

for(i in 1:length(list.out)){
  tabelsesd3[i,5] <- list.out[[i]][2,21]  
}

models    <- rep(c("one indicator",
                   "restricted conditional model"), each=length(list.out))

tabelsesd <- rbind(tabelsesd1,tabelsesd3)
tabelsesd <- cbind(tabelsesd, models)

tabelsesd$models2 <- factor(tabelsesd$models, 
                            as.character(tabelsesd$models))

tabelsesd$imp <- factor(tabelsesd$imp, labels = c("P(Z=2)=0.01", 
                                                "P(Z=2)=0.05", 
                                                "P(Z=2)=0.10", 
                                                "P(Z=2)=0.20"))

tabelsesd$ss <- factor(tabelsesd$ss, labels = c("sample size 1,000",
                                                "sample size 10,000"))

tabelsesd$co.coef <- factor(tabelsesd$co.coef, labels = c("-0.2007",
                                                        "0.2007",
                                                        "0.6190"))


# GREY
palette <- c( "#252525", "#737373", "#bdbdbd")
p <- ggplot(tabelsesd, aes(factor(c.prob), sesd))
p + facet_grid(ss~imp) + 
  theme_bw() + 
  geom_point(aes(colour = factor(models2), shape = factor(co.coef)), 
             position=position_jitter(width=0.4), size=6) + 
  guides(colour = guide_legend("models"), shape = guide_legend("coefficients")) +
  geom_hline(yintercept = 1) +
  scale_colour_manual(values=palette) +
  xlab("classification probabilities") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))
