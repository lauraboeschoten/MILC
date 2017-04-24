library(ggplot2)
options(scipen=999)

scens <- list(
  ss     =c(1000, 10000),
  c.prob =c(0.70, 0.80, 0.90, 0.95, 0.99),
  imp    =c(0.01),
  co.coef=c(0.45, 0.55, 0.65))

scen.mat <- expand.grid(scens) 
list.out <- vector("list",nrow(scen.mat))

# model 1 indicator
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

# obtain coverage of coefficient 
tabelcov1 <- cbind(scen.mat, cov=rep(NA,length(list.out)))

for(i in 1:length(list.out)){
  tabelcov1[i,5] <- list.out[[i]][2,9]  
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
tabelcov3 <- cbind(scen.mat, cov=rep(NA,length(list.out)))

for(i in 1:length(list.out)){
  tabelcov3[i,5] <- list.out[[i]][2,20]  
}

models    <- rep(c("one indicator",
                   "restricted conditional model"), each=length(list.out))

tabelcov <- rbind(tabelcov1,tabelcov3)
tabelcov <- cbind(tabelcov, models)

tabelcov$models2 <- factor(tabelcov$models, 
                           as.character(tabelcov$models))

tabelcov$imp <- factor(tabelcov$imp, labels = c("P(Z=2)=0.01", 
                                                "P(Z=2)=0.05", 
                                                "P(Z=2)=0.10", 
                                                "P(Z=2)=0.20"))

tabelcov$ss <- factor(tabelcov$ss, labels = c("sample size 1,000",
                                              "sample size 10,000"))

tabelcov$co.coef <- factor(tabelcov$co.coef, labels = c("-0.2007 (W*Z restricted)",
                                                          "0.2007 (W*Z restricted)",
                                                          "0.6190 (W*Z restricted)"))

onein <- subset(tabelcov, models=="one indicator")
restr <- subset(tabelcov, models=="restricted conditional model")

onein$co.coef <- factor(onein$co.coef, labels = c("-0.2007 (Y*Z)",
                                                  "0.2007 (Y*Z)",
                                                  "0.6190 (Y*Z)"))

tabelcov <- rbind(onein, restr)
# GREY
p <- ggplot(tabelcov, aes(factor(c.prob), cov))
p + facet_wrap(~ss) + 
  theme_bw() + 
  geom_point(aes(shape=factor(co.coef)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_fill_manual(values=c("black", NA)) + 
  scale_shape_manual(values=c(15,16,17,0,1,2)) +
  guides(shape = guide_legend("coefficients")) +
  geom_hline(yintercept = 0.95) +
  scale_colour_manual(values=palette) +
  xlab("classification probabilities") +
  ylab("coverage of 95 percent confidence interval") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))
#p + guides(fill = guide_legend(override.aes = list(shape = 21), "models"))
