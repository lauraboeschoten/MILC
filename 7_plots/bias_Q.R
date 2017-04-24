library(ggplot2)
options(scipen=999)

scens <- list(
  ss     =c(1000),
  c.prob =c(0.70, 0.80, 0.90, 0.95, 0.99),
  imp    =c(0.01),
  co.coef=c(0.45, 0.55, 0.65))

scen.mat <- expand.grid(scens) 
list.out <- vector("list",nrow(scen.mat))

# 1 indicator model
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

# obtain bias of coefficient 
tabelbias1 <- cbind(scen.mat, bias=rep(NA,length(list.out)))

for(i in 1:length(list.out)){
  tabelbias1[i,5] <- list.out[[i]][2,8]  
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

# obtain bias of coefficient 
tabelbias3 <- cbind(scen.mat, bias=rep(NA,length(list.out)))

for(i in 1:length(list.out)){
  tabelbias3[i,5] <- list.out[[i]][2,19]  
}

## 

models    <- rep(c("one indicator", "restricted conditional model"), each=length(list.out))
tabelbias <- rbind(tabelbias1,tabelbias3)
tabelbias <- cbind(tabelbias, models)
tabelbias$models2 <- factor(tabelbias$models, as.character(tabelbias$models))
tabelbias$co.coef <- factor(tabelbias$co.coef, labels = c("-0.2007 (W*Z restricted)",
                                                          "0.2007 (W*Z restricted)",
                                                          "0.6190 (W*Z restricted)"))
onein <- subset(tabelbias, models=="one indicator")
restr <- subset(tabelbias, models=="restricted conditional model")
onein$co.coef <- factor(onein$co.coef, labels = c("-0.2007 (Y*Z)",
                                                "0.2007 (Y*Z)",
                                                "0.6190 (Y*Z)"))
tabelbias <- rbind(onein, restr)


p <- ggplot(tabelbias, aes(factor(c.prob), bias))
p + theme_bw() + 
  geom_point(aes(shape=factor(co.coef)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,0,1,2)) +
  guides(shape = guide_legend("coefficients")) +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=palette) +
  xlab("classification probabilities") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))
