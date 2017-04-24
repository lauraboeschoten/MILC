library(ggplot2)
library(Rmisc)
library(gridExtra)
options(scipen=999)

#Set global workspace
setwd("D:\\Surfdrive\\Project 1\\simulation\\sim_jan_nboot\\plots")

scens <- list(
  co.coef=c(0.50),
  imp    =c(0.1),
  c.prob =c(0.70, 0.80, 0.90, 0.95, 0.99),
  ss     =c(1000))

#nboot <- c(5, 10)

#Expand grid for scenarios  
scen.mat <- expand.grid(scens) 
scen.mat <- scen.mat[,rev(1:ncol(scen.mat))]
list.out5 <- vector("list",nrow(scen.mat))
list.out40 <- vector("list",nrow(scen.mat))

# TABEL1Z BIAS
setwd("D:\\Surfdrive\\Project 1\\simulation\\sim_mar\\model3")
for(a in 1:nrow(scen.mat)){
  nam.load <- paste0("sim_res_",scen.mat[a,1],"_",
                     substring(scen.mat[a,2], 3),"_",
                     substring(scen.mat[a,3], 3),"_",
                     substring(scen.mat[a,4], 3),"poolZ.txt") 
  nam.out.data <- read.table(paste0(nam.load)) 
  list.out5[[a]] <- nam.out.data
  names(list.out5)[a] <- paste0(nam.load,"_5")
}

setwd("D:\\Surfdrive\\Project 1\\simulation\\sim_jan_nboot\\nboot_40\\model3")
for(a in 1:nrow(scen.mat)){
  nam.load <- paste0("sim_res_",scen.mat[a,1],"_",
                     substring(scen.mat[a,2], 3),"_",
                     substring(scen.mat[a,3], 3),"Z.txt") 
  nam.out.data <- read.table(paste0(nam.load)) 
  list.out40[[a]] <- nam.out.data
  names(list.out40)[a] <- paste0(nam.load,"_40")
}

#
tabel1Zbias1 <- cbind(scen.mat,bias=rep(NA,(2*length(list.out5))))
tabel1Zbias2 <- cbind(scen.mat,bias=rep(NA,(2*length(list.out5))))
tabel1Zbias3 <- cbind(scen.mat,bias=rep(NA,(2*length(list.out5))))
tabel1Zbias4 <- cbind(scen.mat,bias=rep(NA,(2*length(list.out5))))
definebias <- rep(c("1","2","3","4"), each=(2*length(list.out5)))
definenboot <- rep(c(5,5,5,5,5,40,40,40,40,40),4)

for(i in 1:length(list.out5)){
  tabel1Zbias1[i,5] <- list.out5[[i]][1,19]
  tabel1Zbias2[i,5] <- list.out5[[i]][2,19]
  tabel1Zbias3[i,5] <- list.out5[[i]][3,19]
  tabel1Zbias4[i,5] <- list.out5[[i]][4,19]
}


for(i in 1:length(list.out5)){
  tabel1Zbias1[i+(length(list.out5)),5] <- list.out40[[i]][1,89]
  tabel1Zbias2[i+(length(list.out5)),5] <- list.out40[[i]][2,89]
  tabel1Zbias3[i+(length(list.out5)),5] <- list.out40[[i]][3,89]
  tabel1Zbias4[i+(length(list.out5)),5] <- list.out40[[i]][4,89]
}

tabel1Zbias <- rbind(tabel1Zbias1,tabel1Zbias2,tabel1Zbias3,tabel1Zbias4)
tabel1Zbias <- cbind(tabel1Zbias, definebias, definenboot)

tabel1Zbias$imp <- factor(tabel1Zbias$imp, labels = c("P(Z=2)=0.10"))


tabel1Zbias$definenboot <- factor(tabel1Zbias$definenboot, labels = c("5","40"))

tabel1Zbias$definebias <- factor(tabel1Zbias$definebias, labels = c("WZ * Z1, m=40",
                                                      "W2 * Z1, m=40",
                                                      "W1 * Z2, m=40",
                                                      "W2 * Z2, m=40"))

onein <- subset(tabel1Zbias, definenboot=="5")
restr <- subset(tabel1Zbias, definenboot=="40")

onein$definebias <- factor(onein$definebias, labels = c("Y1 1 * Z1, m=5",
                                              "Y1 2 * Z1, m=5",
                                              "Y1 1 * Z2, m=5",
                                              "Y1 2 * Z2, m=5"))
tabel1Zbias <- rbind(onein, restr)



p <- ggplot(tabel1Zbias, aes(factor(c.prob), bias))
p0 <- p + theme_bw() + 
  geom_point(aes(shape=factor(definebias)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,18,0,1,2,5)) +
  guides(shape = guide_legend("cells")) +
  geom_hline(yintercept = 0) +
  xlab("classification probabilities") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))


# BIAS zonder legend 

p1 <- ggplot(tabel1Zbias, aes(factor(c.prob), bias))
p1 <- p1 + theme_bw() + 
  geom_point(aes(shape=factor(definebias)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,18,0,1,2,5)) +
  guides(shape = guide_legend("cells")) +
  geom_hline(yintercept = 0) +
  xlab("classification probabilities") +
  ylab("") +
  ggtitle("Bias") +
  theme(legend.position="none")

# coverage
tabel1Zcov1 <- cbind(scen.mat,cov=rep(NA,(2*length(list.out5))))
tabel1Zcov2 <- cbind(scen.mat,cov=rep(NA,(2*length(list.out5))))
tabel1Zcov3 <- cbind(scen.mat,cov=rep(NA,(2*length(list.out5))))
tabel1Zcov4 <- cbind(scen.mat,cov=rep(NA,(2*length(list.out5))))
definecov <- rep(c("1","2","3","4"), each=(2*length(list.out5)))
definenboot <- rep(c(5,5,5,5,5,40,40,40,40,40),4)

for(i in 1:length(list.out5)){
  tabel1Zcov1[i,5] <- list.out5[[i]][1,20]
  tabel1Zcov2[i,5] <- list.out5[[i]][2,20]
  tabel1Zcov3[i,5] <- NA
  tabel1Zcov4[i,5] <- list.out5[[i]][4,20]
}

for(i in 1:length(list.out5)){
  tabel1Zcov1[i+(length(list.out5)),5] <- list.out40[[i]][1,90]
  tabel1Zcov2[i+(length(list.out5)),5] <- list.out40[[i]][2,90]
  tabel1Zcov3[i+(length(list.out5)),5] <- NA
  tabel1Zcov4[i+(length(list.out5)),5] <- list.out40[[i]][4,90]
}

tabel1Zcov <- rbind(tabel1Zcov1,tabel1Zcov2,tabel1Zcov3,tabel1Zcov4)
tabel1Zcov <- cbind(tabel1Zcov, definecov, definenboot)

tabel1Zcov$definecov <- factor(tabel1Zcov$definecov, labels = c("W1 * Q1",
                                                                "W2 * Q1",
                                                                "W1 * Q2 (imp. comb.)",
                                                                "W2 * Q2"))

tabel1Zcov$definenboot <- factor(tabel1Zcov$definenboot, labels = c("5","40"))

tabel1Zcov$definecov <- factor(tabel1Zcov$definecov, labels = c("WZ * Z1, m=40",
                                                                "W2 * Z1, m=40",
                                                                "W1 * Z2, m=40",
                                                                "W2 * Z2, m=40"))

onein <- subset(tabel1Zcov, definenboot=="5")
restr <- subset(tabel1Zcov, definenboot=="40")

onein$definecov <- factor(onein$definecov, labels = c("Y1 1 * Z1, m=5",
                                                      "Y1 2 * Z1, m=5",
                                                      "Y1 1 * Z2, m=5",
                                                      "Y1 2 * Z2, m=5"))
tabel1Zcov <- rbind(onein, restr)

p2 <- ggplot(tabel1Zcov, aes(factor(c.prob), cov))
p2 <- p2 + theme_bw() + 
  geom_point(aes(shape=factor(definecov)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,18,0,1,2,5)) +
  guides(shape = guide_legend("cells")) +
  geom_hline(yintercept = 0.95) +
  xlab("classification probabilities") +
  ylab("coverage of the 95 percent confidence interval")
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))

p3 <- ggplot(tabel1Zcov, aes(factor(c.prob), cov))
p3 <- p3 + theme_bw() + 
  geom_point(aes(shape=factor(definecov)), 
             position=position_jitter(width=0.6), size = 4) + 
  scale_shape_manual(values=c(15,16,17,18,0,1,2,5)) +
  guides(shape = guide_legend("cells")) +
  geom_hline(yintercept = 0.95) +
  xlab("classification probabilities") +
  ylab("") +
  ggtitle("Coverage of the 95 percent confidence interval") +
  theme(legend.position="none")

# YLAB lege aanhalingstekens maken

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg <- g_legend(p0)
leg + guides(fill = guide_legend(override.aes = list(shape = 21), 
                                  "number of bootstrap samples"))


grid <- grid.arrange(arrangeGrob(p1,p3,leg,ncol=3,widths=c(5/12,5/12,2/12)))

grid.arrange(arrangeGrob(p1,p2,ncol=2,widths=c(5/12,7/12)))


