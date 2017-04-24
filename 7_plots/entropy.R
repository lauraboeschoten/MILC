library(ggplot2)
options(scipen=999)

model <- rep(c("unconditional","conditional"),each=20)
imp.c <- rep(c("P(Z=2)=0.01","P(Z=2)=0.05","P(Z=2)=0.10","P(Z=2)=0.20"),10)
con.p <- rep(rep(c("0.70","0.80","0.90","0.95","0.99"),each=4),2)
entropy <- c(0.3070,0.3056,0.3034,0.3070,
             0.5935,0.5938,0.5929,0.5930, 
             0.8623,0.8619,0.8617,0.8621,
             0.9575,0.9572,0.9576,0.9576,
             0.9976,0.9975,0.9976,0.9975,
             0.3135,0.3409,0.3782,0.4643,
             0.5975,0.6147,0.6358,0.6863,
             0.8638,0.8691,0.8764,0.8935,
             0.9579,0.9593,0.9621,0.9670,
             0.9976,0.9977,0.9978,0.9981)
data <- cbind(model,imp.c,con.p,entropy)
data[,"entropy"] <- as.numeric(data[,"entropy"])
data <- as.data.frame(data)
data$entropy <- as.numeric(paste(data$entropy))

data$models2 <- factor(data$model, 
                            as.character(data$model))

data2 <- data[21:40,]

# GREY
palette <- c("#252525", "#969696")
p <- ggplot(data, aes(con.p, entropy))
p + facet_wrap(~imp.c) +
  theme_bw() +
  geom_point(aes(shape=factor(models2)), position=position_jitter(width=0.4), size=6) +
  scale_shape_manual(values=c(16,1)) +
  guides(shape = guide_legend("models")) + 
  xlab("classification probability") + 
  ylab(expression(paste("entropy ", R^2, sep=""))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        legend.text=element_text(size=14),
        strip.text=element_text(size=14),
        legend.title=element_text(size=18))

