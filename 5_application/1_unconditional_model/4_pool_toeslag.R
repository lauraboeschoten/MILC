setwd("D:\\Surfdrive\\Project 1\\toepassing\\model1")
data  <- read.table("imp.samples.txt", header=TRUE)

Q1hat1 <- table(data$i1, data$toeslag)[1,1]/sum(table(data$toeslag))
Q1hat2 <- table(data$i2, data$toeslag)[1,1]/sum(table(data$toeslag))
Q1hat3 <- table(data$i3, data$toeslag)[1,1]/sum(table(data$toeslag))
Q1hat4 <- table(data$i4, data$toeslag)[1,1]/sum(table(data$toeslag))
Q1hat5 <- table(data$i5, data$toeslag)[1,1]/sum(table(data$toeslag))

Q2hat1 <- table(data$i1, data$toeslag)[2,1]/sum(table(data$toeslag))
Q2hat2 <- table(data$i2, data$toeslag)[2,1]/sum(table(data$toeslag))
Q2hat3 <- table(data$i3, data$toeslag)[2,1]/sum(table(data$toeslag))
Q2hat4 <- table(data$i4, data$toeslag)[2,1]/sum(table(data$toeslag))
Q2hat5 <- table(data$i5, data$toeslag)[2,1]/sum(table(data$toeslag))

Q3hat1 <- table(data$i1, data$toeslag)[1,2]/sum(table(data$toeslag))
Q3hat2 <- table(data$i2, data$toeslag)[1,2]/sum(table(data$toeslag))
Q3hat3 <- table(data$i3, data$toeslag)[1,2]/sum(table(data$toeslag))
Q3hat4 <- table(data$i4, data$toeslag)[1,2]/sum(table(data$toeslag))
Q3hat5 <- table(data$i5, data$toeslag)[1,2]/sum(table(data$toeslag))

Q4hat1 <- table(data$i1, data$toeslag)[2,2]/sum(table(data$toeslag))
Q4hat2 <- table(data$i2, data$toeslag)[2,2]/sum(table(data$toeslag))
Q4hat3 <- table(data$i3, data$toeslag)[2,2]/sum(table(data$toeslag))
Q4hat4 <- table(data$i4, data$toeslag)[2,2]/sum(table(data$toeslag))
Q4hat5 <- table(data$i5, data$toeslag)[2,2]/sum(table(data$toeslag))

U1hat1 <- (Q1hat1*(1-Q1hat1))/sum(table(data$toeslag))
U1hat2 <- (Q1hat2*(1-Q1hat2))/sum(table(data$toeslag))
U1hat3 <- (Q1hat3*(1-Q1hat3))/sum(table(data$toeslag))
U1hat4 <- (Q1hat4*(1-Q1hat4))/sum(table(data$toeslag))
U1hat5 <- (Q1hat5*(1-Q1hat5))/sum(table(data$toeslag))

U2hat1 <- (Q2hat1*(1-Q2hat1))/sum(table(data$toeslag))
U2hat2 <- (Q2hat2*(1-Q2hat2))/sum(table(data$toeslag))
U2hat3 <- (Q2hat3*(1-Q2hat3))/sum(table(data$toeslag))
U2hat4 <- (Q2hat4*(1-Q2hat4))/sum(table(data$toeslag))
U2hat5 <- (Q2hat5*(1-Q2hat5))/sum(table(data$toeslag))

U3hat1 <- (Q2hat1*(1-Q3hat1))/sum(table(data$toeslag))
U3hat2 <- (Q2hat2*(1-Q3hat2))/sum(table(data$toeslag))
U3hat3 <- (Q2hat3*(1-Q3hat3))/sum(table(data$toeslag))
U3hat4 <- (Q2hat4*(1-Q3hat4))/sum(table(data$toeslag))
U3hat5 <- (Q2hat5*(1-Q3hat5))/sum(table(data$toeslag))

U4hat1 <- (Q2hat1*(1-Q4hat1))/sum(table(data$toeslag))
U4hat2 <- (Q2hat2*(1-Q4hat2))/sum(table(data$toeslag))
U4hat3 <- (Q2hat3*(1-Q4hat3))/sum(table(data$toeslag))
U4hat4 <- (Q2hat4*(1-Q4hat4))/sum(table(data$toeslag))
U4hat5 <- (Q2hat5*(1-Q4hat5))/sum(table(data$toeslag))

Qbar1 <- mean(c(Q1hat1, Q1hat2, Q1hat3, Q1hat4, Q1hat5))
Qbar2 <- mean(c(Q2hat1, Q2hat2, Q2hat3, Q2hat4, Q2hat5))
Qbar3 <- mean(c(Q3hat1, Q3hat2, Q3hat3, Q3hat4, Q3hat5))
Qbar4 <- mean(c(Q4hat1, Q4hat2, Q4hat3, Q4hat4, Q4hat5))

Ubar1 <- mean(c(U1hat1, U1hat2, U1hat3, U1hat4, U1hat5))
Ubar2 <- mean(c(U2hat1, U2hat2, U2hat3, U2hat4, U2hat5))
Ubar3 <- mean(c(U3hat1, U3hat2, U3hat3, U3hat4, U3hat5))
Ubar4 <- mean(c(U4hat1, U4hat2, U4hat3, U4hat4, U4hat5))

Uhat1 <- c(U1hat1, U1hat2, U1hat3, U1hat4, U1hat5)
Uhat2 <- c(U2hat1, U2hat2, U2hat3, U2hat4, U2hat5)
Uhat3 <- c(U3hat1, U3hat2, U3hat3, U3hat4, U3hat5)
Uhat4 <- c(U4hat1, U4hat2, U4hat3, U4hat4, U4hat5)

B1 <- var(Uhat1)
B2 <- var(Uhat2)
B3 <- var(Uhat3)
B4 <- var(Uhat4)

T1 <- Ubar1+(5+1)*(B1/5)
T2 <- Ubar2+(5+1)*(B2/5)
T3 <- Ubar3+(5+1)*(B3/5)
T4 <- Ubar4+(5+1)*(B4/5)

sqrtT1  <- sqrt(T1)   
sqrtT2  <- sqrt(T2) 
sqrtT3  <- sqrt(T3) 
sqrtT4  <- sqrt(T4) 

ll1 <- Qbar1 - qt(.975, sum(table(data$toeslag))-1)*T1
ll2 <- Qbar2 - qt(.975, sum(table(data$toeslag))-1)*T1
ll3 <- Qbar3 - qt(.975, sum(table(data$toeslag))-1)*T1
ll4 <- Qbar4 - qt(.975, sum(table(data$toeslag))-1)*T1

ul1 <- Qbar1 + qt(.975, sum(table(data$toeslag))-1)*T1
ul2 <- Qbar2 + qt(.975, sum(table(data$toeslag))-1)*T1
ul3 <- Qbar3 + qt(.975, sum(table(data$toeslag))-1)*T1
ul4 <- Qbar4 + qt(.975, sum(table(data$toeslag))-1)*T1

ci1 <- ul1 - ll1
ci2 <- ul2 - ll2
ci3 <- ul3 - ll3
ci4 <- ul4 - ll4

model3 <- matrix(NA, nrow=4, ncol=8)
colnames(model3) <- c("Qbar","Ubar","B","T","sqrtT","ll","ul","ci")
model3[,"Qbar"]  <- c(Qbar1, Qbar2, Qbar3, Qbar4)
model3[,"Ubar"]  <- c(Ubar1, Ubar2, Ubar3, Ubar4)
model3[,"B"]     <- c(B1, B2, B3, B4)
model3[,"T"]     <- c(T1, T2, T3, T4)
model3[,"sqrtT"] <- c(sqrtT1, sqrtT2, sqrtT3, sqrtT4)
model3[,"ll"]    <- c(ll1, ll2, ll3, ll4)
model3[,"ul"]    <- c(ul1, ul2, ul3, ul4)
model3[,"ci"]    <- c(ci1, ci2, ci3, ci4)

round(model3, 4)

