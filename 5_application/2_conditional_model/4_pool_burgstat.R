setwd("D:\\Surfdrive\\Project 1\\datapackage\\5_application\\2_conditional_model")
data  <- read.table("imp.samples.txt", header=TRUE)

data$burgstat <- ifelse(data$burgstat==1,0,1)

gl1 <- glm(formula=i1~burgstat, family="binomial", data)
gl2 <- glm(formula=i2~burgstat, family="binomial", data)
gl3 <- glm(formula=i3~burgstat, family="binomial", data)
gl4 <- glm(formula=i4~burgstat, family="binomial", data)
gl5 <- glm(formula=i5~burgstat, family="binomial", data)

Q1hat1 <- summary(gl1)$coefficients[1,1]
Q1hat2 <- summary(gl2)$coefficients[1,1]
Q1hat3 <- summary(gl3)$coefficients[1,1]
Q1hat4 <- summary(gl4)$coefficients[1,1]
Q1hat5 <- summary(gl5)$coefficients[1,1]

Q2hat1 <- summary(gl1)$coefficients[2,1]
Q2hat2 <- summary(gl2)$coefficients[2,1]
Q2hat3 <- summary(gl3)$coefficients[2,1]
Q2hat4 <- summary(gl4)$coefficients[2,1]
Q2hat5 <- summary(gl5)$coefficients[2,1]


U1hat1 <- summary(gl1)$coefficients[1,2]
U1hat2 <- summary(gl2)$coefficients[1,2]
U1hat3 <- summary(gl3)$coefficients[1,2]
U1hat4 <- summary(gl4)$coefficients[1,2]
U1hat5 <- summary(gl5)$coefficients[1,2]

U2hat1 <- summary(gl1)$coefficients[2,2]
U2hat2 <- summary(gl2)$coefficients[2,2]
U2hat3 <- summary(gl3)$coefficients[2,2]
U2hat4 <- summary(gl4)$coefficients[2,2]
U2hat5 <- summary(gl5)$coefficients[2,2]

Qbar1 <- mean(Q1hat1, Q1hat2, Q1hat3, Q1hat4, Q1hat5)
Qbar2 <- mean(Q2hat1, Q2hat2, Q2hat3, Q2hat4, Q2hat5)

Ubar1 <- (mean(U1hat1, U1hat2, U1hat3, U1hat4, U1hat5))^2
Ubar2 <- (mean(U2hat1, U2hat2, U2hat3, U2hat4, U2hat5))^2

Uhat1 <- c(U1hat1, U1hat2, U1hat3, U1hat4, U1hat5)
Uhat2 <- c(U2hat1, U2hat2, U2hat3, U2hat4, U2hat5)

B1 <- var(Uhat1)
B2 <- var(Uhat2)

T1 <- Ubar1+(5+1)*(B1/5)
T2 <- Ubar2+(5+1)*(B2/5)

sqrtT1  <- sqrt(T1)   
sqrtT2  <- sqrt(T2) 

ll1 <- Qbar1 - qt(.975, sum(table(data$toeslag))-1)*sqrtT1
ll2 <- Qbar2 - qt(.975, sum(table(data$toeslag))-1)*sqrtT1

ul1 <- Qbar1 + qt(.975, sum(table(data$toeslag))-1)*sqrtT1
ul2 <- Qbar2 + qt(.975, sum(table(data$toeslag))-1)*sqrtT1

ci1 <- ul1 - ll1
ci2 <- ul2 - ll2

model3 <- matrix(NA, nrow=2, ncol=8)
colnames(model3) <- c("Qbar","Ubar","B","T","sqrtT","ll","ul","ci")
model3[,"Qbar"]  <- c(Qbar1, Qbar2)
model3[,"Ubar"]  <- c(Ubar1, Ubar2)
model3[,"B"]     <- c(B1, B2)
model3[,"T"]     <- c(T1, T2)
model3[,"sqrtT"] <- c(sqrtT1, sqrtT2)
model3[,"ll"]    <- c(ll1, ll2)
model3[,"ul"]    <- c(ul1, ul2)
model3[,"ci"]    <- c(ci1, ci2)

round(model3, 4)