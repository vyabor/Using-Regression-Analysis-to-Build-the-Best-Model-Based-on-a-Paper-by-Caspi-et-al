set.seed(123)
# Reading in the data
ide <- read.csv('C:/Users/vyabo/Desktop/AMS 578 Project/IDEgroup756962.csv',header=T)[,-1]
idg <- read.csv('C:/Users/vyabo/Desktop/AMS 578 Project/IDGgroup756962.csv',header=T)[,-1]
idy <- read.csv('C:/Users/vyabo/Desktop/AMS 578 Project/IDYgroup756962.csv',header=T)[,-1]

# Merging by ID
data1 <- merge(ide,idg,by='ID')
dat <- merge(data1,idy,by='ID')

# Summary stats and correlation matrix (excluding ID)
summary(dat)[,-1]
apply(dat,2,length)
apply(dat,2,sd,na.rm=T)[-1]

length(apply(dat,1,function(x) sum(is.na(x)))[apply(dat,1,function(x) sum(is.na(x)))>=1])

cor(na.omit(dat[,-1]))

# Dealing with missing data
library(Amelia)

a.out <- amelia(dat,noms=c('R1','R2','R3','R4','R5','R6','R7','R8','R9','R10','R11','R12','R13','R14','R15','R16','R17','R18','R19','R20','R21','R22','R23','R24','R25'),idvars = 'ID',m=1)
summary(a.out)
a.out$imputations$imp1
data <- a.out$imputations$imp1[,-1]

# summary stats and correlation matrix for the imputation model (excluding ID)
summary(a.out$imputations$imp1)[,-1]
cor(a.out$imputations$imp1)[-1,-1]
apply(a.out$imputations$imp1,2,sd)[-1]

corr <- round(cor(data$Y,data[,]),4)

ff1 <- Y~(R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11+R12+R13+R14+R15+R16+R17+R18+R19+R20+R21+R22+R23+R24+R25)+poly(E1,4)+poly(E2,4)+poly(E3,4)+poly(E4,4)+poly(E5,4)+poly(E6,4)
ff2 <- Y~(poly(E1,4)+poly(E2,4)+poly(E3,4)+poly(E4,4)+poly(E5,4)+poly(E6,4)+R1+R2+R3+R4+R5+R6+R7+R8+R9+R10+R11+R12+R13+R14+R15+R16+R17+R18+R19+R20+R21+R22+R23+R24+R25)^2

fit1 <- lm(ff1,data)
summary(fit1)$coefficients[,4][summary(fit1)$coefficients[,4]<0.001]
summary(fit1)
# E2+E5+E6

fit2 <- lm(ff2,data)
summary(fit2)$coefficients[,4][summary(fit2)$coefficients[,4]<0.001]
summary(fit2)
# E2 + E5 + E6 + E1:R20 E2:E5 + E2:E6

data.fit <- data.frame(R20=data$R20,E1=data$E1,E2=data$E2,E5=data$E5,E6=data$E6,Y=data$Y)
data.fit

fit3 <- lm(Y~(R20+E1+E2+E5+E6)^4,data=data.fit)
summary(fit3)

?step
stp <- step(fit3,data=data.fit,direction = 'both',trace=0)
stp
summary(stp)$coefficients[,4][summary(stp)$coefficients[,4]<0.001]
summary(stp)$adj.r.squared
stp$anova
# R20 + E1 + E2 + E5 + E6 + R20:E1 + R20:E2 
# + R20:E5 + E1:E2 + E2:E5 + E2:E6 + E5:E6 + R20:E1:E2 + R20:E2:E5 
# + E2:E5:E6

# E2:E5:E6 is significant
plot(data.fit$E2*data.fit$E5*data.fit$E6,data.fit$Y)
summary(fit4)
step(fit4)

fit4 <- lm(Y~E2:E5:E6,data=data.fit)
summary(fit4)
write.csv(coef(summary(fit4)),'C:/Users/vyabo/Desktop/coefs.csv')

a1 <- AIC(fit1)
b1 <- BIC(fit1)

a2 <- AIC(fit2)
b2 <- BIC(fit2)

a3 <- AIC(fit3)
b3 <- BIC(fit3)

a4 <- AIC(fit4)
b4 <- BIC(fit4)

a5 <- AIC(fit5)
b5 <- BIC(fit5)

data.frame(c(a1,b1),c(a2,b2),c(a3,b3),c(a4,b4),c(a5,b5))

plot(resid(fit1)~fitted(fit1))
plot(resid(fit2)~fitted(fit2))
plot(resid(fit3)~fitted(fit3))
plot(resid(fit4)~fitted(fit4))

par(mfrow=c(2,2))
plot(fit5)
par(mfrow=c(1,1))

#knitr::knit2pdf('C:/Users/vyabo/Desktop/AMS 578 Project/test.Rnw', bib_engine = "biber")