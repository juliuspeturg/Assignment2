# Assignment 1 - Time Series Analysis

# Importing data
fuel <- read.csv(file.choose())
rm(list=ls())

# A1-Q1: Lineplot
require(ggplot2)
ggplot(data=fuel, aes(x=rtime, y=fpi, group=1)) + 
  geom_line() + xlab("Year") + ylab("Fuel Price Index") + 
  ggtitle("US Fuel Price Index: 1979-2004")

# A1-Q2: Is it reasonable to estimate the mean and standard deviation of the data?
# blalblablalblab


# A1-Q3: GLM Model 

y <- c(0.2,1.2,1.9,2.3,1.9)
x <- c(0.4,1.2,2.3,3.4,4.3)
X <- matrix(c(rep(1,5),x,x^2),nrow=5)
theta <- solve(t(X)%*%X)%*%t(X)%*%y
x2 <- c(1.4,2.2,3.3,3.7,5.3)
X2 <- matrix(c(rep(1,5),x2,x2^2),nrow=5)
yhat <- X2%*%theta
yhat


plot(yhat.ts)
par(new=T)
plot(fuel.ts)

plot(yhat.ts,type="l",col=2)
lines(fuel.ts,col=3)


# A1-Q4: 
# ATH stl() function - þurfum að breyta fuel$fpi í time series með
fuel.ts <- ts(fuel$fpi, frequency=12, start=c(1979,1))
# og keyra svo fuel.stl <- stl(fuel.ts, s.window="periodic")
# nota fuel2 = window(fuel.ts, start=c(2000,1), end=c(2004,12)) til að skipta upp í tímabil


# A1-Q5: Split into test and training
fuel_training <- window(fuel.ts, start=c(1979,1), end=c(2003,12))
fuel_test <- window(fuel.ts, start=c(2004,1), end=c(2004,12))





# A1-Q7: Simple Exponential Smoothing

lossf <- function(alpha,y,k,d){
  # use 100 obs to initialize mu
  mu <- mean(y[1:d])
  
  yy <- y[-c(1:d)]
  eps <- yy[k] - mu
  for(tt in 1:(length(yy)-k)){
    mu <- alpha*yy[tt] + (1-alpha)*mu
    eps <- c(eps,yy[tt+k] - mu)
  }
  
  sum(eps^2)
}


#Initialize alpha

alpha.init <- 0.1

# minimize
alpha <- optim(alpha.init,lossf,gr=NULL,y=fuel_test,k=20,d=50,method="L-BFGS-B", lower=0,upper=1)


# Prediction for 2004
fuel_predict$Date <- fuel_training$[nrow(fuel_training):fuel_test[nrow(fuel_test)]]
