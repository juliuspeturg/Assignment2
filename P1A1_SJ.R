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



# A1-Q4: 
# ATH stl() function - þurfum að breyta fuel$fpi í time series með
# fuel.ts <- ts(fuel$fpi, frequency=12, start=c(1979,1))
# og keyra svo fuel.stl <- stl(fuel.ts, s.window="periodic")
# nota fuel2 = window(fuel.ts, start=c(2000,1), end=c(2004,12)) til að skipta upp í tímabil


# A1-Q5: Split into test and training
# > training_set <- window(fuel.ts, start=c(1979,1), end=c(2003,12))
# > test_set <- window(fuel.ts, start=c(2004,1), end=c(2004,12))

