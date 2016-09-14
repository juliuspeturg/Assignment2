# ------------
# Assignment 1
# Project 3
# ------------

# ----------
# Question 1
# ----------

n <- 10
x = getURL("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/AirPassengers.csv")
flightdata = read.csv(text=x,stringsAsFactors = FALSE,header=TRUE)
flightdata$X <- NULL
y <- flightdata$AirPassengers

SimpleMovingAverage <- function(y,n){
  FC=matrix(nrow=(nrow(y)-(n-1)),ncol=2)
  Y = y[[2]]
  for(i in 1:(nrow(y)-(n-1))){
  FC[i,2] = sum(Y[1:n])/n
  FC[i,1] = y[(n+i),1]
  Y <- Y[-1] 
  }
  FC <- na.omit(FC)
  FC
}

A <- SimpleMovingAverage(flightdata,10)
B <- SimpleMovingAverage(flightdata,5)


plot(flightdata$time,flightdata$AirPassengers,type="l",col="blue")
lines(A[,1],A[,2],col="red")
lines(B[,1],B[,2],col="green")
grid()

# ----------
# Question 2
# ----------

ExponentialSmoothing <- function(y,alpha){
  actual <- y[[2]]
  forcast <- matrix(nrow=length(actual),ncol=2)
  forcast[1,2] <- actual[1]
  for(i in 2:length(actual)){
    forcast[i,2] <- alpha*actual[i-1] + (1-alpha)*forcast[i-1,2]
  }
  forcast[,1] <- y[[1]]
  forcast
}

C <- ExponentialSmoothing(flightdata,1)
D <- ExponentialSmoothing(flightdata,0.5)


plot(flightdata$time,flightdata$AirPassengers,type="l",col="blue")
lines(C[,1],C[,2],col="red")
lines(D[,1],D[,2],col="green")
grid()

# ----------
# Question 3
# ----------

DoubleExponentialSmoothing <- function(y,alpha,beta){
  actual <- y[[2]]
  trend <- vector(length=length(actual))
  forcast <- matrix(nrow=length(actual),ncol=2)
  forcast[1,2] <- actual[1]
  trend[1] <- actual[2] - actual[1]
  for(i in 2:length(actual)){
    forcast[i,2] <- alpha*actual[i-1] + (1-alpha)*(forcast[i-1,2]+trend[i-1])
    trend[i]     <- beta*(forcast[i,2]-forcast[i-1,2])+(1-beta)*trend[i-1]
  }
  forcast[,1] <- y[[1]]
  forcast
  return((list(forcast,trend[length(trend)])))
}

E <- DoubleExponentialSmoothing(flightdata,0.2,0.3)
G <- matrix(unlist(E[1]),ncol=2,byrow=FALSE)
trend <- as.numeric(E[2])
predicted <- matrix(nrow = 20,ncol = 2)
predicted[1,2] <- G[nrow(G),2]
difference <- G[nrow(G),1]-G[(nrow(G)-1),1]
predicted[1,1] <- G[nrow(G),1]+difference
for(i in 2:nrow(predicted)){
  predicted[i,2] <- predicted[1,2]+trend*(i-1)
  predicted[i,1] <- predicted[i-1,1]+difference
}

stora <- rbind(G,predicted)
plot(stora[,1],stora[,2],col="white",xlim=c(min(stora[,1]),max(stora[,1])),ylim=c(min(stora[,2]),(max(stora[,2])+50)))
lines(flightdata$time,flightdata$AirPassengers,type="l",col="blue")
lines(G[,1],G[,2],col="green")
lines(predicted[,1],predicted[,2],col="red")
grid()