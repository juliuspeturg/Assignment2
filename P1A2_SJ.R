# Importing data:

library(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/simdata.csv")
simdata = read.csv(text=x,stringsAsFactors = FALSE,header=TRUE, sep="")

# A2-Q1: Plot the data

attach(simdata)

# á sama grafi
plot(y1, type="l",col='red', main="Simdata time series",
     xlab="Time", ylab="Data")
lines(y2,col='blue')
lines(y3,col='green')
legend('topright', legend=c("y1", "y2","y3"),
       col=c('red','blue','green'), lty=1, bty='y', cex=.85)


# á þremur gröfum

par(mfrow=c(3,1))
plot(y1, type="l",col=2, main="Time series y1", xlab="Time", ylab="Data")
plot(y2, type="l",col=3, main="Time series y2", xlab="Time", ylab="Data")
plot(y3, type="l",col=4, main="Time series y3", xlab="Time", ylab="Data")


# # A2-Q2: Plot acf and pacf

attach(simdata)
par(mfrow=c(3,2)) 
# par(mfrow=c(1,1)) 
acf(y1)
pacf(y1)
acf(y2)
pacf(y2)
acf(y3)
pacf(y3)

# A2-Q3: Identify the models using arima

arima(y1)
arima(y2)
arima(y3)

auto.arima(y1) # líka hægt að nota auto.arima

require(forecast)
fit1.arima <- arima(y1, order=c(0,1,0))
summary(fit1.arima)
plot(forecast(fit1.arima,12))

# A2-Q4: Simulate a model

ts.sim <- arima.sim(list(order = c(1,0,0), ar = 0.9), n = 250)


# A2-Q5: Plot timeseries, ACF and PACF

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))  
plot(ts.sim, main="Simulated time series ts.sim")
acf(ts.sim, main="ACF for ts.sim")
pacf(ts.sim, main="PACF for ts.sim")

# A2-Q6: Create own plot function that plots the timeseries, ACF and PACF

acf.plot <- function(y){
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) 
  plot(y, main=c("Simulated time series",deparse(substitute(y))))
  acf(y, main=c("ACF for",deparse(substitute(y))))
  pacf(y,main=c("PACF for",deparse(substitute(y))))
}

# Plotting the series, ACF and PACF with the new function
acf.plot(ts.sim)


# A2-Q7: Simulate the following series and plot with the plot function (n=250)

ts.sim1 <- arima.sim(n=250, list(ar=c(0.5)))        
ts.sim2 <- arima.sim(n=250, list(ar=c(-0.5)))
ts.sim3 <- arima.sim(n=250, list(ar=c(0.5,0.2)))
ts.sim4 <- arima.sim(n=250, list(ar=c(0.7)))
ts.sim5 <- arima.sim(n=250, list(ar=c(0.5,-0.2)))
ts.sim6 <- arima.sim(n=250, list(ma=c(0.5,0.5)))
ts.sim7 <- arima.sim(n=250, list(ma=c(-0.9)))
ts.sim8 <- arima.sim(n=250, list(ar=c(0.5), ma=c(-0.8)))

# Plotting the simulated series with the acf function
acf.plot(ts.sim1)
acf.plot(ts.sim2)
acf.plot(ts.sim3)
acf.plot(ts.sim4)
acf.plot(ts.sim5)
acf.plot(ts.sim6)
acf.plot(ts.sim7)
acf.plot(ts.sim8)


# A2-Q8

ts.sim9 <- arima.sim(list(order = c(1,1,1), ar = 0.7, ma=0.3), n = 250)


ts.sim9_diff1 <- diff(ts.sim9, differences=1) #first difference
plot.ts(ts.sim9_diff1, col=3)
lines(ts.sim9, col=2)

