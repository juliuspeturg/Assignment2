# Importing data:

library(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/simdata.csv")
simdata = read.csv(text=x,stringsAsFactors = FALSE,header=TRUE, sep="")

# simdata <- read.csv("simdata.csv", header=TRUE, sep="") <<<< ATH breyta í URL import

# A2-Q1: Plot the data

# á sama grafi
plot(simdata$y1, type="l",col=2)
lines(simdata$y3,col=3)
lines(simdata$y2,col=4)

# # á þremur gröfum
# attach(simdata)
# par(mfrow=c(3,1)) 
# plot(simdata$y1, type="l",col=2)
# plot(simdata$y2, type="l",col=3)
# plot(simdata$y3, type="l",col=4)

# # A2-Q2: Plot acf and pacf
#
attach(simdata)
par(mfrow=c(3,2)) 
# par(mfrow=c(1,1)) 
acf(y1)
pacf(y1)
acf(y2)
pacf(y2)
acf(y3)
pacf(y3)

arima(y1)

# A2-Q3: Identify the models using arima

fit1.arima <- arima(y1, order=c(0,1,0))
summary(fit1.arima)
plot(forecast(fit1.arima,12))

# A2-Q4: Simulate a model

ts.sim <- arima.sim(list(order = c(1,0,0), ar = 0.9), n = 250) # líka hægt að nota aðferð í A2-Q7, skoða betur hvort er betra


# A2-Q5: Plot timeseries, ACF and PACF

par(mfrow=c(3,1)) 
plot(ts.sim)
acf(ts.sim)
pacf(ts.sim)

# A2-Q6: Create own plot function that plots the timeseries, ACF and PACF

acf.plot <- function(y){
  par(mfrow=c(3,1)) 
  plot(y)
  acf(y)
  pacf(y)
}

acf.plot(ts.sim)


# A2-Q7: Simulate the following series and plot with the plot function (n=250)

ts.sim1 <- arima.sim(n=250, list(ar=c(0.5)))          # ATH hvort þetta er rétt allt saman
ts.sim2 <- arima.sim(n=250, list(ar=c(-0.5)))
ts.sim3 <- arima.sim(n=250, list(ar=c(0.5,0.2)))
ts.sim4 <- arima.sim(n=250, list(ar=c(0.7)))
ts.sim5 <- arima.sim(n=250, list(ar=c(0.5,-0.2)))
ts.sim6 <- arima.sim(n=250, list(ma=c(0.5,0.5)))
ts.sim7 <- arima.sim(n=250, list(ma=c(-0.9)))
ts.sim8 <- arima.sim(n=250, list(ar=c(0.5), ma=c(-0.8)))


acf.plot(ts.sim1) # osfrv

# A2-Q8

ts.sim9 <- arima.sim(n=250, list(ar=c(0.7), ma=c(0.3))) # HVERNIG Á AÐ SETJA HELVÍTIS D FAKTORINN INN OG HVAÐ ER HANN???


ts.sim9_diff1 <- diff(ts.sim9, differences=1) #first difference
plot.ts(ts.sim9_diff1)
lines(ts.sim9)
