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
acf(y1)
pacf(y1)
acf(y2)
pacf(y2)
acf(y3)
pacf(y3)

arima()

# # A2-Q3: Identify the models using arima
# arima(simdata$y1) osfrv
