library(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/fuel.csv")
fuel = read.csv(text=x,stringsAsFactors = FALSE)
summary(fuel)
mean(fuel$fpi)
attach(fuel)
mean(fpi)
plot(fpi~rtime)
# ---
# Linear Model
# ---
mod <- lm(fpi~rtime)
summary(mod)
# T stærra en 2
#viljum að breyturnar hafi 3X stjörnur, gefur til kynna að þetta sé statisticly segificant
#Það er að segja þær eru stabílar
par(mfrow=c(2,2))
plot(mod)
#viljum ekki trend í residualið
# trainingset
myts <- ts(fpi, start = c(1979,1), end =c(2003,12),frequency = 12)
#testset
myts2 <- ts(fpi, start = c(2004,1), end =c(2004,12),frequency = 12)

par(mfrow=c(1,1))
plot(myts)
fit1 <- stl(myts,s.window = "period")
#3 eða 4
plot(fit1)

par(mfrow=c(2,1))
require(forecast)
monthplot(myts)
seasonplot(myts)

fit2 <- ses(myts,h = 12, alpha = 0.1,exponential=TRUE)
fit3 <- HoltWinters(myts, beta = FALSE,gamma = FALSE)
fit4 <- ses(myts,h = 12, exponential=FALSE)
fit5 <- rwf(myts, h=12, drift=FALSE, fan=FALSE, lambda=NULL, biasadj=FALSE)
fit6 <- rwf(myts, h=12, drift=TRUE, fan=FALSE, lambda=NULL, biasadj=FALSE)
f.fit3 <- forecast(fit3,12)

plot(f.fit3)
lines(myts2,col=2)
lines(myts)

accuracy(f.fit3)
#arima excericise
acf(myts)
pacf(myts)

fit.arima <- arima(myts,order = c(1,1,1))
summary(fit.arima)
plot(forecast(fit.arima,12))

?arima.sim()

ts.sim <- arima.sim(myts,list(order = c(1,10),ar=c(0.5,0.2)),n=200)

acf.plot <- function(y){
  par(mfrow=c(3,1))
  plot(y)
  acf(y)
  pacf(y)
}

acf.plot(myts)


# aic google a***** information criteria
# coefficient should be two times larger than the standart error

#rbind(accuracy(fit3))



#detach(fuel)