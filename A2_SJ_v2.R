# ASSIGNMENT 2
# T-862-TIMA: Time Series Analysis

#fjarlægja breytur úr global umhverfi
rm(list=ls())

plot()

# Importing data

library(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment2/master/VEKS.csv")
veks = read.csv(text=x,stringsAsFactors = FALSE,header=TRUE)

attach(veks)

#######################################
############     Task 1      ##########
#######################################

# First we look at the plot of the HC.c vector (heat consumption with some NA values) and the Ta.c, the ambient air temperature.
par(mfrow=c(2,1))
plot(HC.c, type="l")
plot(Ta.c, type="l")

# We notice that there are fluctuations in the heat consumption data on a small scale (day by day and week by week) 
# and also a seasonal fluctuation, indicating more heat consumption during the winter and less during the summer when
# the ambient air temperature is higher. 


# Now we look at the autocovariance, autocorrelation and partial autocorrelation:

par(mfrow=c(3,1)) 
# par(mfrow=c(1,1)) 
acf(HC.c, type="correlation", na.action = na.pass) # Autocovariance
acf(HC.c, type="covariance", na.action = na.pass) # Autocorrelation
pacf(HC.c, na.action = na.pass) # Partial autocorrelation

# Make a new vector with the NA values omitted
HC.c2 <- na.omit(HC.c) 

##### Spectrum

# We start by taking the first difference of the series to make it stationary:

HC_diff1 <- diff(HC.c2, differences=1) #first difference

acf(HC_diff1, type="correlation", na.action = na.pass) # Autocovariance
acf(HC_diff1, type="covariance", na.action = na.pass) # Autocorrelation
pacf(HC_diff1, na.action = na.pass) # Partial autocorrelation

##### Spectrum

# Using spec.pgram
k = kernel("daniell", c(20,20)) # From testing different combinations of the span, (20,20) gave a good looking periodogram
specvalues = spec.pgram(HC_diff1, k, taper=0, log = "no")
abline(v=peak, lty="dotted")
peak

freq <- 1/peak
freq

# We see that there is a peak in the frequency at 0.085, equivalent to 11.75 hours. That indicates a peak in the usage of the 
# district heating system roughly every 12 hours.



#######################################
############     Task 2      ##########
#######################################

require(forecast)

# Intuitively we know that the data should have seasonality within every 24 hours, every week and also within every year.
# When predicting for such a short time period (1 hour and 6 hours), we conclude that the 24 hour cycle is the 
# most important seasonality cycle to look at, and therefore we change the data into a time series with frequency 24:

heat_con <- ts(HC.c2, frequency = 24)  

### FORECAST FOR 6 HOURS AHEAD ###

# Let us first try a linear model using tslm()

fit_lm <- tslm(heat_con ~ trend + season)
fcast_lm <- forecast(fit_lm, h=6)
plot(fcast_lm)
summary(fit5)

# Let's split into a test and training set

# split <- ceiling(0.7 * length(HC.c2)) # Make the split at 30/70
# heatC_Train <- ts(HC.c2[1:split], frequency = 24) # First 70% of the data is the training set
# heatC_Test <- ts(HC.c2[c((split+1):length(HC.c2))], frequency = 24) # Last 30% of the data is the test set
# 
# plot(heatC_Train)


# After taking the first difference of the heat consumption time series, we inspect the ACF and PACF plots
# and estimate that the heat consumption can be described with a ARIMA(3,1,1) process. 

# Using arima to predict with ARIMA(3,1,1):

fit1.arima <- arima(HC.c2, order=c(3,1,1))
summary(fit1.arima)
fcast1 <- forecast(fit1.arima,6)
plot(fcast1)


# Now we use the auto.arima() function from the forecast package in R. That gives a different model 
# than we estimated earlier, or ARIMA(4,1,4).

auto.arima(HC.c2)  # Gives ARIMA(4,1,4)

# Let's now use this input to predict:

fit2.arima <- arima(HC.c2, order=c(4,1,4))
summary(fit2.arima)
fcast2 <- forecast(fit2.arima,6)
plot(fcast2)

### FORECAST FOR 1 HOUR AHEAD ###

# Using a linear model with tslm()

fit6 <- tslm(heat_con ~ trend + season)
plot(forecast(fit6, h=1))
accuracy(fit6)

# Using arima to predict with ARIMA(3,1,1):

fit3.arima <- arima(HC.c2, order=c(3,1,1))
summary(fit3.arima)
fcast3 <- forecast(fit3.arima,1)
plot(fcast3)

# Using arima to predict with ARIMA(4,1,4):

fit4.arima <- arima(HC.c2, order=c(4,1,4))
summary(fit4.arima)
fcast4 <- forecast(fit4.arima,1)
plot(fcast4)


# Testing the models on one day and one week:

HC_week <- HC.c2[500:668] # Extracting a random week from the data (randomly chosen)
HC_day <- HC.c2[250:264] # Extracting a 24 hour interval from the data (randomly chosen)

HC_week.ts <- ts(HC_week, frequency = 168) # Creating a ts object
HC_day.ts <- ts(HC_day, frequency = 24) # Creating a ts object

plot(HC_week, type="l") # Plotting the week values
plot(HC_day, type="l") # Plotting the 24 hour values


# Arima fit for the week series, forecasting 6 hours ahead
fit10.arima <- arima(HC_week.ts, order=c(4,1,4)) 
summary(fit10.arima)
fcast10 <- forecast(fit10.arima,6)
plot(fcast10)


# Arima fit for the 24 hour series, forecasting 1 hour ahead
fit11.arima <- arima(HC_day.ts, order=c(4,1,4))
summary(fit11.arima)
fcast11 <- forecast(fit11.arima,1)
plot(fcast11)


### SUMMARY ###

# Accuracies of the models:
accuracy(fcast_lm) # Linear model
accuracy(fcast1) # ARIMA(3,1,1)
accuracy(fcast2) # ARIMA(4,1,4)
accuracy(fcast3) # ARIMA(3,1,1)
accuracy(fcast4) # ARIMA(4,1,4)
accuracy(fcast10) # ARIMA(4,1,4), one week
accuracy(fcast11) # ARIMA(4,1,4), one day


# CONCLUSION

# We see that, as expected, the linear model is performing worse than the ARIMA models (RMSE=537.2 vs. RMSE=43.8)
# The RMSE of ARIMA(4,1,4), the model order that auto.arima suggested, and ARIMA(3,1,1), the model
# order that we estimated was very similar (RMSE=42.9 for ARIMA(4,1,4) and RMSE=43.8 for ARIMA(3,1,1)).

# Therefore, an ARIMA model would be considered suitable if no other variables are taken into account, such as  
# outside temperature or solar radiation.



#######################################
############     Task 3      ##########
#######################################



#######################################
############     Task 4      ##########
#######################################


