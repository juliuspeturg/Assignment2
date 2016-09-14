# Assignment 4

rm(list=ls())

# Importing data
require(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/gamma-visitolur.csv")
gamma <- read.csv(text=x, stringsAsFactors = FALSE, header=TRUE)

# Data preparation
gamma$Date <- as.Date(gamma$Date, "%Y-%m-%d")
gamma <- na.omit(gamma)
names(gamma)[names(gamma)=="GAMMA.v.ed.sit.f6.lur"] <- "GAMMA Equity Index"

# A4-Q1: Plot the time series

require(googleVis)
GAMMA_Equity_Index <-
  A <- gvisAnnotationChart(gamma, datevar = 'Date', numvar = 'GAMMA Equity Index', date.format = "YY-mm-dd")
A$html$footer <- NULL
A$html$jsFooter <- NULL
A$html$caption <- NULL
plot(A)


# A4-Q2: Split into training and test set

# Make the split at the end of 2014 (5 years for training, 2 years for testing)

max_row_training = max(which(gamma$Date=="2014-12-31"))

gamma_train <- head(gamma, n=max_row_training)
gamma_test <- tail(gamma, n=(nrow(gamma)-max_row_training))


plot(gamma, type="l", col="white", main="Split of test & training set")
lines(gamma_train, type="l", col=2)
lines(gamma_test, type="l", col=3)
legend('bottomright', legend=c("Training set", "Test set"),
       col=c(2,3), lty=1, bty='y', cex=.85)



# A4-Q3: Choose a model and predict 20 steps ahead

# Creating a time series object
gamma.ts <- ts(gamma$`GAMMA Equity Index`)
gamma_train.ts <- ts(gamma_train$`GAMMA Equity Index`)
gamma_test.ts <- ts(gamma_test$`GAMMA Equity Index`)

# Modeling the forecast
require(forecast)
fit_gamma <- HoltWinters(gamma_train.ts, beta = FALSE, gamma = FALSE)
f.fit_gamma <- forecast(fit_gamma, 20)
plot(f.fit_gamma, col=2)
plot.ts(gamma_test.ts, col=2)
lines(f.fit_gamma,col=3)
accuracy(f.fit_gamma)

