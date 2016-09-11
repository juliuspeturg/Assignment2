# Assignment 4

rm(list=ls())

require(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/gamma-visitolur.csv")
gamma <- read.csv(text=x, stringsAsFactors = FALSE, header=TRUE)

gamma$Date <- as.Date(gamma$Date, "%Y-%m-%d")
gamma <- na.omit(gamma)
names(gamma)[names(gamma)=="GAMMA.v.ed.sit.f6.lur"] <- "GAMMA Equity Index"


# A4-Q1: Plot the time series

# require(ggplot2)
# ggplot(data=gamma, aes(x=gamma$Date, y=gamma$GAMMA.vísitölur, group=1)) + 
#   geom_line() + xlab("Date") + ylab("Gamma Equity Index") + 
#   ggtitle("Gamma Equity Index: 2009-2016")

require(googleVis)
GAMMA_Equity_Index <-
A <- gvisAnnotationChart(gamma, datevar = 'Date', numvar = 'GAMMA Equity Index', date.format = "YY-mm-dd")
A$html$footer <- NULL
A$html$jsFooter <- NULL
A$html$caption <- NULL
plot(A)


# A4-Q2: Split into training and test set

# Make the split at the end of 2013 (4 years for training, 3 years for testing)

max_row_training = max(which(gamma$Date=="2013-12-31"))

gamma_train <- head(gamma, n=max_row_training)
gamma_test <- tail(gamma, n=(nrow(gamma)-max_row_training))

gamma.ts <- ts(gamma$`GAMMA Equity Index`)
#View(gamma.ts)

#gamma_training = window(gamma.ts, start=c(2008,12,31), end=c(2013,12,31))


# A4-Q3: Choose a model and predict 20 steps ahead

# create a time series object
gamma.ts <- ts(gamma$`GAMMA Equity Index`)
gamma_train.ts <- ts(gamma_train$`GAMMA Equity Index`)
gamma_test.ts <- ts(gamma_test$`GAMMA Equity Index`) 

# fit_gamma <- ses(gamma_training$`GAMMA Equity Index`)
require(forecast)
fit_gamma <- HoltWinters(gamma_train.ts, beta = FALSE,gamma = FALSE)
f.fit_gamma <- forecast(fit_gamma, 20)
plot(f.fit_gamma)
accuracy(f.fit_gamma)
