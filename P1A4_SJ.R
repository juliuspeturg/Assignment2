# Assignment 4

# URL <- "http://datamarket.com/api/v1/list.csv"
#gamma <- read.csv(URL)

gamma <- read.csv("gamma-visitolur.csv", header=TRUE)
gamma$Date <- as.Date(gamma$Date, "%Y-%m-%d")
gamma <- na.omit(gamma)
names(gamma)[names(gamma)=="GAMMA.vísitölur"] <- "GAMMA Equity Index"


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

gamma_training <- head(gamma, n=max_row_training)
gamma_testing <- tail(gamma, n=(nrow(gamma)-max_row_training))


# A4-Q3: Choose a model and predict 20 steps ahead


