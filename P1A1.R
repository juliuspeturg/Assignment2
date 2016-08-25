#fjarlægja breytur úr global umhverfi
rm(list=ls())

#import Data
library(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/fuel.csv")
fuel = read.csv(text=x,stringsAsFactors = FALSE)
fuel.ts = ts(data = fuel$fpi, start = c(1979,1),frequency = 12)
#finna hæsta gildi í fuel sem er með árið 2013, það er december 2013
max_row_training = max(which(fuel$year==2003))

fuel_training = fuel[1:max_row_training,]
fuel_test = fuel[((max_row_training+1):nrow(fuel)),]

X <- matrix(c(rep(1,nrow(fuel_training)),fuel_training$rtime),nrow=nrow(fuel_training))
X2 <- matrix(c(rep(1,nrow(fuel_test)),fuel_test$rtime),nrow=nrow(fuel_test))

theta <- solve(t(X)%*%X)%*%t(X)%*%fuel_training$fpi

yhat <- X2%*%theta
cat(yhat)

#generate time sequence for google plot 
date_sequence = seq(as.Date("1979/1/1"), as.Date("2004/12/1"), "month")
fuel$DATE <- date_sequence
fuel$yhat <- NA
fuel$index <- NA
fuel$index[(1:max_row_training)] <- 'y'
fuel$index[(max_row_training+1):nrow(fuel)] <- 'yhat'
fuel$yhat[(1:max_row_training)] <-fuel$fpi[(1:max_row_training)]
fuel$yhat[(max_row_training+1):nrow(fuel)] <- yhat


#tstplot
require(ggplot2)
p <- ggplot(data=fuel, aes(x=rtime, y=fpi, group=1)) + 
  geom_line() + xlab("Year") + ylab("Fuel Price Index") + 
  ggtitle("US Fuel Price Index: 1979-2004")
  
  
plot(p)
