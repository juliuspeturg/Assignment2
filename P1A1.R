#fjarlægja breytur úr global umhverfi
rm(list=ls())
#Setja working directory
#setwd('C:/Users/Julius/Desktop/Skóli Haust 2016/tima/Rscripts/Assignment1')
#setwd('C:/Users/Julius/Desktop/Skóli Haust 2016/tima/Rscripts/Assignment1')
#loda library´s
require(ggplot2)
#require()

#import Data
library(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/fuel.csv")
fuel = read.csv(text=x,stringsAsFactors = FALSE)
fuel.ts = ts(data = fuel$fpi, start = c(1979,1),frequency = 12)
#finna hæsta gildi í fuel sem er með árið 2013, það er december 2013
max_row_training = max(which(fuel$year==2003))

fuel_training = fuel[1:max_row_training,]
fuel_test = fuel[(max_row_training+1):nrow(fuel),]

#generate time sequence for google plot 
seq(as.Date("1979/1/1"), as.Date("2004/12/1"), "month")

#Negrar




#tstplot
ggplot(data=fuel, aes(x=rtime, y=fpi, group=1)) + 
  geom_point() + xlab("Year") + ylab("Fuel Price Index") + 
  ggtitle("US Fuel Price Index: 1979-2004")





#siggatest

