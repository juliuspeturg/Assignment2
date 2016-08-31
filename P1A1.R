#fjarlægja breytur úr global umhverfi
rm(list=ls())

#import Data
library(RCurl)
x = getURL("https://raw.githubusercontent.com/juliuspeturg/Assignment1/master/fuel.csv")
fuel = read.csv(text=x,stringsAsFactors = FALSE)
fuel.ts = ts(data = fuel$fpi, start = c(1979,1),frequency = 12)
#finna hæsta gildi í fuel sem er með árið 2013, það er december 2013
max_row_training = max(which(fuel$year==2003))

#generate time sequence for google plot 
date_sequence = seq(as.Date("1979/1/1"), as.Date("2004/12/1"), "month")
fuel$DATE <- date_sequence

fuel_training = fuel[1:max_row_training,]
fuel_test = fuel[((max_row_training+1):nrow(fuel)),]

X <- matrix(c(rep(1,nrow(fuel_training)),fuel_training$rtime),nrow=nrow(fuel_training))
X2 <- matrix(c(rep(1,nrow(fuel)),fuel$rtime),nrow=nrow(fuel))

theta <- solve(t(X)%*%X)%*%t(X)%*%fuel_training$fpi

yhat <- X2%*%theta

fuel_googledata <- fuel

fuel_googledata[(nrow(fuel)+1):(2*nrow(fuel_googledata)),] <- NA
fuel_googledata$DATE[(which.min(!is.na(fuel_googledata$rtime)):nrow(fuel_googledata))] <- date_sequence
fuel_googledata$fpi[(which.min(!is.na(fuel_googledata$rtime)):nrow(fuel_googledata))] <- yhat

fuel_googledata$index <- NA
fuel_googledata$index[1:(which.min(!is.na(fuel_googledata$rtime))-1)] <- 'y'
fuel_googledata$index[(which.min(!is.na(fuel_googledata$rtime)):nrow(fuel_googledata))] <- 'yhat'

require(googleVis)
A <- gvisAnnotationChart(fuel_googledata,datevar = 'DATE',numvar = 'fpi',idvar = 'index',date.format = "YY-MM-dd")
A$html$footer <- NULL
A$html$jsFooter <- NULL
A$html$caption <- NULL
#plot(A)
B <- A$html$chart


# A1-Q6: Simple Exponential Smoothing

lossf <- function(alpha,y,k,d){
  # use 100 obs to initialize mu
  mu <- mean(y[1:d])
  
  yy <- y[-c(1:d)]
  eps <- yy[k] - mu
  for(tt in 1:(length(yy)-k)){
    mu <- alpha*yy[tt] + (1-alpha)*mu
    eps <- c(eps,yy[tt+k] - mu)
  }
  
  sum(eps^2)
}

alpha.init <- 0.1

# minimize
k=20
alpha <- optim(alpha.init,lossf,gr=NULL,y=fuel_training$fpi,k=20,d=50,method="L-BFGS-B", lower=0,upper=1)


# Prediction for 2004, búa til dataframe með emty DATE dálki

fuel_predict <- data.frame(DATE=as.Date(character()), stringsAsFactors = F)

date_sequence2 <- seq(fuel_training$DATE[(nrow(fuel_training)-k)],fuel_test$DATE[nrow(fuel_test)],"month")

fuel_predict[1:length(date_sequence2),] <- NA

fuel_predict$DATE[1:length(date_sequence2)] <- date_sequence2
# kalla þetta fpi eins og hitt sem má kannski kalla heimskt en eitthvað þarf þetta aðheita
fuel_predict$actual <- NA
fuel_predict$forecast<- NA
#setja actual gildi á k sæti aftur í tíma í actual
fuel_predict$actual[1:(length(date_sequence2))] <- fuel$fpi[fuel$DATE %in% as.Date(date_sequence2)]
# setja fyrsta gildi í forcast sem það sem var raungildi í dec 2013
fuel_predict$forecast[1] <- fuel_predict$actual[1]


# svo for lykkja til að nigga inn rest af gildum skv jöfnu
for(i in 2:nrow(fuel_predict)){
  fuel_predict$forecast[i] <- fuel_predict$forecast[(i-1)]+alpha$par*(fuel_predict$actual[(i-1)]-fuel_predict$forecast[(i-1)])  
}

fuel_googledata[((nrow(fuel_googledata)+1):(nrow(fuel_googledata)+length(date_sequence2))),] <- NA
#bæta við dagsetningum fyrir exponential smoothing
fuel_googledata$DATE[(nrow(fuel_googledata)-length(date_sequence2)+1):nrow(fuel_googledata)] <- date_sequence2

fuel_googledata$fpi[(nrow(fuel_googledata)-length(date_sequence2)+1):nrow(fuel_googledata)] <- fuel_predict$forecast

fuel_googledata$index[(nrow(fuel_googledata)-length(date_sequence2)+1):nrow(fuel_googledata)] <- 'y_SES'


# A1-Q8

lossf2 <- function(alpha2,y,k,h,d){
  mu <- mean(y[1:d])
  
  yy <- y[-c(1:d)]
  hh <- h[-c(1:d)]
  eps <- yy[k] - mu
  S <- rep(0,24)
  betatm1 <- beta <- 0
  for(tt in 2:(length(yy)-k)){
    mu <- c(mu,alpha2[1]*(yy[tt]-S[hh[tt]]) + (1-alpha2)*(mu[tt-1]+beta))
    beta <- alpha2[2]*(mu[tt] - mu[tt-1]) + (1-alpha2[2])*betatm1
    betatm1 <- beta
    S[hh[tt]] <- alpha2[3]*(yy[tt] - mu[tt]) + (1-alpha2[3])*S[hh[tt]]
    eps <- c(eps,yy[tt+k] - (mu[tt] + beta*k + S[hh[tt+k]]))
  }
  sum(na.omit(eps)^2)
}

alpha2.init <- rep(0.1,3)

alpha2 <- optim(alpha2.init,lossf2,gr=NULL,y=fuel_training$fpi,h=fuel_training$DATE,k=4,d=10,method="L-BFGS-B", lower=0,upper=1)


#A1-Q9-Random Walk
T <- nrow(fuel_test)
k <- 10
initial.value <- fuel_training$fpi[nrow(fuel_training)] 
GetRandomWalk <- function() {
  # Add a standard normal at each step
  initial.value + c(0, cumsum(rnorm(T)))
}
# Matrix of random walks
values <- replicate(k, GetRandomWalk())






suppressPackageStartupMessages(require(googleVis))
Z <- gvisAnnotationChart(fuel_googledata,datevar = 'DATE',numvar = 'fpi',idvar = 'index',date.format = "YY-MM-dd")
Z$html$footer <- NULL
Z$html$jsFooter <- NULL
Z$html$caption <- NULL
plot(Z)
#B <- A$html$chart
