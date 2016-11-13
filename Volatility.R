## Author: Peter Byrd
## Data: November 13, 2016
## Financial data and volitility

## Set the working directory and load packages
setwd("/Users/pbyrd/Git/Week11LectureAssignment")

## Install and load the following packages: 
install.packages('tseries')
library(tseries)

## Read the file
SNPdata <- get.hist.quote('^gspc',quote="Close")
length(SNPdata)

## Calculate Returns and Volatility
SNPret  <- log(lag(SNPdata)) - log(SNPdata)
SNPvol  <- sd(SNPret)*sqrt(250)*100
length(SNPret)
SNPvol

## Create volatility function
Vol <- function (d,logrets){
  var=0
  lam=0
  varlist <- c()
  for (r in logrets) {
    lam = lam*(1 - 1/d) + 1
    var = (1 - 1/lam)*var + (1/lam)*r^2
    varlist <- c(varlist,var)
  }
  sqrt(varlist)}

## Run three scenarios of d=10,30,100
volest <- Vol(10,SNPret)
volest2 <- Vol(30,SNPret)
volest3 <- Vol(100,SNPret)

## Plot the volatility results
plot(volest,type="l")
lines(volest2,type="l",col="red")
lines(volest3,type="l",col="blue")
