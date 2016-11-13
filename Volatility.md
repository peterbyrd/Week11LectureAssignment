# Volatility
Peter Byrd  
November 13, 2016  

# Financial Data and Volatility


## Load data
First we must install packages and load the appropriate data from our data source.


```r
## Set the working directory and load packages
setwd("/Users/pbyrd/Git/Week11LectureAssignment")

## Install and load the following packages: 
# install.packages('tseries')
library(tseries)

## Read the file
SNPdata <- get.hist.quote('^gspc',quote="Close")
```

```
## time series ends   2016-11-11
```

```r
length(SNPdata)
```

```
## [1] 6519
```

## Create a new dataset of Returns and calculate Volatility


```r
## Calculate Returns and Volatility
SNPret  <- log(lag(SNPdata)) - log(SNPdata)
SNPvol  <- sd(SNPret)*sqrt(250)*100
length(SNPret)
```

```
## [1] 6518
```

```r
SNPvol
```

```
## [1] 17.91128
```

## Creat a volatility function with lookback window


```r
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
```

## Run three scenarios with d = 10, 30, and 100


```r
## Run three scenarios of d=10,30,100
volest <- Vol(10,SNPret)
volest2 <- Vol(30,SNPret)
volest3 <- Vol(100,SNPret)
```

## Plot the results


```r
## Plot the volatility results
plot(volest,type="l")
lines(volest2,type="l",col="red")
lines(volest3,type="l",col="blue")
```

![](Volatility_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Conclusion

We see the smoothest result with d=100.
