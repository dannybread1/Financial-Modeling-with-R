# Daniel Carr
# 3/27/2025

# BSMOVM pricing demonstration with simulations

rm(list = ls())
source('generalUse.R')
source('./BSMOVM/BSMOVM functions.R')

fxnSimOptionBSMOVM <- function(s, x, vol, r, q, dtm, ind){
  # this should work as a combination of normally distributed returns
  
  # Time to maturity (annual)
  ttm <- dtm/365
  
  # cumulative return; volatility gets scaled from annual to daily with 1/sqrt(365)
  # continuous dividend yield factors in.
  # return <- sum(qnorm(p = runif(n = dtm), sd = vol/sqrt(365)))-q*ttm+r*ttm
  
  # Change in s over time t
  s_t <- s*exp((r-q-(vol^2)/2)*ttm+vol*qnorm(runif(n = 1), sd = sqrt(ttm)))
  
  
  # option values
  o <- c(max(0, x - s_t), max(0, s_t-x))[ind/2+1.5]*exp(-r*ttm)
  
  return(o)
}

fxnSampleOptionsBSMOVM <- function(s, x, vol, r, q, dtm, ind){
  
  # Let's run this a million times or so
  oAvg <- mean(replicate(n = 1000000, expr = fxnSimOptionBSMOVM(s = s, x = x, vol = vol, r = r, q = q, dtm = dtm, ind = ind)))
  
  return(oAvg)
}

fxnChartSimResults <- function(s, x, vol, r, q, dtm, ind){
  
  vResults <- replicate(n = 1000000, expr = fxnSimOptionBSMOVM(s = s, x = x, vol = vol, r = r, q = q, dtm = dtm, ind = ind))
  
  df <- data.frame(PresentValue = vResults)
  
  p <- ggplot(data = df, aes(x = PresentValue)) + geom_histogram()
  
  return(p)
}


t0 <- Sys.time()
fxnSampleOptionsBSMOVM(s = 110, x = 110, vol = 0.20, r = 0.05, q = 0.02, dtm = 20, ind = 1)
print(Sys.time() - t0)

t1 <- Sys.time()
priceBsmovm(s = 110, x = 110, vol = 0.20, r = 0.05, q = 0.02, dtm = 20, ind = 1)
print(Sys.time() - t1)

fxnChartSimResults(s = 110, x = 110, vol = 0.20, r = 0.05, q = 0.02, dtm = 20, ind = 1)
