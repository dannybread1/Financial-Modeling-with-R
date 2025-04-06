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
  
  # Change stock value at time T
  s_T <- s*exp((r-q-(vol^2)/2)*ttm+vol*qnorm(runif(n = 1), sd = sqrt(ttm)))
  
  
  # option values, call and put, chosen by the ind value (-1 for put, +1 for call)
  o <- c(max(0, x - s_T), max(0, s_T-x))[ind/2+1.5]*exp(-r*ttm)
  
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


# A second stab at making a simulated option price
fxnSampleOptionsBSMOVMv2 <- function(s, x, vol, r, q, dtm, ind){
  # Time to maturity (annual)
  ttm <- dtm/365
  
  vUnif <- (1:1000000-.5)/1000000
  
  s_T <- s*exp((r-q-(vol^2)/2)*ttm+vol*qnorm(vUnif, sd = sqrt(ttm)))
  
  if (ind == 1) {
    o <- mean(pmax(0, s_T - x))
  }
  if (ind == -1) {
    o <- mean(pmax(0, x - s_T))
  }
  
  return(o)
}

fxnAmericanBSMOVM <- function(s, x, vol, r, q, dtm, ind){
  # Valuing options that can be exercised at any time on or before maturity
  # Assuming prescience, because that's easier.
  
  # Days from initiation to maturity, annualized
  ttm <- (1:dtm)/365
  
  # Calculating the path the stock takes.
    # Using the cumulative sum of the random distribution. 
  s_t <- s*exp((r-q-(vol^2)/2)*ttm+vol*cumsum(qnorm(p = runif(n = dtm), sd = sqrt(1/365)))) #Holy parentheses, Batman.
  
  # put first, then call
  o <- c(max(pmax(0, x-s_t)*exp(-r*ttm)), max(pmax(0, s_t-x)*exp(-r*ttm)))[ind/2+1.5]
  
  return(o)
}


fxnSampleAmericanBSMOVM <- function(s, x, vol, r, q, dtm, ind){
  oAvg <- mean(replicate(n = 1000000, expr = fxnAmericanBSMOVM(s = s, x = x, vol = vol, r = r, q = q, dtm = dtm, ind = ind)))
  
  return(oAvg)
}



# There should be a significant (large?) price differential for American puts over European puts
  # Does this bear out in the market? If I buy underpriced American puts and short reasonably priced European puts, would that be profitable?
  # Are the American puts reasonably priced, or are they underpriced?




volImpliedBsmovm(s = 555.66, x = 550, vol = 0.2, r = 0.02, q = 0, dtm = 30, ind = -1, o = 9.90)

priceBsmovm(s = 556, x = 550, vol = 0.2045, r = 0.02, q = 0, dtm = 30, ind = 1)
fxnSampleOptionsBSMOVMv2(s = 556, x = 550, vol = 0.2045, r = 0.02, q = 0, dtm = 30, ind = 1)

deltaBsmovm(s = 556, x = 550, vol = 0.2045, r = 0.02, q = 0, dtm = 30, ind = 1)
calcSensitivity1(args = c(556, 550, 0.2045, 0.02, 0, 30, 1), val = 1, fxn = fxnSampleOptionsBSMOVMv2, inc = 1)


# fxnSampleOptionsBSMOVM(s = 110, x = 110, vol = 0.20, r = 0.05, q = 0.02, dtm = 40, ind = 1)

fxnSampleAmericanBSMOVM(s = 555.66, x = 550, vol = 0.2045, r = 0.02, q = 0, dtm = 30, ind = -1)



volImpliedBsmovm(s = 5580.94, x = 5525, vol = .09036, r = 0.02, q = 0, dtm = 30, ind = -1, o = 30.60)

priceBsmovm(s = 5580.94, x = 5525, vol = .09036, r = 0.02, q = 0, dtm = 30, ind = -1)

# fxnSampleOptionsBSMOVM(s = 110, x = 110, vol = 0.20, r = 0.05, q = 0.02, dtm = 40, ind = 1)

fxnSampleAmericanBSMOVM(s = 5580.94, x = 5525, vol = .09036, r = 0.02, q = 0, dtm = 30, ind = -1)

# fxnChartSimResults(s = 110, x = 110, vol = 0.20, r = 0.05, q = 0.02, dtm = 20, ind = 1)
