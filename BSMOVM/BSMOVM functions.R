# Daniel Carr
# 2/22/2025

# source('./BSMOVM/BSMOVM functions.R')

# Black-Scholes-Merton Option Valuation Model functions

# These should be relatively easy to understand
# Also intended to be able to accept vectors.

# Explanation of variables: ----

# s = stock price at time zero
# x = strike price at maturity
# vol = volatility in annual, continuous terms
# r = risk-free rate in annual, continuous terms
# ttm = time to maturity (days)
# ind = +/-1 depending on if it is a call(+1) or put(-1)
  # ind is short for indicator

# Functions: ----

priceBsmovm <- function(s, x, vol, r, q, dtm, ind){
  
  # Assuming 365 days per year, all days (even non-trading) count.
  ttm <- dtm/365
  
  # Calculate d1 and d2
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  d2 <- d1-vol*sqrt(ttm)
  
  # Calculate option price
  o <- ind*s*exp(-q*ttm)*pnorm(ind*d1) - ind*exp(-r*ttm)*x*pnorm(ind*d2)
  
  # pnorm is the cumulative distribution function
  
  return(o)
}

deltaBsmovm <- function(s, x, vol, r, q, dtm, ind){
  
  ttm <- dtm/365
  
  # Calculating 1-st order sensitivity of option value to underlying price
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  
  delta <- ind*exp(-q*ttm)*pnorm(ind*d1)
  
  return(delta)
}

gammaBsmovm <- function(s, x, vol, r, q, dtm){
  # Second order sensitivity of option value to underlying price
  
  ttm <- dtm/365
  # Funny enough, don't actually need to indicator value for gamma.
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  
  gamma <- dnorm(d1)*exp(-q*ttm)/(s*vol*sqrt(ttm))
  # dnorm is the probability density function
  
  return(gamma)
}

vegaBsmovm <- function(s, x, vol, r, q, dtm){
  # Sensitivity of option value to change in volatility of price
  # Also insensitive to whether it is a call or put
  
  ttm <- dtm/365
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  
  vega <- .01*s*dnorm(d1)*sqrt(ttm)
  
  return(vega)
}

rhoBsmovm <- function(s, x, vol, r, q, dtm, ind){
  # Sensitivity of option value to change in risk-free rate
  
  ttm <- dtm/365
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  d2 <- d1-vol*sqrt(ttm)
  
  rho <- .01*ind*x*ttm*exp(-r*ttm)*pnorm(ind*d2)
  
  return(rho)
}

thetaBsmovm <- function(s, x, vol, r, q, dtm, ind){
  # Sensitivity of option value to progression in time
    # Because this is almost always negative, frequently called "theta decay"
  
  ttm <- dtm/365
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  d2 <- d1-vol*sqrt(ttm)
  
  theta <- 1/365*(-(s*vol*exp(-q*ttm)*dnorm(d1)/(2*sqrt(ttm)))-r*x*exp(-r*ttm)*pnorm(ind*d2)+ind*q*s*exp(-q*ttm)*pnorm(ind*d1))
  
  return(theta)
}

volImpliedBsmovm <- function(s, x, vol, r, q, dtm, ind, o){
  # Volatility implied by a given option price (other values also given)
  # Requires initial guess for vol
  
  IV <- fxnGoalSeek(args = c(s, x, vol, r, q, dtm, ind), val = 3, fxn = priceBsmovm, goal = o, precision = 0.001)
  
  return(IV)
}
