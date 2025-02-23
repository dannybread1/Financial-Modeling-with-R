# Daniel Carr
# 2/22/2025

# Black-Scholes-Merton Option Valuation Model functions

# These should be relatively easy to understand
# Also intended to be able to accept vectors.

# Explanation of variables: ----

# s = stock price at time zero
# x = strike price at maturity
# vol = volatility in annual, continuous terms
# r = risk-free rate in annual, continuous terms
# ttm = time to maturity (annual)
# ind = +/-1 depending on if it is a call(+1) or put(-1)
  # ind is short for indicator

# Functions: ----

priceBsmovm <- function(s, x, vol, r, q, ttm, ind){
  
  # Calculate d1 and d2
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  d2 <- d1-vol*sqrt(ttm)
  
  # Calculate option price
  o <- ind*s*exp(-q*ttm)*pnorm(ind*d1) - ind*exp(-r*ttm)*x*pnorm(ind*d2)
  
  # pnorm is the cumulative distribution function
  
  return(o)
}

deltaBsmovm <- function(s, x, vol, r, q, ttm, ind){
  
  # Calculating 1-st order sensitivity of option value to underlying price
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  
  delta <- ind*exp(-vol*ttm)*pnorm(ind*d1)
  
  return(delta)
}

gammaBsmovm <- function(s, x, vol, r, q, ttm){
  # Second order sensitivity of option value to underlying price
  
  # Funny enough, don't actually need to indicator value for gamma.
  d1 <- (log(s/x)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  
  gamma <- qnorm(d1)*(exp(-r*ttm))/(s*vol*sqrt(ttm))
  # qnorm is the probability density function
  
  return(gamma)
}

vegaBsmovm <- function(){
  # Sensitivity of option value to change in volatility of price
}

rhoBsmovm <- function(){
  # Sensitivity of option value to change in risk-free rate
  
}

thetaBsmovm <- function(){
  # Sensitivity of option value to progression in time
    # Because this is almost always negative, frequently called "theta decay"
}

volImpliedBsmovm <- function(){
  # Volatility implied by a given option price (other values also given)
  
}