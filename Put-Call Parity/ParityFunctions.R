# Daniel Carr
# 5/20/2025

# Put-Call Parity functions and implied lending rates

# Explanation of variables: ----

# s = stock price at time zero
# x = strike price at maturity
# r = risk-free rate in annual, continuous terms
# q = dividend yield in annual, continuous terms
# c = price of a European call option
# p = price of a European put option
# t = time to maturity (annual)

# Please note that this is for European options, not American.

# Broadly, put-call parity can be seen as such:
# c + PV(X) = p + S

# This is for European options. If there are dividends, then it can be phrased as so:
# c + PV(X) + PV(Q) = p + S

# Assuming continuous rates, we get:
# c + X*e^-r*t = p + S*e^-q*t


# This can also be reframed using forwards instead of the current stock price.
  # In that case, PV(F) replaces S if the stock (or index) cannot be bought directly, but there is a futures market for it.
  # Generally not terribly important.

# Important to note: prices can vary based on whether buying or selling. 
  # Do keep in mind whether the bid (for selling) or the ask (for buying) would be appropriate. 

parityCall <- function(s, x, p, r, q, t){
  # Essentially buying a put, buying a stock, and lending the strike
  
  c <- p + s*exp(-q*t) - x*exp(-r*t)
  return(c)
}

parityPut <- function(s, x, c, r, q, t){
  # Buy a call, borrow the strike (in PV terms), short the stock. 
  
  p <- c + x*exp(-r*t) - s*exp(-q*t)
  return(p)
}

parityStock <- function(x, c, p, r, q, t){
  # I don't know why you would use this, but maybe there is a use case.
  # buy a call, borrow the strike, short a put
    # Receive dividends (?)
  
  s <- exp(q*t)*(c + x - p)
  return(s)
}

# I really can't see any occasion where the strike price needs to be derived; skipping it.


parityRate <- function(s, x, c, p, q, t){
  # This one is a little strange
  # lending the strike at rate r:
    # buy a put, buy the stock, sell a call
  
  # To borrow at rate r, you would sell a put, short the stock, buy a call.
  r <- log((p + s*exp(-q*t) - c)/x)/(-t)
  return(r)
}

parityRateRoi <- function(s, x, c, p){
  # non-annualized rate, 0 dividends
  r <- log((p + s - c)/x)*-1
  return(r)
}

