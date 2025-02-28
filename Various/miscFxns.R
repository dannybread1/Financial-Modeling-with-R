# Daniel Caarr
# 2/26/2025

# Miscellaneous functions with specific application

fxnRatePutCallParity <- function(s, x, p, c, q, dtm){
  # Function to calculate the continuously compounded rate
  # Uses put-call parity
  # c + PV(x) = p + s - PV(q)
  # c + x*exp(r*dtm/252) = p + s*exp(-q*dtm/252)
  
  # Assumes 365 trading days per year
  r = (365/dtm)*log((p + s*exp(-q*dtm/365) - c)/x)
  
  return(r)
}

