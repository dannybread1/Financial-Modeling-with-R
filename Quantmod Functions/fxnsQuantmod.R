# Daniel Carr
# 5/21/2025

# Various quantmod things; adding to this as necessary.

library(quantmod)

qmodPrice <- function(symbol){
  # Removes some of the specific data, but it's a little cleaner.
  return(getQuote(Symbols = symbol)$Last)
}

qmodOptionDataFrame <- function(symbol, expiry){
  # Expiry can be a date, a year, or multiple years in YYYY/YYYY format. Accepts strings.
  
  # Getting the option chains from Yahoo Finance.
  lOpt <- getOptionChain(Symbols = symbol, Exp = expiry)
  
  # A slightly faster version than a FOR loop; 
    # Merges the put and call data frames for a given date base on the 4 columns
    # binds that data frame to the empty/previously bound data frames.
  df <- do.call(rbind, lapply(lOpt, function(opt) {
    merge(x = opt$calls,
          y = opt$puts,
          by = c('ConractSize', 'Currency', 'Expiration', 'Strike'),
          all = TRUE,
          suffixes = c('_call', '_put'))
  }))
  
  return(df)
}
