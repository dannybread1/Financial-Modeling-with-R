# Daniel Carr
# 2/28/2025

# A script to demonstrate the various Black-Scholes-Merton functions.

rm(list = ls())
source('generalUse.R')
source('./BSMOVM/BSMOVM functions.R')
source('./Various/MiscFxns.R')

library(quantmod)


SpxOptions <- getOptionChain(Symbols = '^SPX', format(Sys.Date(), '%Y'))

# Merging puts and calls with the chosen strike date.
  # The index here is manual; there should be a way to improve this.
  # For today, this is 3/31/2025
df <- merge(x = SpxOptions[[22]]$calls
            , y = SpxOptions[[22]]$puts
            , by = c('ConractSize', 'Currency', 'Expiration', 'Strike')
            , all = TRUE
            , suffixes = c('_call', '_put'))


# Re-format the last-trade date, get the max
df$LastTradeTime_call <- as.Date(df$LastTradeTime_call)
df$LastTradeTime_put <- as.Date(df$LastTradeTime_put)
lastTradeDate <- max(df$LastTradeTime_call, df$LastTradeTime_put, na.rm = TRUE)

# Restrict to those that have traded as of the most recent trading day
  # Have to remove the NA values
df <- df[df$LastTradeTime_call == lastTradeDate 
         & df$LastTradeTime_put == lastTradeDate 
         & !is.na(df$Strike) 
         & !is.na(df$LastTradeTime_call) 
         & !is.na(df$LastTradeTime_put),]


# Haven't found a way to pull the current active price from online - putting this in manually
price <- 5851.90

# Calculating the days to expiry
daysToExpiry <- as.numeric(as.Date(df$Expiration[1]) - Sys.Date())

# Settle as the midpoint of the bid and ask
df$settle_call <- (df$Bid_call + df$Ask_call)/2
df$settle_put <- (df$Bid_put + df$Ask_put)/2

# Calculating the rate based on put-call parity; we're going to assume a 0 value of dividend yield
df$Rate <- fxnRatePutCallParity(s = price, x = df$Strike, p = df$settle_put, c = df$settle_call, q = 0, dtm = daysToExpiry)

# Let's actually calculate the dividend yield
for (i in 1:nrow(df)) {
  df$divYield_call[i] <- fxnGoalSeek(args = c(price, df$Strike[i], df$IV_call[i], df$Rate[i], 0, daysToExpiry, 1)
                                , val = 5
                                , fxn = priceBsmovm
                                , goal = df$settle_call[i]
                                , precision = 0.0001)
  df$divYield_put[i] <- fxnGoalSeek(args = c(price, df$Strike[i], df$IV_put[i], df$Rate[i], 0, daysToExpiry, -1)
                                     , val = 5
                                     , fxn = priceBsmovm
                                     , goal = df$settle_put[i]
                                     , precision = 0.0001)
}

# Let's demonstrate the implied volatility
  # First for calls
for (i in 1:nrow(df)) {
  df$BSMOVM_iv_call[i] <- volImpliedBsmovm(s = price
                                           , x = df$Strike[i]
                                           , vol = 0.20
                                           , r = df$Rate[i]
                                           , q = df$divYield_call[i]
                                           , dtm = daysToExpiry
                                           , ind = 1
                                           , o = df$settle_call[i])
}

# And then for puts
for (i in 1:nrow(df)) {
  df$BSMOVM_iv_put[i] <- volImpliedBsmovm(s = price
                                           , x = df$Strike[i]
                                           , vol = 0.20
                                           , r = df$Rate[i]
                                           , q = df$divYield_put[i]
                                           , dtm = daysToExpiry
                                           , ind = -1
                                           , o = df$settle_put[i])
}

# we could use a static value of volatility, but let's use the one provided.
df$BSMOVM_call <- priceBsmovm(s = price
                              , x = df$Strike
                              , vol = df$BSMOVM_iv_call
                              , r = df$Rate
                              , q = df$divYield_call
                              , dtm = daysToExpiry
                              , ind = 1) # 1 for call, -1 for put

df$BSMOVM_put <- priceBsmovm(s = price
                              , x = df$Strike
                              , vol = df$BSMOVM_iv_put
                              , r = df$Rate
                              , q = df$divYield_put
                              , dtm = daysToExpiry
                              , ind = -1) # 1 for call, -1 for put

# Calculating deltas
df$delta_call <- deltaBsmovm(s = price
                             , x = df$Strike
                             , vol = df$BSMOVM_iv_call
                             , r = df$Rate
                             , q = df$divYield_call
                             , dtm = daysToExpiry
                             , ind = 1)

df$delta_put <- deltaBsmovm(s = price
                             , x = df$Strike
                             , vol = df$BSMOVM_iv_put
                             , r = df$Rate
                             , q = df$divYield_put
                             , dtm = daysToExpiry
                             , ind = -1)

# Although the formula is equal for puts and calls, the inputs are not.
df$gamma_call <- gammaBsmovm(s = price
                             , x = df$Strike
                             , vol = df$BSMOVM_iv_call
                             , r = df$Rate
                             , q = df$divYield_call
                             , dtm = daysToExpiry)

df$gamma_put <- gammaBsmovm(s = price
                            , x = df$Strike
                            , vol = df$BSMOVM_iv_put
                            , r = df$Rate
                            , q = df$divYield_put
                            , dtm = daysToExpiry)

# Similarly for vegas
df$vega_call <- vegaBsmovm(s = price
                           , x = df$Strike
                           , vol = df$BSMOVM_iv_call
                           , r = df$Rate
                           , q = df$divYield_call
                           , dtm = daysToExpiry)

df$vega_put <- vegaBsmovm(s = price
                          , x = df$Strike
                          , vol = df$BSMOVM_iv_put
                          , r = df$Rate
                          , q = df$divYield_put
                          , dtm = daysToExpiry)

# Then the thetas
df$theta_call <- thetaBsmovm(s = price
                             , x = df$Strike
                             , vol = df$BSMOVM_iv_call
                             , r = df$Rate
                             , q = df$divYield_call
                             , dtm = daysToExpiry
                             , ind = 1)

df$theta_put <- thetaBsmovm(s = price
                            , x = df$Strike
                            , vol = df$BSMOVM_iv_put
                            , r = df$Rate
                            , q = df$divYield_put
                            , dtm = daysToExpiry
                            , ind = -1)

df$rho_call <- rhoBsmovm(s = price
                         , x = df$Strike
                         , vol = df$BSMOVM_iv_call
                         , r = df$Rate
                         , q = df$divYield_call
                         , dtm = daysToExpiry
                         , ind = 1)

df$rho_put <- rhoBsmovm(s = price
                        , x = df$Strike
                        , vol = df$BSMOVM_iv_put
                        , r = df$Rate
                        , q = df$divYield_put
                        , dtm = daysToExpiry
                        , ind = -1)

# I need some way to set the overall color as one thing, which depends on a variable.
ggplot() + geom_line(data = df, aes(x = Strike, y = theta_put, color = ITM_put)) +
  geom_line(data = df, aes(x = Strike, y = theta_call, color = ITM_call))
