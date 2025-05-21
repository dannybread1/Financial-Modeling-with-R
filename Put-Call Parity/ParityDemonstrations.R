# Daniel Carr
# 5/21/2025

# Demonstrations of the various put-call parity functions

# Available indices:
# ^SPX
# ^NDX
# ^VIX

rm(list = ls())
source('./Put-Call Parity/ParityFunctions.R')
source('./Quantmod Functions/fxnsQuantmod.R')

fxnSmallOptionChain <- function(symbol){
  # Something to pull an option chain and shorten it to the most active contracts.
  
  dfO <- qmodOptionDataFrame(symbol = symbol, expiry = paste0(format(Sys.Date(), '%Y'), "/", as.numeric(format(Sys.Date(), '%Y')) + 1))
  
  dfO <- dfO[!is.na(dfO$ContractID_call) & !is.na(dfO$ContractID_put),]
  
  dfO$midpoint_call <- rowMeans(x = dfO[,colnames(dfO) %in% c('Bid_call', 'Ask_call')], na.rm = TRUE)
  dfO$midpoint_put <- rowMeans(x = dfO[,colnames(dfO) %in% c('Bid_put', 'Ask_put')], na.rm = TRUE)
  
  dfO <- dfO[as.Date(dfO$LastTradeTime_call) == Sys.Date() & as.Date(dfO$LastTradeTime_put) == Sys.Date(),]
  
  return(dfO)
}

xMark <- '^VIX'

dfS <- fxnSmallOptionChain(symbol = xMark)


dfS$rate <- parityRate(s = qmodPrice(symbol = xMark), x = dfS$Strike, c = dfS$midpoint_call, p = dfS$midpoint_put, q = 0
                       , t = (as.numeric(as.Date(dfS$Expiration) - Sys.Date()) + 1)/365)


dfS$rateRoi <- parityRateRoi(s = qmodPrice(symbol = xMark), x = dfS$Strike, c = dfS$midpoint_call, p = dfS$midpoint_put)
