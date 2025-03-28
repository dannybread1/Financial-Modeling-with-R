# Daniel Carr
# 2/28/2025

# A script to demonstrate the various Black-Scholes-Merton functions.

rm(list = ls())
source('generalUse.R')
source('./BSMOVM/BSMOVM functions.R')

# Arbitrary figures:----
vVol <- c(0.01, 0.05, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00)
vStrike <- c(80, 90, 95, 98, 99, 100, 105, 110, 120)
vDtm <- c(seq(from = 1, to = 20, by = 1), seq(25, 95, 5), seq(100, 400, 10))

df <- as.data.frame(expand.grid(vVol, vStrike, vDtm))
colnames(df) <- c('vol', 'Strike', 'Dtm')


# Locking in these values for the sake of illustration
price <- 98.50
divYield <- 0.02
rate <-  0.05



# Calculation portion ----
df$BSMOVM_call <- priceBsmovm(s = price
                              , x = df$Strike
                              , vol = df$vol
                              , r = rate
                              , q = divYield
                              , dtm = df$Dtm
                              , ind = 1)

df$BSMOVM_put <- priceBsmovm(s = price
                              , x = df$Strike
                              , vol = df$vol
                              , r = rate
                              , q = divYield
                              , dtm = df$Dtm
                              , ind = -1)

df$delta_c <- deltaBsmovm(s = price
                          , x = df$Strike
                          , vol = df$vol
                          , r = rate
                          , q = divYield
                          , dtm = df$Dtm
                          , ind = 1)

df$delta_p <- deltaBsmovm(s = price
                          , x = df$Strike
                          , vol = df$vol
                          , r = rate
                          , q = divYield
                          , dtm = df$Dtm
                          , ind = -1)

df$gamma <- gammaBsmovm(s = price
                        , x = df$Strike
                        , vol = df$vol
                        , r = rate
                        , q = divYield
                        , dtm = df$Dtm)

df$vega <- vegaBsmovm(s = price
                      , x = df$Strike
                      , vol = df$vol
                      , r = rate
                      , q = divYield
                      , dtm = df$Dtm)

df$theta_c <- thetaBsmovm(s = price
                          , x = df$Strike
                          , vol = df$vol
                          , r = rate
                          , q = divYield
                          , dtm = df$Dtm
                          , ind = 1)

df$theta_p <- thetaBsmovm(s = price
                          , x = df$Strike
                          , vol = df$vol
                          , r = rate
                          , q = divYield
                          , dtm = df$Dtm
                          , ind = -1)

df$rho_c <- rhoBsmovm(s = price
                      , x = df$Strike
                      , vol = df$vol
                      , r = rate
                      , q = divYield
                      , dtm = df$Dtm
                      , ind = 1)

df$rho_p <- rhoBsmovm(s = price
                      , x = df$Strike
                      , vol = df$vol
                      , r = rate
                      , q = divYield
                      , dtm = df$Dtm
                      , ind = -1)

# Graphing portion ----
library(ggplot2)

# ggplot() + geom_point(data = df[df$vol == 0.20,], aes(x = Dtm, y = BSMOVM_call, color = Strike)) +
#   geom_point(data = df[df$vol == 0.20,], aes(x = Dtm, y = BSMOVM_put, color = Strike))
# 
# ggplot(data = df[df$vol == 0.20,], aes(x = Dtm, y = BSMOVM_call, color = Strike)) + geom_point()


# Option prices
# ~~~~~~~~~~~~~
ggplot(df[df$vol == 0.20, ], aes(x = Dtm, y = BSMOVM_call, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike Price") +
  labs(title = "Call value vs Days to Maturity, vol = 20%", y = "BSMOVM Call Price, USD", x = "Days to maturity")
ggplot(df[df$vol == 0.20, ], aes(x = Dtm, y = BSMOVM_put, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike Price") +
  labs(title = "Put value vs Days to Maturity, vol = 20%", y = "BSMOVM Put Price, USD", x = "Days to maturity")

ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = BSMOVM_put, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Put value vs Days to Maturity, Strike Price = $100", y = "BSMOVM Put Price, USD", x = "Days to Maturity")
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = BSMOVM_call, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Call value vs Days to Maturity, Strike Price = $100", y = "BSMOVM Put Price, USD", x = "Days to Maturity")
# ~~~~~~~~~~~~~



# Delta
# ~~~~~~~~~~~~~
ggplot(df[df$vol == 0.20, ], aes(x = Dtm, y = delta_c, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike") +
  labs(title = "Call delta vs Days to Maturity, Volatility = 20%", y = "BSMOVM Call Delta", x = "Days to Maturity")
ggplot(df[df$vol == 0.20, ], aes(x = Dtm, y = delta_p, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike") +
  labs(title = "Put delta vs Days to Maturity, Volatility = 20%", y = "BSMOVM Put Delta", x = "Days to Maturity")

ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = delta_c, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Call delta vs Days to Maturity, Strike = 100", y = "BSMOVM Call Delta", x = "Days to Maturity")
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = delta_p, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Put delta vs Days to Maturity, Strike = 100", y = "BSMOVM Put Delta", x = "Days to Maturity")
# ~~~~~~~~~~~~~




# Gamma
# ~~~~~~~~~~~~~
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = gamma, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Gamma vs Days to Maturity, Strike = 100", y = "BSMOVM Gamma", x = "Days to Maturity")
ggplot(df[df$vol == 0.30, ], aes(x = Dtm, y = gamma, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Gamma vs Days to Maturity, Volatility = 30%", y = "BSMOVM Gamma", x = "Days to Maturity")
# ~~~~~~~~~~~~~



# Vega
# ~~~~~~~~~~~~~
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = vega, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Vega vs Days to Maturity, Strike = 100", y = "BSMOVM Vega", x = "Days to Maturity")
ggplot(df[df$vol == 0.30, ], aes(x = Dtm, y = vega, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike") +
  labs(title = "Vega vs Days to Maturity, Volatility = 30%", y = "BSMOVM Vega", x = "Days to Maturity")
# ~~~~~~~~~~~~~



# Theta
# ~~~~~~~~~~~~~
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = theta_c, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Call Theta vs Days to Maturity, Strike = 100", y = "BSMOVM Call Theta", x = "Days to Maturity")
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = theta_p, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Put Theta vs Days to Maturity, Strike = 100", y = "BSMOVM Put Theta", x = "Days to Maturity")
ggplot(df[df$vol == 0.30, ], aes(x = Dtm, y = theta_c, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike") +
  labs(title = "Call Theta vs Days to Maturity, Volatility = 30%", y = "BSMOVM Call Theta", x = "Days to Maturity")
ggplot(df[df$vol == 0.30, ], aes(x = Dtm, y = theta_p, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike") +
  labs(title = "Put Theta vs Days to Maturity, Volatility = 30%", y = "BSMOVM Put Theta", x = "Days to Maturity")
# ~~~~~~~~~~~~~



# Rho
# ~~~~~~~~~~~~~
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = rho_c, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Call Rho vs Days to Maturity, Strike = 100", y = "BSMOVM Call Theta", x = "Days to Maturity")
ggplot(df[df$Strike == 100, ], aes(x = Dtm, y = rho_p, color = factor(vol))) +
  geom_line() +
  scale_color_discrete(name = "Volatility") +
  labs(title = "Put Rho vs Days to Maturity, Strike = 100", y = "BSMOVM Put Theta", x = "Days to Maturity")
ggplot(df[df$vol == 0.30, ], aes(x = Dtm, y = rho_c, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike") +
  labs(title = "Call Rho vs Days to Maturity, Volatility = 30%", y = "BSMOVM Call Theta", x = "Days to Maturity")
ggplot(df[df$vol == 0.30, ], aes(x = Dtm, y = rho_p, color = factor(Strike))) +
  geom_line() +
  scale_color_discrete(name = "Strike") +
  labs(title = "Put Rho vs Days to Maturity, Volatility = 30%", y = "BSMOVM Put Theta", x = "Days to Maturity")
# ~~~~~~~~~~~~~
