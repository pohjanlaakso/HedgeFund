# Antti Ilmanen argumented on behalf of rebalancing portfolio weights
# every quarter instead of every day or every month. 
#
#

source('portfolio.R')

# fetch commodities data, rename columns, and plot
commodities <- getSymbols('GD=F', auto.assign = F)
colnames(commodities) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
plot(commodities$Close)

# Nasdaq crypto index
getSymbols('BTC-USD') # weight 73.2
getSymbols('ETH-USD') # w  16.5
getSymbols('SOL-USD') # w 5.3
getSymbols('XRP-USD') # w 2.2
getSymbols('ADA-USD') # w 1.1

# add REITS, Emerging markets, and EAFE

# simple returns
r_commodities <- commodities$Close / lag(commodities$Close, k=1)-1
r_commodities <- na.omit(r_commodities)
plot(r_commodities)


# log returns
rlog_commodities <- log(commodities$Close) - log(lag(commodities$Close, k=1))
rlog_commodities <- na.omit(rlog_commodities)
plot(rlog_commodities)

# geometric mean return
geomean_commodities <- exp(mean(log(1+r_commodities)))^252-1
gmean_test(r_commodities, rlog_commodities)

# data frame: simple returns
simple_return_portfolio <- cbind(r_equity, r_bond, r_commodities)
simple_return_portfolio <- na.omit(simple_return_portfolio)

# data frame: log returns
log_return_portfolio <- cbind(rlog_equity, rlog_bond, rlog_commodities)
log_return_portfolio <- na.omit(log_return_portfolio)

# test input is data frame or matrix
input_test <- function(df) {
  if(!is.data.frame(df) && !is.matrix(df)) {
    stop('Input must be a data frame or a matrix.')
  }
}

volatility_window <- function(df, period_length) {
  rollapply(df, width = period_length, FUN = sd, fill = NA, align = 'right')
}

# volatility and inverse volatility for simple returns
simple_return_volatility <- volatility_window(simple_return_portfolio, period_length = (36*(252/12)))
simple_return_volatility <- na.omit(simple_return_volatility)
simpleReturn_inverseVolatility <- 1/simple_return_volatility

# visual check A
plot(simple_return_volatility$QQQ.Close, ylim = c(0, 0.02), col = 'red', type = 'l')
lines(simple_return_volatility$VCLT.Close, col = 'green')
lines(simple_return_volatility$Close, col = 'blue')
#legend('topleft', legend = c('A', 'B', 'C'), col = c('red', 'green', 'blue'), lty = 1)

# visual check B
plot(simpleReturn_inverseVolatility$QQQ.Close, col = 'red', ylim = c(50, 230))
lines(simpleReturn_inverseVolatility$VCLT.Close, col = 'green')
lines(simpleReturn_inverseVolatility$Close, col = 'blue')

# volatility and inverse volatility for log returns.
log_return_volatility <- volatility_window(log_return_portfolio, period_length = (36*(252/12)))
log_return_volatility <- na.omit(log_return_volatility)
logReturn_inverseVolatility <- 1/log_return_volatility

# visual check A --> no change between simple and log returns.
plot(log_return_volatility$QQQ.Close, ylim = c(0, 0.02), col = 'red', type = 'l')
lines(log_return_volatility$VCLT.Close, col = 'green')
lines(log_return_volatility$Close, col = 'blue')
#legend('topleft', legend = c('A', 'B', 'C'), col = c('red', 'green', 'blue'), lty = 1)

# visual check B --> no change between simple and log returns.
plot(logReturn_inverseVolatility$QQQ.Close, col = 'red', ylim = c(50, 230))
lines(logReturn_inverseVolatility$VCLT.Close, col = 'green')
lines(logReturn_inverseVolatility$Close, col = 'blue')

# calculate weights
weights <- t(apply(simpleReturn_inverseVolatility, 1, function(row) row / sum(row, na.rm = TRUE)))
weights <- as.data.frame(weights)

# visual check
plot(weights$QQQ.Close, type = 'l', col = 'red', ylim = c(0.2, 0.6))
lines(weights$VCLT.Close, col = 'green')
lines(weights$Close, col = 'blue')
grid()

# portfolio variance
returns <- tail(simple_return_portfolio, -(36*(252/12))+1)

complex_portfolio_variance <- function(weights_df, returns_df) {
  pvar_df <- numeric(nrow(weights_df)) # initialize daily portfolio variance vector
  
  for(i in seq_len(nrow(weights_df))) {
    w <- as.numeric(weights_df[i, ])
    r <- returns_df
    cov_matrix <- cov(r)
    pvar_df[i] <- t(w) %*% cov_matrix %*% w
  }
  return (pvar_df)
}

# test if these things even work
test_abc <- complex_portfolio_variance(weights, returns)
plot(sqrt(test_abc) * sqrt(252), type = 'l', ylim = c(0.1, 0.12)) # variance to volatility and daily to annual

# calculate risk parity portfolio Sharpe
# calculate RP returns
# visual check for RP portfolio vs 60/40
