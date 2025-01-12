# Antti Ilmanen argumented on behalf of rebalancing portfolio weights
# every quarter instead of every day or every month. 
#
#

source('portfolio.R')

# fetch commodities data, rename columns, and plot
commodities <- getSymbols('GD=F', auto.assign = F)
colnames(commodities) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
plot(commodities$Close)

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

#### portfolio variance ####

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

inverse_volatility_weight <- function() {
  
}

####
