
# Mise en place
options(scipen=999)
rm(list=ls()) # empty environment
library(tidyquant) # getSymbols()

# access other R-files
source('functions.R')
source('fama_french2.R')

if (!file.exists("functions.R")) stop("Missing functions.R")

# download data and visualize
getSymbols('QQQ'); plot(QQQ$QQQ.Close) # Nasdaq 100
getSymbols('VCLT'); plot(VCLT$VCLT.Close) # Long-term corporate bonds
getSymbols('FALN'); plot(FALN$FALN.Close) # Non-investment grade bonds
getSymbols('VECA.DE'); plot(VECA.DE$VECA.DE.Close) # Vanguard Eur corporate bonds
getSymbols('IEAC.AS'); plot(IEAC.AS$IEAC.AS.Close) # Blackrock Eur corporate bonds
getSymbols('CRPA.L'); plot(CRPA.L$CRPA.L.Close) # Global corporate bonds
getSymbols('ANGL'); plot(ANGL$ANGL.Close) # Non-investment grade bonds
getSymbols('CORP'); plot(CORP$CORP.Close) # Corporate bonds
BTC <- getSymbols('BTC-USD', auto.assign = F); plot(BTC$`BTC-USD.Close`) # Bitcoin 
# get commodities
# get gold
# get REITS
# get EME
# get World ex-USA

# basic return: https://www.r-bloggers.com/2020/05/basic-statistical-concepts-for-finance/#google_vignette
r_equity <- QQQ$QQQ.Close / lag(QQQ$QQQ.Close, k=1)-1; r_equity <- na.omit(r_equity); plot(r_equity)
r_bond <- VCLT$VCLT.Close / lag(VCLT$VCLT.Close, k=1)-1; r_bond <- na.omit(r_bond); plot(r_bond)
r_bond2 <- FALN$FALN.Close / lag(FALN$FALN.Close, k=1)-1; r_bond2 <- na.omit(r_bond2); plot(r_bond2)
r_ebond <- VECA.DE$VECA.DE.Close / lag(VECA.DE$VECA.DE.Close, k=1)-1; r_ebond <- na.omit(r_ebond); plot(r_ebond)
r_ebond2 <- IEAC.AS$IEAC.AS.Close / lag(IEAC.AS$IEAC.AS.Close, k=1)-1; r_ebond2 <- na.omit(r_ebond2); plot(r_ebond2)
r_gbond <- CRPA.L$CRPA.L.Close / lag(CRPA.L$CRPA.L.Close, k=1)-1; r_gbond <- na.omit(r_gbond); plot(r_gbond)
r_fallen <- ANGL$ANGL.Close / lag(ANGL$ANGL.Close, k = 1) - 1; r_fallen <- na.omit(r_fallen); plot(r_fallen)
r_corp <- CORP$CORP.Close / lag(CORP$CORP.Close, k = 1) - 1; r_corp <- na.omit(r_corp); plot(r_corp)
r_btc <- BTC$`BTC-USD.Close` / lag(BTC$`BTC-USD.Close`, k = 1) -1; r_btc <- na.omit(r_btc); plot(r_btc)

# logarithmic return
rlog_equity <- log(QQQ$QQQ.Close) - log(lag(QQQ$QQQ.Close, k=1)); rlog_equity <- na.omit(rlog_equity); plot(rlog_equity)
rlog_bond <- log(VCLT$VCLT.Close) - log(lag(VCLT$VCLT.Close, k=1)); rlog_bond <- na.omit(rlog_bond); plot(rlog_bond)
rlog_bond2 <- log(FALN$FALN.Close) - log(lag(FALN$FALN.Close, k=1)); rlog_bond2 <- na.omit(rlog_bond2); plot(rlog_bond2)
rlog_ebond <- log(VECA.DE$VECA.DE.Close) - log(lag(VECA.DE$VECA.DE.Close, k=1)); rlog_ebond <- na.omit(rlog_ebond); plot(rlog_ebond)
rlog_ebond2 <- log(IEAC.AS$IEAC.AS.Close) - log(lag(IEAC.AS$IEAC.AS.Close, k=1)); rlog_ebond2 <- na.omit(rlog_ebond2); plot(rlog_ebond2)
rlog_gbond <- log(CRPA.L$CRPA.L.Close) - log(lag(CRPA.L$CRPA.L.Close, k=1)); rlog_gbond <- na.omit(rlog_gbond); plot(rlog_gbond)
rlog_fallen <- log(ANGL$ANGL.Close) - log(lag(ANGL$ANGL.Close, k = 1)); rlog_fallen <- na.omit(rlog_fallen); plot(rlog_fallen)
rlog_corp <- log(CORP$CORP.Close) - log(lag(CORP$CORP.Close, k = 1)); rlog_corp <- na.omit(rlog_corp); plot(rlog_corp)
rlog_btc <- log(BTC$`BTC-USD.Close`) - log(lag(BTC$`BTC-USD.Close`, k = 1)); rlog_btc <- na.omit(rlog_btc); plot(rlog_btc)

# annualized geometric returns: https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
geomean_equity <- exp(mean(log(1+r_equity)))^252-1 # annualisation
geomean_bond <- exp(mean(log(1+r_bond)))^252-1
geomean_bond2 <- exp(mean(log(1+r_bond2)))^252-1
geomean_ebond <- exp(mean(log(1+r_ebond)))^252-1
geomean_ebond2 <- exp(mean(log(1+r_ebond2)))^252-1
geomean_gbond <- exp(mean(log(1+r_gbond)))^252-1
geomean_fallen <- exp(mean(log(1+r_fallen)))^252-1
geomean_corp <- exp(mean(log(1+r_corp)))^252 -1
geomean_btc <- exp(mean(log(1+r_btc)))^252-1

# testing 
gmean_test(r_equity, rlog_equity)

# portfolio variance
pvar <- portfolio_variance(rlog_equity, rlog_bond, 0.6, 0.4)
pvar2 <- portfolio_variance(rlog_equity, rlog_bond2, 0.6, 0.4)
pvar_ebond <- portfolio_variance(rlog_equity, rlog_ebond, 0.6, 0.4)
pvar_ebond2 <- portfolio_variance(rlog_equity, rlog_ebond2, 0.6, 0.4)
pvar_gbond <- portfolio_variance(rlog_equity, rlog_gbond, 0.6, 0.4)
pvar_fallen <- portfolio_variance(rlog_equity, rlog_fallen, 0.6, 0.4)
pvar_corp <- portfolio_variance(rlog_equity, rlog_corp,  0.6, 0.4)
pvar_btc <- portfolio_variance(rlog_equity, rlog_btc, 0.98, 0.02)

# expected return
expected_return <- 0.6 * geomean_equity + 0.4 * geomean_bond
expected_return2 <- 0.6 * geomean_equity + 0.4 * geomean_bond2
expected_return_ebond <- 0.6 * geomean_equity + 0.4 * geomean_ebond
expected_return_ebond2 <- 0.6 * geomean_equity + 0.4 * geomean_ebond2
expected_return_gbond <- 0.6 * geomean_equity + 0.4 * geomean_gbond
expected_return_fallen <- 0.6 * geomean_equity + 0.4 * geomean_fallen
expected_return_corp <- 0.6 * geomean_equity + 0.4 * geomean_corp
expected_return_btc <- 0.98 * geomean_equity + 0.02 * geomean_btc

# risk-free rate
rf_rate <- ( 1 + median(data$RF)/100 )^12-1 # calculate an annual long term average from monthly data
rf_rate <- (1 + median(RF/100))^12-1

# portfolio Sharpe ratio
psharpe <- round(portfolio_sharpe(rf_rate, pvar, expected_return), 2)
psharpe2 <- round(portfolio_sharpe(rf_rate, pvar2, expected_return2), 2)
psharpe_ebond <- round(portfolio_sharpe(rf_rate, pvar_ebond, expected_return_ebond), 2)
psharpe_ebond2 <- round(portfolio_sharpe(rf_rate, pvar_ebond, expected_return_ebond2), 2)
psharpe_gbond <- round(portfolio_sharpe(rf_rate, pvar_gbond, expected_return_gbond), 2)
psharpe_fallen <- round(portfolio_sharpe(rf_rate, pvar_fallen, expected_return_fallen), 2)
psharpe_corp <- round(portfolio_sharpe(rf_rate, pvar_corp, expected_return_corp), 2)
psharpe_btc <- round(portfolio_sharpe(rf_rate, pvar_btc, expected_return_btc), 2)

returns_aligned <- na.omit(merge(
  rlog_equity,
  rlog_bond,
  rlog_fallen,
  rlog_ebond2,
  rlog_corp,
  rlog_btc
))

multiasset_portfolio_variance(returns_aligned[, c(1, 5, 6)], c(0.58, 0.38, 0.02))
sd(returns_aligned$QQQ.Close) * sqrt(252)

portfolio_variance(returns_aligned$QQQ.Close, returns_aligned$VCLT.Close, 0.6, 0.4)
multiasset_portfolio_variance(returns_aligned[, c(1,2)], c(0.6, 0.4))
