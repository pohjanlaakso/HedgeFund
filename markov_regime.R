# --- Setup ---
options(scipen=999)
rm(list=ls())
library(tidyquant)
library(MSwM)

# Helper functions (define if not in functions.R)
geomean_f <- function(returns) { exp(mean(log(1+returns)))^252-1 }
CVaR <- function(returns, alpha=0.95) {
  sorted_returns <- sort(returns)
  threshold <- floor(length(sorted_returns)*alpha)
  mean(sorted_returns[1:threshold])
}

# Download data (as in your script)
getSymbols('QQQ'); getSymbols('VCLT'); getSymbols('IEAC.AS')
getSymbols('GLD'); getSymbols('VEU'); getSymbols('USRT')
getSymbols('EFA'); getSymbols('GSG')
BTC <- getSymbols('BTC-USD', auto.assign = FALSE)
commodities <- getSymbols('GD=F', auto.assign = FALSE)

# Prepare log returns
assets <- list(
  QQQ = QQQ$QQQ.Close,
  VCLT = VCLT$VCLT.Close,
  IEAC = IEAC.AS$IEAC.AS.Close,
  GLD = GLD$GLD.Close,
  VEU = VEU$VEU.Close,
  USRT = USRT$USRT.Close,
  EFA = EFA$EFA.Close,
  GSG = GSG$GSG.Close
)
logrets <- lapply(assets, function(x) na.omit(diff(log(x))))
rets <- lapply(assets, function(x) na.omit(x / lag(x) - 1))

# Align returns
returns_aligned <- na.omit(do.call(merge, logrets))
colnames(returns_aligned) <- names(assets)

# --- Markov Regime Switching on QQQ ---
equity_vec <- as.numeric(returns_aligned$QQQ)
mod_lm <- lm(as.numeric(equity_vec) ~ 1)
mod_mswm <- msmFit(mod_lm, k = 2, sw = c(TRUE, TRUE))
regime_prob <- mod_mswm@Fit@smoProb[,1]
# Use regime 1 as "high volatility" regime
is_high_vol <- regime_prob > 0.5
plot.ts(regime_prob)
abline(h = 0.5, col =2)

# --- Expected Returns with Forward-Looking Adjustment ---
# Simple valuation adjustment: penalize if trailing PE is high (mocked here)
calc_adj_return <- function(ret, pe_ratio=25) {
  base <- geomean_f(ret)
  adj <- ifelse(pe_ratio > 25, base * 0.8, base)
  return(adj)
}
exp_returns <- c(
  calc_adj_return(rets$QQQ, pe_ratio=30),
  calc_adj_return(rets$VCLT, pe_ratio=18),
  calc_adj_return(rets$IEAC, pe_ratio=18),
  calc_adj_return(rets$GLD, pe_ratio=20),
  calc_adj_return(rets$VEU, pe_ratio=18),
  calc_adj_return(rets$USRT, pe_ratio=20),
  calc_adj_return(rets$EFA, pe_ratio=18),
  calc_adj_return(rets$GSG, pe_ratio=20)
)

# --- Covariance Matrix: Regime-Adjusted ---
cov_normal <- cov(returns_aligned)
cov_stress <- cov_normal * 1.5
cov_matrix <- if (mean(is_high_vol) > 0.5) cov_stress else cov_normal

# --- Robust Risk Measure (CVaR + Variance) ---
risk_metric <- function(weights, returns_mat) {
  port_rets <- as.numeric(returns_mat %*% weights)
  0.7 * sd(port_rets) * sqrt(252) + 0.3 * abs(CVaR(port_rets))
}

# --- Optimization Function with Constraints ---
rf_rate <- 0.02 # Example: 2% annual risk-free rate
transaction_cost <- 0.0025 # 25bps per rebalance
tax_rate <- 0.23

neg_sharpe_penalty <- function(weights) {
  weights <- weights / sum(weights)
  # Constraints
  concentration_penalty <- sum(pmax(weights - 0.35, 0)) * 1000
  divers_penalty <- sum(pmax(0.05 - weights[c(3,4,5,6,7,8)], 0)) * 1000 # min 5% for diversifiers
  penalty <- 1000 * abs(sum(weights) - 1)
  # Transaction cost and tax adjustment
  gross_return <- sum(weights * exp_returns)
  net_return <- ifelse(gross_return > 0, gross_return * (1 - tax_rate), gross_return) - sum(abs(weights)) * transaction_cost
  # Robust risk
  risk <- risk_metric(weights, as.matrix(returns_aligned))
  sharpe <- (net_return - rf_rate) / risk
  return(-sharpe + concentration_penalty + divers_penalty + penalty)
}

# --- Run Optimization ---
n_assets <- length(exp_returns)
init_weights <- rep(1/n_assets, n_assets)
result <- optim(
  par = init_weights,
  fn = neg_sharpe_penalty,
  method = "L-BFGS-B",
  lower = rep(0, n_assets),
  upper = rep(0.35, n_assets),
  control = list(maxit=1000)
)
opt_weights <- result$par / sum(result$par)

# --- Output Results ---
cat("Diversified Optimal Weights:\n")
print(data.frame(
  Asset = names(assets),
  Weight = round(opt_weights, 4)
))
cat("Maximum Sharpe ratio (robust):", round(-result$value, 4), "\n")
