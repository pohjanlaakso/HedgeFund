# --- Setup ---
options(scipen=999)
rm(list=ls())
library(tidyquant)
library(MSwM)
library(xts)
library(quantmod)

# Helper: geometric mean annualized
geomean_f <- function(returns) { exp(mean(log(1+returns)))^252-1 }

# Helper: Conditional Value-at-Risk (CVaR)
CVaR <- function(returns, alpha=0.95) {
  sorted_returns <- sort(returns)
  threshold <- floor(length(sorted_returns)*alpha)
  mean(sorted_returns[1:threshold])
}

# --- Macro and Valuation Inputs ---
macro_forecast <- list(
  inflation = 0.025,
  gdp_growth = 0.021,
  fed_rate = 0.042
)

# Advanced macro-economic forecast
getSymbols(c("CPIAUCSL", "GDPC1", "FEDFUNDS", "UNRATE", "T10Y2Y", "UMCSENT"), src="FRED")

# Calculate macro variables robustly
gdp_growth <- tail(na.omit(as.numeric(GDPC1/lag(GDPC1, 4) - 1)), 1)
inflation <- tail(na.omit(as.numeric(CPIAUCSL/lag(CPIAUCSL, 12) - 1)), 1)
fed_rate <- as.numeric(last(FEDFUNDS))/100
unemployment <- as.numeric(last(UNRATE))/100
yield_curve <- as.numeric(last(T10Y2Y))/100
sentiment <- as.numeric(last(UMCSENT))

# Macro regime logic (example)
#if (yield_curve < 0 & unemployment > 0.05) {
#  macro_regime <- "recession"
#} else if (inflation > 0.04) {
#  macro_regime <- "stagflation"
#} else if (gdp_growth > 0.025 & inflation < 0.03 & yield_curve > 0.5) {
#  macro_regime <- "boom"
#} else {
#  macro_regime <- "normal"
#}

# Calculate YoY changes (annualized)
cpi <- CPIAUCSL
gdp <- GDPC1

# Inflation: 12-month % change
inflation_yoy <- na.omit(100 * diff(log(cpi), 12))
# Growth: 4-quarter % change
gdp_yoy <- na.omit(100 * diff(log(gdp), 4))

# Use the most recent 3 values to estimate trend
infl_trend <- mean(tail(inflation_yoy, 3), na.rm=TRUE)
gdp_trend <- mean(tail(gdp_yoy, 3), na.rm=TRUE)

# Calculate change in inflation and growth (momentum) robustly
if (length(inflation_yoy) >= 4) {
  infl_recent <- tail(inflation_yoy, 4)
  infl_mom <- as.numeric(infl_recent[4]) - as.numeric(infl_recent[1])
} else {
  infl_mom <- NA
}

if (length(gdp_yoy) >= 4) {
  gdp_recent <- tail(gdp_yoy, 4)
  gdp_mom <- as.numeric(gdp_recent[4]) - as.numeric(gdp_recent[1])
} else {
  gdp_mom <- NA
}

# Defensive regime assignment
if (is.finite(gdp_mom) && is.finite(infl_mom)) {
  if (gdp_mom >= 0 & infl_mom >= 0) {
    macro_regime <- "Rising Growth & Rising Inflation (boom)"
  } else if (gdp_mom >= 0 & infl_mom < 0) {
    macro_regime <- "Rising Growth & Falling Inflation (normal)"
  } else if (gdp_mom < 0 & infl_mom >= 0) {
    macro_regime <- "Falling Growth & Rising Inflation (stagflation)"
  } else {
    macro_regime <- "Falling Growth & Falling Inflation (recession)"
  }
} else {
  macro_regime <- "Unknown (insufficient data)"
}

cat("Current macro regime:", macro_regime, "\n")

# Example: Adjust expected returns or risk premia by regime
regime_adjustments <- list(
  "Rising Growth & Rising Inflation" = c(stocks=0.01, bonds=-0.01, commodities=0.02, reits=0.01),
  "Rising Growth & Falling Inflation" = c(stocks=0.02, bonds=0.01, commodities=0, reits=0.01),
  "Falling Growth & Rising Inflation" = c(stocks=-0.02, bonds=-0.03, commodities=0.03, reits=-0.01),
  "Falling Growth & Falling Inflation" = c(stocks=-0.01, bonds=0.02, commodities=-0.01, reits=0)
)

# Example: Add to your expected return function
#regime_adj <- regime_adjustments[[macro_regime]]
# Then, for each asset class, add regime_adj[asset_class] to its expected return

macro_forecast <- list(
  inflation = inflation,
  gdp_growth = gdp_growth,
  fed_rate = fed_rate,
  unemployment = unemployment,
  yield_curve = yield_curve,
  sentiment = sentiment,
  macro_regime = macro_regime
)

# valuation metrics
valuation_metrics <- list(
  QQQ_CAPE = 32, QQQ_CAPE_avg = 20,
  EAFE_CAPE = 17, EAFE_CAPE_avg = 21,
  VEU_CAPE = 19, VEU_CAPE_avg = 21,
  IEV_CAPE = 16, IEV_CAPE_avg = 20,
  EEMS_CAPE = 14, EEMS_CAPE_avg = 16,
  IEAC_Yield = 0.038,
  USRT_DivYield = 0.042
)

# --- Download Data ---
symbols <- c('QQQ','IEAC.AS','GD=F','USRT','EFA','VEU','IEV','EEMS')
for(sym in symbols) getSymbols(sym, auto.assign=TRUE)

# --- Calculate Returns ---
get_r <- function(x) na.omit(Ad(x)/lag(Ad(x))-1)
get_rlog <- function(x) na.omit(diff(log(Ad(x))))
r_equity <- get_r(QQQ)
r_ebond2 <- get_r(IEAC.AS)
r_commodities <- get_r(`GD=F`)
r_reit <- get_r(USRT)
r_EAFE <- get_r(EFA)
r_VEU <- get_r(VEU)
r_IEV <- get_r(IEV)
r_EEMS <- get_r(EEMS)

rlog_equity <- get_rlog(QQQ)
rlog_ebond2 <- get_rlog(IEAC.AS)
rlog_commodities <- get_rlog(`GD=F`)
rlog_reit <- get_rlog(USRT)
rlog_EAFE <- get_rlog(EFA)
rlog_VEU <- get_rlog(VEU)
rlog_IEV <- get_rlog(IEV)
rlog_EEMS <- get_rlog(EEMS)

# --- Align all returns ---
returns_mat <- na.omit(merge(
  rlog_equity, rlog_ebond2, rlog_commodities, rlog_reit,
  rlog_EAFE, rlog_VEU, rlog_IEV, rlog_EEMS
))
colnames(returns_mat) <- c("QQQ","IEAC","Commodities","USRT","EAFE","VEU","IEV","EEMS")

# Use only those dates for QQQ log returns
rlog_equity_common <- returns_mat[, "QQQ"]

# --- Markov Regime-Switching Model on QQQ (on aligned dates only!) ---
mod_lm <- lm(as.numeric(rlog_equity_common) ~ 1)
mod_mswm <- msmFit(mod_lm, k = 2, sw = c(TRUE, TRUE))

regime_probs <- mod_mswm@Fit@smoProb
# Get the full index of rlog_equity_common
# Step 1: Extract index and filter out any problematic entries
n <- length(rlog_equity_common)
regime_probs_xts <- xts(regime_probs[seq_len(n), ], order.by = index(rlog_equity_common))

# Get regime state for each row
regime_state <- apply(regime_probs_xts, 1, which.max)
returns_regime1 <- returns_mat[regime_state == 1, ]
returns_regime2 <- returns_mat[regime_state == 2, ]

# Use last observation's probabilities as current regime estimate
p1 <- as.numeric(tail(regime_probs_xts[,1], 1))
p2 <- as.numeric(tail(regime_probs_xts[,2], 1))

# --- CAPE-Adjusted Forward-Looking Expected Return Function ---
calc_cape_adj_return <- function(
    hist_returns,
    current_cape,
    long_term_avg_cape,
    macro,
    asset_class,
    regime_adjustments
) {
  hist_return <- geomean_f(hist_returns)
  earnings_yield <- 1 / current_cape
  cape_adj_factor <- long_term_avg_cape / current_cape
  adj_earnings_yield <- earnings_yield * cape_adj_factor
  
  # Macro regime adjustment
  # Macro regime adjustment
  #macro_adj <- switch(macro$macro_regime,
  #                    "recession" = -0.03,
  #                    "stagflation" = -0.02,
  #                    "boom" = 0.02,
  #                    "normal" = 0
  #)
  
  exp_return <- 0.3 * hist_return +
    0.5 * (adj_earnings_yield + macro$gdp_growth - macro$inflation) +
    0.2 * macro$gdp_growth +
    regime_adj[asset_class]
  return(exp_return)
}

calc_adj_return <- function(
    hist_returns,
    asset_type = "bond",
    valuation_metric = NA,
    macro = macro_forecast,
    spot = NA,
    fut = NA
) {
  hist_return <- geomean_f(hist_returns)
  if (asset_type == "bond") {
    bond_yield <- if (!is.na(valuation_metric)) valuation_metric else 0.03
    exp_return <- 0.2 * hist_return +
      0.7 * (bond_yield - macro$inflation) +
      0.1 * macro$fed_rate
  } else if (asset_type == "reit") {
    reit_yield <- if (!is.na(valuation_metric)) valuation_metric else 0.04
    exp_return <- 0.3 * hist_return +
      0.5 * (reit_yield + macro$gdp_growth - macro$inflation) +
      0.2 * macro$gdp_growth
  } else if (asset_type == "commodity") {
    if (!is.na(spot) && !is.na(fut)) {
      roll_yield <- (fut - spot) / spot
    } else {
      roll_yield <- 0
    }
    exp_return <- 0.4 * hist_return +
      0.4 * macro$inflation +
      0.2 * roll_yield
  } else {
    exp_return <- hist_return
  }
  return(exp_return)
}

# --- Calculate Regime-Weighted Expected Returns (CAPE-adjusted) ---
exp_returns_regime1 <- c(
  calc_cape_adj_return(returns_regime1[,"QQQ"], valuation_metrics$QQQ_CAPE, valuation_metrics$QQQ_CAPE_avg, macro_forecast),
  calc_adj_return(returns_regime1[,"IEAC"], "bond", valuation_metrics$IEAC_Yield, macro_forecast),
  calc_adj_return(returns_regime1[,"Commodities"], "commodity", NA, macro_forecast),
  calc_adj_return(returns_regime1[,"USRT"], "reit", valuation_metrics$USRT_DivYield, macro_forecast),
  calc_cape_adj_return(returns_regime1[,"EAFE"], valuation_metrics$EAFE_CAPE, valuation_metrics$EAFE_CAPE_avg, macro_forecast),
  calc_cape_adj_return(returns_regime1[,"VEU"], valuation_metrics$VEU_CAPE, valuation_metrics$VEU_CAPE_avg, macro_forecast),
  calc_cape_adj_return(returns_regime1[,"IEV"], valuation_metrics$IEV_CAPE, valuation_metrics$IEV_CAPE_avg, macro_forecast),
  calc_cape_adj_return(returns_regime1[,"EEMS"], valuation_metrics$EEMS_CAPE, valuation_metrics$EEMS_CAPE_avg, macro_forecast)
)
exp_returns_regime2 <- c(
  calc_cape_adj_return(returns_regime2[,"QQQ"], valuation_metrics$QQQ_CAPE, valuation_metrics$QQQ_CAPE_avg, macro_forecast),
  calc_adj_return(returns_regime2[,"IEAC"], "bond", valuation_metrics$IEAC_Yield, macro_forecast),
  calc_adj_return(returns_regime2[,"Commodities"], "commodity", NA, macro_forecast),
  calc_adj_return(returns_regime2[,"USRT"], "reit", valuation_metrics$USRT_DivYield, macro_forecast),
  calc_cape_adj_return(returns_regime2[,"EAFE"], valuation_metrics$EAFE_CAPE, valuation_metrics$EAFE_CAPE_avg, macro_forecast),
  calc_cape_adj_return(returns_regime2[,"VEU"], valuation_metrics$VEU_CAPE, valuation_metrics$VEU_CAPE_avg, macro_forecast),
  calc_cape_adj_return(returns_regime2[,"IEV"], valuation_metrics$IEV_CAPE, valuation_metrics$IEV_CAPE_avg, macro_forecast),
  calc_cape_adj_return(returns_regime2[,"EEMS"], valuation_metrics$EEMS_CAPE, valuation_metrics$EEMS_CAPE_avg, macro_forecast)
)

# Regime-probability-weighted expected returns and covariance
exp_returns <- as.numeric(p1 * exp_returns_regime1 + p2 * exp_returns_regime2)
cov_regime1 <- cov(returns_regime1)
cov_regime2 <- cov(returns_regime2)
cov_matrix <- p1 * cov_regime1 + p2 * cov_regime2

# --- Robust Risk Metric (CVaR + Variance) ---
risk_metric <- function(weights, returns_mat, cov_matrix) {
  port_rets <- as.numeric(returns_mat %*% weights)
  0.7 * sqrt(as.numeric(t(weights) %*% cov_matrix %*% weights) * 252) + 0.3 * abs(CVaR(port_rets))
}

# --- Optimization Function with Constraints ---
rf_rate <- 0.025
transaction_cost <- 0.0025
tax_rate <- 0.23
max_weights <- rep(0.35, length(exp_returns))

neg_sharpe_penalty <- function(weights) {
  weights <- weights / sum(weights)
  penalty <- 1000 * abs(sum(weights) - 1)
  concentration_penalty <- sum(pmax(weights - max_weights, 0)) * 1000
  divers_penalty <- sum(pmax(0.05 - weights, 0)) * 1000
  gross_return <- sum(weights * exp_returns)
  net_return <- ifelse(gross_return > 0, gross_return * (1 - tax_rate), gross_return) - sum(abs(weights)) * transaction_cost
  risk <- risk_metric(weights, as.matrix(returns_mat), cov_matrix)
  sharpe <- (net_return - rf_rate) / risk
  return(-sharpe + penalty + concentration_penalty + divers_penalty)
}

# --- Run Optimization ---
n_assets <- length(exp_returns)
init_weights <- rep(1/n_assets, n_assets)
result <- optim(
  par = init_weights,
  fn = neg_sharpe_penalty,
  method = "L-BFGS-B",
  lower = rep(0, n_assets),
  upper = max_weights,
  control = list(maxit=1000)
)
opt_weights <- result$par / sum(result$par)

# --- Output Results ---
cat("Diversified Optimal Weights (Regime-Switching):\n")
print(data.frame(
  Asset = c("QQQ","IEAC","Commodities","USRT","EAFE","VEU","IEV","EEMS"),
  Weight = round(opt_weights, 4)
))
cat("Maximum Sharpe ratio (robust, regime-weighted):", round(-result$value, 4), "\n")
cat("Current regime probabilities: Regime 1 =", round(p1,3), ", Regime 2 =", round(p2,3), "\n")
