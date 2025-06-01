# --- Setup ---
options(scipen=999)
rm(list=ls())
library(tidyquant)
library(MSwM)
library(xts)
library(quantmod)

# --- Helper Functions ---
geomean_f <- function(returns) { exp(mean(log(1+returns)))^252-1 }
CVaR <- function(returns, alpha=0.95) {
  sorted_returns <- sort(returns)
  threshold <- floor(length(sorted_returns)*alpha)
  mean(sorted_returns[1:threshold])
}

# --- Download Asset Data ---
symbols <- c('QQQ','IEAC.AS','GD=F','USRT','EFA','VEU','IEV','EEMS')
for(sym in symbols) getSymbols(sym, auto.assign=TRUE)

get_rlog <- function(x) na.omit(diff(log(Ad(x))))
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

# --- Bridgewater-Style Macro Regime Logic ---
getSymbols(c("CPIAUCSL", "GDPC1"), src="FRED", warnings=FALSE, quiet=TRUE)

# Inflation: 12-month % change
inflation_yoy <- na.omit(100 * diff(log(CPIAUCSL), 12))
# Growth: 4-quarter % change
gdp_yoy <- na.omit(100 * diff(log(GDPC1), 4))

# Robust momentum calculation
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

if (is.finite(gdp_mom) && is.finite(infl_mom)) {
  if (gdp_mom >= 0 & infl_mom >= 0) {
    macro_regime <- "Rising Growth & Rising Inflation"
  } else if (gdp_mom >= 0 & infl_mom < 0) {
    macro_regime <- "Rising Growth & Falling Inflation"
  } else if (gdp_mom < 0 & infl_mom >= 0) {
    macro_regime <- "Falling Growth & Rising Inflation"
  } else {
    macro_regime <- "Falling Growth & Falling Inflation"
  }
} else {
  macro_regime <- "Unknown (insufficient data)"
}
cat("Current macro regime:", macro_regime, "\n")

# --- Regime-specific expected return adjustments (All Weather style) ---
regime_adj <- list(
  "Rising Growth & Rising Inflation" = c(QQQ=0.01, IEAC=-0.01, Commodities=0.02, USRT=0.01, EAFE=0.01, VEU=0.01, IEV=0.01, EEMS=0.01),
  "Rising Growth & Falling Inflation" = c(QQQ=0.02, IEAC=0.01, Commodities=0.00, USRT=0.01, EAFE=0.02, VEU=0.02, IEV=0.02, EEMS=0.02),
  "Falling Growth & Rising Inflation" = c(QQQ=-0.02, IEAC=-0.03, Commodities=0.03, USRT=-0.01, EAFE=-0.02, VEU=-0.02, IEV=-0.02, EEMS=-0.02),
  "Falling Growth & Falling Inflation" = c(QQQ=-0.01, IEAC=0.02, Commodities=-0.01, USRT=0.00, EAFE=-0.01, VEU=-0.01, IEV=-0.01, EEMS=-0.01),
  "Unknown (insufficient data)" = rep(0, 8)
)
regime_adj_vec <- regime_adj[[macro_regime]]
names(regime_adj_vec) <- c("QQQ","IEAC","Commodities","USRT","EAFE","VEU","IEV","EEMS")

# --- Macro and Valuation Inputs (can be made dynamic) ---
valuation_metrics <- list(
  QQQ_CAPE = 32, QQQ_CAPE_avg = 20,
  EAFE_CAPE = 17, EAFE_CAPE_avg = 21,
  VEU_CAPE = 19, VEU_CAPE_avg = 21,
  IEV_CAPE = 16, IEV_CAPE_avg = 20,
  EEMS_CAPE = 14, EEMS_CAPE_avg = 16,
  IEAC_Yield = 0.038,
  USRT_DivYield = 0.042
)

# Use latest available macro data for inflation, GDP growth, etc.
macro_forecast <- list(
  inflation = as.numeric(tail(na.omit(inflation_yoy), 1))/100,
  gdp_growth = as.numeric(tail(na.omit(gdp_yoy), 1))/100,
  fed_rate = 0.042 # You can pull from FRED if desired
)

# --- Markov Regime-Switching Model on QQQ (on aligned dates only!) ---
rlog_equity_common <- returns_mat[, "QQQ"]
mod_lm <- lm(coredata(rlog_equity_common) ~ 1)
mod_mswm <- msmFit(mod_lm, k = 2, sw = c(TRUE, TRUE))
regime_probs <- mod_mswm@Fit@smoProb

n <- length(rlog_equity_common)
regime_probs_xts <- xts(regime_probs[seq_len(n), ], order.by = index(rlog_equity_common))

stopifnot(nrow(regime_probs_xts) == nrow(returns_mat))

regime_state <- apply(regime_probs_xts, 1, which.max)
returns_regime1 <- returns_mat[regime_state == 1, ]
returns_regime2 <- returns_mat[regime_state == 2, ]

p1 <- as.numeric(tail(regime_probs_xts[,1], 1))
p2 <- as.numeric(tail(regime_probs_xts[,2], 1))

# --- CAPE-Adjusted Forward-Looking Expected Return Function ---
calc_cape_adj_return <- function(
    hist_returns,
    current_cape,
    long_term_avg_cape,
    macro,
    asset_name,
    regime_adj_vec
) {
  hist_return <- geomean_f(hist_returns)
  earnings_yield <- 1 / current_cape
  cape_adj_factor <- long_term_avg_cape / current_cape
  adj_earnings_yield <- earnings_yield * cape_adj_factor
  exp_return <- 0.3 * hist_return +
    0.5 * (adj_earnings_yield + macro$gdp_growth - macro$inflation) +
    0.2 * macro$gdp_growth +
    regime_adj_vec[asset_name]
  return(exp_return)
}

calc_adj_return <- function(
    hist_returns,
    asset_type = "bond",
    valuation_metric = NA,
    macro = macro_forecast,
    asset_name = NULL,
    regime_adj_vec = NULL
) {
  hist_return <- geomean_f(hist_returns)
  regime_adj_val <- if (!is.null(regime_adj_vec) && !is.null(asset_name)) regime_adj_vec[asset_name] else 0
  if (asset_type == "bond") {
    bond_yield <- if (!is.na(valuation_metric)) valuation_metric else 0.03
    exp_return <- 0.2 * hist_return +
      0.7 * (bond_yield - macro$inflation) +
      0.1 * macro$fed_rate +
      regime_adj_val
  } else if (asset_type == "reit") {
    reit_yield <- if (!is.na(valuation_metric)) valuation_metric else 0.04
    exp_return <- 0.3 * hist_return +
      0.5 * (reit_yield + macro$gdp_growth - macro$inflation) +
      0.2 * macro$gdp_growth +
      regime_adj_val
  } else if (asset_type == "commodity") {
    exp_return <- 0.4 * hist_return +
      0.4 * macro$inflation +
      regime_adj_val
  } else {
    exp_return <- hist_return + regime_adj_val
  }
  return(exp_return)
}

# --- Calculate Regime-Weighted Expected Returns (CAPE-adjusted, macro regime aware) ---
exp_returns_regime1 <- c(
  calc_cape_adj_return(returns_regime1[,"QQQ"], valuation_metrics$QQQ_CAPE, valuation_metrics$QQQ_CAPE_avg, macro_forecast, "QQQ", regime_adj_vec),
  calc_adj_return(returns_regime1[,"IEAC"], "bond", valuation_metrics$IEAC_Yield, macro_forecast, "IEAC", regime_adj_vec),
  calc_adj_return(returns_regime1[,"Commodities"], "commodity", NA, macro_forecast, "Commodities", regime_adj_vec),
  calc_adj_return(returns_regime1[,"USRT"], "reit", valuation_metrics$USRT_DivYield, macro_forecast, "USRT", regime_adj_vec),
  calc_cape_adj_return(returns_regime1[,"EAFE"], valuation_metrics$EAFE_CAPE, valuation_metrics$EAFE_CAPE_avg, macro_forecast, "EAFE", regime_adj_vec),
  calc_cape_adj_return(returns_regime1[,"VEU"], valuation_metrics$VEU_CAPE, valuation_metrics$VEU_CAPE_avg, macro_forecast, "VEU", regime_adj_vec),
  calc_cape_adj_return(returns_regime1[,"IEV"], valuation_metrics$IEV_CAPE, valuation_metrics$IEV_CAPE_avg, macro_forecast, "IEV", regime_adj_vec),
  calc_cape_adj_return(returns_regime1[,"EEMS"], valuation_metrics$EEMS_CAPE, valuation_metrics$EEMS_CAPE_avg, macro_forecast, "EEMS", regime_adj_vec)
)
exp_returns_regime2 <- c(
  calc_cape_adj_return(returns_regime2[,"QQQ"], valuation_metrics$QQQ_CAPE, valuation_metrics$QQQ_CAPE_avg, macro_forecast, "QQQ", regime_adj_vec),
  calc_adj_return(returns_regime2[,"IEAC"], "bond", valuation_metrics$IEAC_Yield, macro_forecast, "IEAC", regime_adj_vec),
  calc_adj_return(returns_regime2[,"Commodities"], "commodity", NA, macro_forecast, "Commodities", regime_adj_vec),
  calc_adj_return(returns_regime2[,"USRT"], "reit", valuation_metrics$USRT_DivYield, macro_forecast, "USRT", regime_adj_vec),
  calc_cape_adj_return(returns_regime2[,"EAFE"], valuation_metrics$EAFE_CAPE, valuation_metrics$EAFE_CAPE_avg, macro_forecast, "EAFE", regime_adj_vec),
  calc_cape_adj_return(returns_regime2[,"VEU"], valuation_metrics$VEU_CAPE, valuation_metrics$VEU_CAPE_avg, macro_forecast, "VEU", regime_adj_vec),
  calc_cape_adj_return(returns_regime2[,"IEV"], valuation_metrics$IEV_CAPE, valuation_metrics$IEV_CAPE_avg, macro_forecast, "IEV", regime_adj_vec),
  calc_cape_adj_return(returns_regime2[,"EEMS"], valuation_metrics$EEMS_CAPE, valuation_metrics$EEMS_CAPE_avg, macro_forecast, "EEMS", regime_adj_vec)
)

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
cat("Diversified Optimal Weights (Regime-Switching, All-Weather Macro):\n")
print(data.frame(
  Asset = c("QQQ","IEAC","Commodities","USRT","EAFE","VEU","IEV","EEMS"),
  Weight = round(opt_weights, 4)
))
cat("Maximum Sharpe ratio (robust, regime-weighted):", round(-result$value, 4), "\n")
cat("Current macro regime:", macro_regime, "\n")
cat("Current regime probabilities: Regime 1 =", round(p1,3), ", Regime 2 =", round(p2,3), "\n")
