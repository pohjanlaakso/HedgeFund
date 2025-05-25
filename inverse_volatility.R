
target_vol <- 0.15
rebalance_period <- 63 # aprox once per quarter 252 trading days

n_periods <- nrow(returns_aligned)
n_assets <- ncol(returns_aligned)
portfolio_returns <- numeric(n_periods)

print(dim(window_returns))
print(window_returns)

# Loop through time, rebalancing every quarter
for (i in seq(1, n_periods - rebalance_period, by = rebalance_period)) {
  
  # Get data window for volatility estimation (e.g., past 63 days)
  if (i < rebalance_period) {
    window_returns <- returns_aligned[1:i, ]
  } else {
    window_returns <- returns_aligned[(i - rebalance_period + 1):i, ]
  }
  
  # Compute annualized volatility (sqrt(252) scaling)
  vol <- apply(window_returns, 2, sd) * sqrt(252)
  
  # Inverse volatility weights
  inv_vol_weights <- 1 / vol
  weights <- inv_vol_weights / sum(inv_vol_weights)
  
  # Compute actual portfolio volatility
  cov_mat <- cov(window_returns) * 252
  port_vol <- sqrt(t(weights) %*% cov_mat %*% weights)
  
  # Rescale weights to match target volatility
  leverage <- target_vol / port_vol
  weights_scaled <- weights * leverage
  
  # Clip to full data range if last window
  window_end <- min(i + rebalance_period - 1, n_periods)
  
  # Apply weights to returns during this period
  for (j in i:window_end) {
    portfolio_returns[j] <- sum(returns[j, ] * weights_scaled)
  }
}