
library(quadprog)
source('portfolio.R')

returns_aligned <- returns_aligned[, c(1,3, 5)]
mean_returns <- colMeans(returns_aligned)
cov_matrix <- cov(returns_aligned)

n_portfolios <- 1000
eff_frontier <- matrix(NA, nrow = n_portfolios, ncol = 2)  # columns: sd, expected return
weights_matrix <- matrix(NA, nrow = n_portfolios, ncol = ncol(returns_aligned))

# Generate efficient frontier
target_returns <- seq(min(mean_returns), max(mean_returns), length.out = n_portfolios)

for (i in 1:n_portfolios) {
  # Solve for weights using quadratic programming formulation
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, ncol(returns_aligned))
  
  Amat <- cbind(rep(1, ncol(returns_aligned)), mean_returns, diag(1, ncol(returns_aligned)))  # constraints: sum(w)=1, return=target, w >= 0
  bvec <- c(1, target_returns[i], rep(0, ncol(returns_aligned)))
  
  sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
  w <- sol$solution
  
  # Store results
  expected_return <- sum(w * mean_returns) * 252
  sd_portfolio <- sqrt(t(w) %*% cov_matrix %*% w) * sqrt(252)
  
  eff_frontier[i, ] <- c(sd_portfolio, expected_return)
  weights_matrix[i, ] <- w
}

# Plot Efficient Frontier
plot(eff_frontier[,1], eff_frontier[,2], type = "l", col = "blue", lwd = 2, #xlim=c(0, 0.25),
     xlab = "Portfolio Standard Deviation (Risk)",
     ylab = "Portfolio Expected Return",
     main = "Efficient Frontier (Base R)")
grid()


# Calculate Sharpe ratios
sharpe_ratios <- (eff_frontier[, 2] - rf_rate) / eff_frontier[, 1]

# Find index of max Sharpe ratio
max_sharpe_index <- which.max(sharpe_ratios)

# Extract weights
max_sharpe_weights <- weights_matrix[max_sharpe_index, ]

# Print results
colnames(weights_matrix) <- colnames(returns_aligned)
names(max_sharpe_weights) <- colnames(weights_matrix)
max_sharpe_weights

points(eff_frontier[max_sharpe_index, 1], eff_frontier[max_sharpe_index, 2],
       col = "red", pch = 19, cex = 1.5)


# Coordinates of max Sharpe point
x_sharpe <- eff_frontier[max_sharpe_index, 1]
y_sharpe <- eff_frontier[max_sharpe_index, 2]

text(x_sharpe, y_sharpe, labels = "Max Sharpe", pos = 4, col = "red")


# Add Capital Market Line (CML)
# Line from risk-free rate (0, rf_rate) to max Sharpe point
segments(0, rf_rate, x_sharpe, y_sharpe, col = "darkgreen", lwd = 2, lty = 2)
text(x_sharpe/2, rf_rate + (y_sharpe - rf_rate)/2, "CML", col = "darkgreen", pos = 3)
