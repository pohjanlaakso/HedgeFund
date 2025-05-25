
# https://stackoverflow.com/questions/8216470/how-to-assign-from-a-function-with-multiple-outputs
simple_return <- function(vector) {
  a <- vector/lag(vector, k=1) - 1
  b <- na.omit(a)
  plot(b)
  return (list(a=a,b=b))
}

log_return <- function(vector) {
  a <- log(vector) - log(lag(vector, k = 1))
  b <- na.omit(a)
  plot(b)
  return(list(a=a, b=b))
}

# tests (at some point put into a different 'test-file.R')
gmean_test <- function(raw_return_vector, log_return_vector){
  a <- exp(mean(log(1+raw_return_vector)))
  b <- exp(mean(log_return_vector))
  return (a-b) # should always return zero
}

# test input is data frame or matrix
input_test <- function(df) {
  if(!is.data.frame(df) && !is.matrix(df)) {
    stop('Input must be a data frame or a matrix.')
  }
}

# portfolio correlation
cor_difdim <- function(vector_a, vector_b) {
  df <- merge(vector_a, vector_b) # equalises the differing dimensions of both vectors
  df <- na.omit(df)
  return(cor(df)[2,1])
}

# portfolio standard deviation
portfolio_variance <- function(vector_a, vector_b, weight_a, weight_b) {
  pvar <- weight_a^2*sd(vector_a)^2 + weight_b^2*sd(vector_b)^2 + 2*weight_a*weight_b*sd(vector_a)*sd(vector_b)*cor_difdim(vector_a, vector_b)
  return(sqrt(pvar)*sqrt(252)) # annualisation
}

# multiasset portfolio variance
multiasset_portfolio_variance <- function(return_matrix, weight_vector) {
  return_matrix <- as.matrix(return_matrix)
  weight_vector <- as.matrix(weight_vector)
  
  if(ncol(return_matrix) != length(weight_vector)) {
    stop("Number of columns in return matrix must match length of weight vector.")
  }
  
  covariance_matrix <- cov(return_matrix)
  por_var <- t(weight_vector) %*% covariance_matrix %*% weight_vector
  por_var <- sqrt(por_var * 252)
  return(as.numeric(por_var))
}

portfolio_sharpe <- function(rf_rate, portfolio_sd, portfolio_return) {
  return((portfolio_return-rf_rate)/portfolio_sd)
}
