
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

gmean_test <- function(raw_return_vector, log_return_vector){
  a <- exp(mean(log(1+raw_return_vector)))
  b <- exp(mean(log_return_vector))
  return (a-b) # should always return zero
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

portfolio_sharpe <- function(rf_rate, portfolio_sd, portfolio_return) {
  return( (portfolio_return-rf_rate)/portfolio_sd )
}

