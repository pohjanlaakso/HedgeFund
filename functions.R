
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