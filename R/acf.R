acf <- function(initial_values, ...) {
  result <- optim(par = initial_values, fn = acf_gmm, ...)
  return(result$par)
}
