acf <- function(initial_values, data, ...) {
  result <- stats::optim(par = initial_values, fn = acf_gmm, data = data, ...)
  return(result$par)
}
