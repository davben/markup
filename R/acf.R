#' Title
#'
#' @param initial_values named numeric
#' @param data data frame
#' @param ... additional arguments to be passed to `optim()`
#'
#' @return list
#' @export
#'
acf <- function(initial_values, data, ...) {
  result <- stats::optim(par = initial_values, fn = acf_gmm, data = data, ...)
  return(result$par)
}
