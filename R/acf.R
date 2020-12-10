#' Title
#'
#' @param initial_values named numeric
#' @param data data frame
#' @param ... additional arguments to be passed to `optim()`
#'
#' @return list
#' @export
#'
acf <- function(initial_values, data, type = c("cd", "tl"), ...) {
  if (type == "cd") {
    if (length(initial_values) != 3) stop("Estimating a Cobb-Douglas function requires initial values for b0, bl, and bk.")
    initial_values <- check_initial_values(x = initial_values, type = "cd")
  }
  result <- stats::optim(par = initial_values, fn = acf_gmm, data = data, ...)
  return(result)
}
