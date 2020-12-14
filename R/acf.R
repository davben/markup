#' Title
#'
#' @param initial_values named numeric
#' @param data data frame
#' @param type string specifying the type of production function to be estimated. Currently, only Cobb-Douglas is supported.
#' @param ... additional arguments to be passed to `optim()`
#'
#' @return list
#' @export
#'
acf <- function(initial_values, data, type = c("cd", "tl"), ...) {
  type <- match.arg(type)

  if (type == "cd") {
    if (length(initial_values) != 3) stop("Estimating a Cobb-Douglas function requires initial values for b0, bl, and bk.")
    initial_values <- check_initial_values(x = initial_values, type = "cd")
    result <- stats::optim(par = initial_values, fn = acf_gmm_cd, data = data, ...)
  }

  if (type == "tl") {
    if (length(initial_values) != 6) stop("Estimating a Translog function requires initial values for b0, bl, bk, bll, bkk, and blk.")
    initial_values <- check_initial_values(x = initial_values, type = "tl")
    result <- stats::optim(par = initial_values, fn = acf_gmm_tl, data = data, ...)
  }

  return(result)
}
