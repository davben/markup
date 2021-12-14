#' Estimate a gross output production function
#'
#' @description
#' Using the two-stage proxy function approach suggested by Ackerberg, Caves, and Frazer (2015) (ACF),
#' estimate a Cobb-Douglas or translog gross output production function.
#' ACF only recommend their approach for structural value added functions and
#' Gandhi, Navarro, and Rivers (2020) show that a gross output function is not identified in this way.
#' However, De Loecker and Scott (2016) suggest, by using a serially correlated input price such as wages
#' in the control function, this approach is still valid.
#'
#' @param initial_values A named numeric vector of initial values for the GMM-procedure.
#' @param data A data frame of input factors and production output. Variable names need to follow a specific convention, see below.
#' @param type A string specifying the type of production function to be estimated, either "cd" for Cobb-Douglas or "tl" for translog.
#' @param ... additional arguments to be passed to `optim()` and the moment evaluator function. See Details for more information.
#'
#' @return A list with components (see [stats::optim()]):
#'
#' * `par` A vector of best parameter estimates.
#' * `value` The minimized moment function value.
#' * `counts` The number of calls to fn and gr.
#' * `convergence` An integer code. 0 indicates sucessful completion.
#' * `message` A character string potentially giving additional information.
#'
#' @importFrom stats optim
#' @export
#'
#' @details
#' ## Estimation
#' The implemented function uses a third-order polynomial to approximate the Markov process
#' of productivity growth.
#'
#' ## Initial values
#' The choice of initial values can significantly influence estimation results.
#' It is recommended to run estimations on a range of combinations of initial values.
#' Initial values must be provided for each coefficient of the underlying production function.
#' For a Cobb-Douglas type, this requires four initial values, `c(b0 = 0, bl = 0.4, bk = 0.3, bm = 0.3)`.
#' It is best practice to provide a named vector to avoid unnecessary, and potentially hard to debug mistakes.
#' Without any names, the estimation routine assumes the order of initial values to be (1) intercept, (2) labour,
#' (3) capital, and (4) materials coefficient.
#'
#' ## Instruments
#' The moment evaluator function by default uses lagged labour and materials as instruments as well as a constant, capital, and lagged phi.
#' In order to manually specify the instruments to be used in the GMM-stage, they can be added via the `instruments`-argument
#' as a character vector of variable names which gets passed to the moment function.
#' For example, adding same-period labour as an instrument is done using `instruments = c("const", "l", "l_lag", "k", "m_lag", "phi_lag")`
#' in the `acf_go()`-function.
#'
#' ## Data
#' Variable names are hard-coded in the estimation procedures of this package and follow conventions in the literature.
#' Depending on the functional type, the following variables as well as their lags are required (sufficient for Cobb-Douglas in bold):
#' * **y** (log output)
#' * **l** (log labour)
#' * **k** (log capital)
#' * **m** (log materials)
#' * **phi** (fitted value from a first-stage approximation)
#' * ll (squared log labour)
#' * kk (square log capital)
#' * mm (squared log materials)
#' * lk (interaction of log labour and log capital)
#' * lm (interaction of log labour and log materials)
#' * km (interaction of log capital and log materials)
#' * l_lag_k (interaction of lagged log labour and current log capital, no further lag required)
#' * k_m_lag (interaction of log capital and lagged log materials, no further lag required)
#' * l_lag_k_m_lag (interaction of lagged log labour, log capital and lagged log materials, no further lag required)
#'
#' @references
#' Ackerberg, D. A., Caves, K., & Frazer, G. (2015). Identification Properties of Recent Production Function Estimators. Econometrica, 83(6), 2411–2451. https://doi.org/10.3982/ECTA13408
#'
#' De Loecker, J., & Scott, P. T. (2016). Estimating market power Evidence from the US Brewing Industry (Working Paper Nr. 22957). National Bureau of Economic Research. https://doi.org/10.3386/w22957
#'
#' Gandhi, A., Navarro, S., & Rivers, D. A. (2020). On the Identification of Gross Output Production Functions. Journal of Political Economy, 128(8), 2973–3016. https://doi.org/10.1086/707736



acf_go <- function(initial_values, data, type = c("cd", "tl"), ...) {
  type <- match.arg(type)

  if (type == "cd") {
    if (length(initial_values) != 4) stop("Estimating a gross output Cobb-Douglas function requires initial values for b0, bl, bk, and bm.")
    initial_values <- check_initial_values(x = initial_values, type = "cdgo")
    result <- stats::optim(par = initial_values, fn = acf_go_gmm_cd, data = data, ...)
  }

  if (type == "tl") {
    if (length(initial_values) != 11) stop("Estimating a Translog function requires initial values for b0, bl, bk, bm, bll, bkk, bmm, blk, blm, bkm, and blkm.")
    initial_values <- check_initial_values(x = initial_values, type = "tlgo")
    result <- stats::optim(par = initial_values, fn = acf_go_gmm_tl, data = data, ...)
  }

  return(result)
}
