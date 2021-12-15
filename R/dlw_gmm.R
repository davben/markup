#' Second Stage GMM (as in DLW)
#'
#' Evaluate the moment conditions of a Cobb-Douglas production function.
#'
#' @param param numeric
#' @param data data frame
#' @param instruments character
#'
#' @return numeric
#' @importFrom stats complete.cases
dlw_gmm_cd <- function(param, data, instruments = c("l_lag", "k")){
  data <- data[stats::complete.cases(data[, instruments]), ]

  if (!exists("const", data)) data$const <- 1

  PHI <- as.matrix(data[, "phi"])
  PHI_lag <- as.matrix(data[, "phi_lag"])
  Z <- as.matrix(data[, instruments])

  X <- as.matrix(data[, c("l", "k")])
  X_lag <- as.matrix(data[, c("l_lag", "k_lag")])

  C <- as.matrix(data[, "const"])
  betas <- as.matrix(c(param["bl"], param["bk"]), ncol = 1)

  OMEGA <- PHI - X %*% betas
  OMEGA_lag <- PHI_lag - X_lag %*% betas

  OMEGA_lag_pol <- cbind(OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3, C)

  g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA
  XI <- OMEGA - OMEGA_lag_pol %*% g_b

  crit <- t(t(Z) %*% XI)  %*%  (t(Z) %*% XI)

  return(crit)
}



#' Second Stage GMM (as in DLW)
#'
#' Evaluate the moment conditions of a Translog production function.
#'
#' @param param numeric
#' @param data data frame
#' @param instruments character
#'
#' @return numeric
#' @importFrom stats complete.cases
dlw_gmm_tl <- function(param, data, instruments = c("l_lag", "k", "ll_lag", "kk", "l_lag_k")){
  data <- data[stats::complete.cases(data[, instruments]), ]

  if (!exists("const", data)) data$const <- 1

  PHI <- as.matrix(data[, "phi"])
  PHI_lag <- as.matrix(data[, "phi_lag"])
  Z <- as.matrix(data[, instruments])

  X <- as.matrix(data[, c("l", "k", "ll", "kk", "lk")])
  X_lag <- as.matrix(data[, c("l_lag", "k_lag",  "ll_lag", "kk_lag", "lk_lag")])

  C <- as.matrix(data[, "const"])
  betas <- as.matrix(c(param["bl"], param["bk"], param["bll"], param["bkk"], param["blk"]),  ncol = 1)

  OMEGA <- PHI - X %*% betas
  OMEGA_lag <- PHI_lag - X_lag %*% betas

  OMEGA_lag_pol <- cbind(OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3, C)

  g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA
  XI <- OMEGA - OMEGA_lag_pol %*% g_b

  crit <- t(t(Z) %*% XI)  %*%  (t(Z) %*% XI)

  return(crit)
}
