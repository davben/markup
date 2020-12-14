#' Second Stage GMM
#'
#' Evaluate the moment conditions of a Cobb-Douglas production function.
#'
#' @param param numeric
#' @param data data frame
#' @param instruments character
#'
#' @return numeric
acf_gmm_cd <- function(param, data, instruments = c("const", "l_lag", "k", "phi_lag")){
  data <- data[!is.na(data$l_lag),]

  PHI <- as.matrix(data[, "phi"])
  PHI_lag <- as.matrix(data[, "phi_lag"])
  Z <- as.matrix(data[, instruments])

  X <- as.matrix(data[, c("const", "l", "k")])
  X_lag <- as.matrix(data[, c("const", "l_lag", "k_lag")])

  Y <- as.matrix(data[, "y"])
  C <- as.matrix(data[, "const"])
  betas <- as.matrix(c(param["b0"], param["bl"], param["bk"]), ncol = 1)

  OMEGA <- Y - X %*% betas
  OMEGA_lag <- PHI_lag - X_lag %*% betas

  OMEGA_lag_pol <- cbind(OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3)

  g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA
  XI <- OMEGA - OMEGA_lag_pol %*% g_b
  W <- solve(t(Z) %*% Z) / nrow(Z)

  crit <- t(t(Z) %*% XI)  %*% W %*%  (t(Z) %*% XI)

  return(crit)
}



#' Second Stage GMM
#'
#' Evaluate the moment conditions of a Translog production function.
#'
#' @param param numeric
#' @param data data frame
#' @param instruments character
#'
#' @return numeric
acf_gmm_tl <- function(param, data, instruments = c("const", "l_lag", "k", "ll_lag", "kk", "l_lag_k", "phi_lag")){
  data <- data[!is.na(data$l_lag),]

  PHI <- as.matrix(data[, "phi"])
  PHI_lag <- as.matrix(data[, "phi_lag"])
  Z <- as.matrix(data[, instruments])

  X <- as.matrix(data[, c("const", "l", "k", "ll", "kk", "lk")])
  X_lag <- as.matrix(data[, c("const", "l_lag", "k_lag",  "ll_lag", "kk_lag", "lk_lag")])

  Y <- as.matrix(data[, "y"])
  C <- as.matrix(data[, "const"])
  betas <- as.matrix(c(param["b0"], param["bl"], param["bk"], param["bll"], param["bkk"], param["blk"]),  ncol = 1)

  OMEGA <- Y - X %*% betas
  OMEGA_lag <- PHI_lag - X_lag %*% betas

  OMEGA_lag_pol <- cbind(OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3)

  g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA
  XI <- OMEGA - OMEGA_lag_pol %*% g_b
  W <- solve(t(Z) %*% Z) / nrow(Z)

  crit <- t(t(Z) %*% XI)  %*% W %*%  (t(Z) %*% XI)

  return(crit)
}
