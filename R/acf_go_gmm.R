#' Second Stage GMM (Gross Output)
#'
#' Evaluate the moment conditions of a gross output Cobb-Douglas production function.
#'
#' @param param numeric
#' @param data data frame
#' @param instruments character
#'
#' @return numeric
acf_go_gmm_cd <- function(param, data, instruments = c("const", "l_lag", "k", "m_lag", "phi_lag")){
  data <- data[complete.cases(data[, instruments]), ]

  if (!exists("const", data)) data$const <- 1

  PHI <- as.matrix(data[, "phi"])
  PHI_lag <- as.matrix(data[, "phi_lag"])
  Z <- as.matrix(data[, instruments])

  X <- as.matrix(data[, c("const", "l", "k", "m")])
  X_lag <- as.matrix(data[, c("const", "l_lag", "k_lag", "m_lag")])

  Y <- as.matrix(data[, "y"])
  C <- as.matrix(data[, "const"])
  betas <- as.matrix(c(param["b0"], param["bl"], param["bk"], param["bm"]), ncol = 1)

  OMEGA <- Y - X %*% betas
  OMEGA_lag <- PHI_lag - X_lag %*% betas

  OMEGA_lag_pol <- cbind(OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3)

  g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA
  XI <- OMEGA - OMEGA_lag_pol %*% g_b
  W <- solve(t(Z) %*% Z) / nrow(Z)

  crit <- t(t(Z) %*% XI)  %*% W %*%  (t(Z) %*% XI)

  return(crit)
}



#' Second Stage GMM (Gross Output)
#'
#' Evaluate the moment conditions of a gross output translog production function.
#'
#' @param param numeric
#' @param data data frame
#' @param instruments character
#'
#' @return numeric
acf_gmm_tl <- function(param, data, instruments = c("const", "l_lag", "k", "m_lag", "ll_lag", "kk",  "mm_lag", "l_lag_k", "lm_lag", "k_m_lag", "phi_lag")){
  data <- data[complete.cases(data[, instruments]), ]

  if (!exists("const", data)) data$const <- 1

  PHI <- as.matrix(data[, "phi"])
  PHI_lag <- as.matrix(data[, "phi_lag"])
  Z <- as.matrix(data[, instruments])

  X <- as.matrix(data[, c("const", "l", "k", "m", "ll", "kk", "mm", "lk", "lm", "km")])
  X_lag <- as.matrix(data[, c("const", "l_lag", "k_lag", "m_lag", "ll_lag", "kk_lag", "mm_lag", "lk_lag", "lm_lag" , "km_lag")])

  Y <- as.matrix(data[, "y"])
  C <- as.matrix(data[, "const"])
  betas <- as.matrix(c(param["b0"], param["bl"], param["bk"], param["bm"], param["bll"], param["bkk"], param["bmm"], param["blk"], param["blm"], param["bkm"]),  ncol = 1)

  OMEGA <- Y - X %*% betas
  OMEGA_lag <- PHI_lag - X_lag %*% betas

  OMEGA_lag_pol <- cbind(OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3)

  g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA
  XI <- OMEGA - OMEGA_lag_pol %*% g_b
  W <- solve(t(Z) %*% Z) / nrow(Z)

  crit <- t(t(Z) %*% XI)  %*% W %*%  (t(Z) %*% XI)

  return(crit)
}
