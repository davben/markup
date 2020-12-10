#' Second Stage GMM
#'
#' Evaluate the moment conditions.
#'
#' @param param numeric
#' @param data data frame
#' @param instruments character
#'
#' @return numeric
acf_gmm <- function(param, data, instruments = c("const", "l_lag", "k", "phi_lag")){
  data <- data[!is.na(data$l_lag),]

  PHI <- data[, "phi"] %>% as.matrix()
  PHI_lag <- data[, "phi_lag"] %>% as.matrix()
  Z <- data[, instruments] %>% as.matrix()

  X <- data[, c("const", "l", "k")] %>% as.matrix()
  X_lag <- data[, c("const", "l_lag", "k_lag")] %>% as.matrix()

  Y <- data[, "y"] %>% as.matrix()
  C <- data[, "const"]
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
