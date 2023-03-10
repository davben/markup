% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acf_go_gmm.R
\name{acf_go_gmm_cd}
\alias{acf_go_gmm_cd}
\title{Second Stage GMM (Gross Output)}
\usage{
acf_go_gmm_cd(
  param,
  data,
  instruments = c("const", "l_lag", "k", "m_lag", "phi_lag"),
  controls = c("log_w", "nace3", "year")
)
}
\arguments{
\item{param}{numeric}

\item{data}{data frame}

\item{instruments}{character}

\item{controls}{character}
}
\value{
numeric
}
\description{
Evaluate the moment conditions of a gross output Cobb-Douglas production function.
}
