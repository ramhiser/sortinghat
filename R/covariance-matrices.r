#' Constructs a p-dimensional intraclass covariance matrix.
#'
#' We define a \eqn{p \times p} intraclass covariance (correlation)
#' matrix to be \eqn{\Sigma = \sigma^2 (1 - \rho) J_p + \rho I_p},
#' where \eqn{-(p-1)^{-1} < \rho < 1}, \eqn{I_p} is the
#' \eqn{p \times p} identity matrix, and \eqn{J_p} denotes the
#' \eqn{p \times p} matrix of ones.
#'
#' @param p the dimension of the matrix
#' @param rho the intraclass covariance (correlation) constant
#' @param sigma2 the coefficient of the intraclass covariance matrix
#' @return an intraclass covariance matrix of size \eqn{p \times p}
cov_intraclass <- function(p, rho, sigma2 = 1) {
  if (rho <= -(p-1)^(-1) || rho >= 1) {
    stop("The value for 'rho' must be exclusively between -1 / (p - 1) and 1.")
  }
  if (sigma2 <= 0) {
    stop("The value for 'sigma2' must be positive.")
  }
  sigma2 * ((1 - rho) * matrix(1, nrow = p, ncol = p) + rho * diag(p))
}

#' Constructs a p-dimensional covariance matrix with an autocorrelation
#' (autoregressive) structure.
#'
#' The value of rho determines the amount of autocorrelation.
#'
#' @export
#' @param p the dimension of the matrix
#' @param rho the autocorrelation value
cov_autocorr <- function(p = 100, rho = 0.9) {
  mat <- diag(p)
  rho^abs(row(mat) - col(mat))
}
