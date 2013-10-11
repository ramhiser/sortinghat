#' Returns a p-dimensional matrix with an autocorrelation (autoregressive)
#' structure. The value of rho determines the amount of autocorrelation.
#'
#' @export
#' @param p the dimension of the matrix
#' @param rho the autocorrelation value
autocorr_mat <- function(p = 100, rho = 0.9) {
  mat <- diag(p)
  rho^abs(row(mat) - col(mat))
}
