#' Generates p-dimensional multivariate normal data with an intraclass covariance
#' matrix.
#'
#' @param n vector of sample sizes. Will generate n observations for the kth class.
#' @param p the feature space dimension for all K classes.
#' @param mean K x p matrix whose columns contain the mean of the kth class.
#' @param rho_k vector of correlations. If this is a constant, each class will use the same rho.
generate_intraclass <- function(n, mean, rho_k) {
  n <- as.vector(n)
  # TODO: Allow the user to specify p.
  p <- nrow(mean)
  K <- length(n)
  rho_k <- as.vector(matrix(rho_k, ncol = K))

  x_df <- data.frame(do.call(rbind,
    llply(seq_len(K), function(i) {
      cbind(i, rmvnorm(n[i], mean[,i], intraclass_cov(p, rho_k[i])))
    })
  ))

  list(x = data.matrix(x_df[,-1]), y = factor(x_df[,1]))
}

