#' Generate mixture (non-Gaussian) data in the format described in Skurichina
#' and Duin (1998).
#'
#' @references Skurichina and Duin (1998)
#' @param n sample size of population
#' @param p dimension of the generated data
#' @param seed the random number seed of the data.
#' @return named list containing:
#' \describe{
#'   \item{x:}{A matrix whose rows are the observations generated and whose
#'   columns are the \code{p} features (variables)}
#'   \item{y:}{A vector denoting the population from which the observation in
#'   each row was generated.}
#' }
#' @export
#' @examples
#' data_generated <- simdata_duin(seed = 42)
#' dim(data_generated$x)
simdata_duin <- function(n = 100, p = 10, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)

  # There are two populations in this generated data set.
  num_groups <- 2
	
  # First we generate two multivariate normal samples that share a common
  # covariance matrix but have different means. The first mean is a
  # p-dimensional vector of 0's.
  mean1 <- replicate(p, 0)
	
  # The mean of the second class is 0.3 for the first feature, 3 for the second
  # feature, and zero for the remaining features.
  mean2 <- c(0.3, 3, replicate(p - 2, 0))
	
  # The variance for the covariance matrix are 0.01 and 40 for the first two
  # dimensions. The rest of the variances are 1.
  Sig <- diag(c(0.01, 40, replicate(p - 2, 1)))

  # x1 contains the generated random variates.
  x1 <- rbind(
    rmvnorm(n/2, mean = mean1, sigma = Sig),
    rmvnorm(n/2, mean = mean2, sigma = Sig)
  )
  y1 <- gl(num_groups, n/2)
	
  # Next we generate two additional multivariate normal samples to obtain a
  # mixture of highly correlated non-Gaussian classes.

  # The first mean is a p-dimensional vector with the first two features having
  # mean 10 and the remaining features having mean 0.
  mean1 <- c(10, 10, replicate(p - 2, 0))

  # The mean of the second class is -9.7 for the first feature,
  # -7 for the second feature,
  # and zero for the remaining features.
  mean2 <- c(-9.7, -7, replicate(p - 2, 0))

  # The variance for the covariance matrix are 0.01 and 40 for the first two
  # dimensions. The rest of the variances are 1.
  Sig <- diag(c(0.01, replicate(p - 1, 1)))

  # x2 contains the generated random variates.
  x2 <- rbind(
    rmvnorm(n/2, mean = mean1, sigma = Sig),
    rmvnorm(n/2, mean = mean2, sigma = Sig)
  )
  y2 <- gl(num_groups, n/2)

  # The list contains two classes each with a mixture of Gaussian populations,
  # such that the resulting classes are highly correlated and non-Gaussian.
  list(x = rbind(x1, x2), y = factor(c(y1, y2)))
}
