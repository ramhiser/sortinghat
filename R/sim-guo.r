#' Returns a data frame of two multivariate normal data sets with their population labels.
#' The data sets share a common covariance matrix defined in Guo (2007).
#'
#' @export
#' @param n sample size of each population
#' @param p dimension of the generated data
#' @param rho autocorrelation of each block matrix; must be in [-1, 1].
#' @param block_size size of each block in the covariance matrix.
#' @param seed the random number seed of the data.
generate_guo <- function(n = 100, p = 100, rho = 0.9, block_size = 100, seed = NULL) {
  stopifnot(p %% block_size == 0)
  if(!is.null(seed)) set.seed(seed)
	
  # This paper only deals with binary data.
  num_groups <- 2
  num_blocks <- p / block_size
          
  # The first mean is a p-dimensional vector of 0's.
  mean1 <- replicate(p, 0)
          
  # The second mean is a p-dimensional vector of 0.5's.
  mean2 <- replicate(p, 0.5)

  # The common covariance matrix is a block-matrix filled with autocorrelation matrices.
  # See paper for more details.
  sigma <- matrix(0, nrow = p, ncol = p)
  for(i in seq_len(num_blocks)) {
    sigma[seq.int((i - 1) * block_size + 1,i * block_size),] <-
      cbind(
        matrix(0, nrow = block_size, ncol = (i - 1) * block_size),
        autocorr_mat(block_size, (-1)^(i+1) * rho),
        matrix(0, nrow = block_size, ncol = (num_blocks - i) * block_size)
      ) 
  }

  # x contains the generated random variates.
  x <- rbind(
    rmvnorm(n, mean = mean1, sigma = sigma),
    rmvnorm(n, mean = mean2, sigma = sigma)
  )
  # y contains the labels of each observation.
  y <- gl(num_groups, n)
  # The list contains three multivariate Gaussian populations of dimension p
  # with configurations detailed in Guo (2007).
  list(x = x, y = y)
}
