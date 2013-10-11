#' Function to generate data in the format described in Friedman (1989)
#'
#' @export
#' @param n1 sample size of group 1
#' @param n2 sample size of group 2
#' @param n3 sample size of group 3
#' @param p dimension of the generated data
#' @param experiment the experiment number from the RDA paper
#' @param seed the random number seed of the data.
#' @references Friedman, J. H. (1989), "Regularized Discriminant Analysis,"
#' Journal of American Statistical Association, 84, 405, 165-175.
#' @return list x and y
generate_friedman <- function(n1 = 10, n2 = 10, n3 = 10, p = 2, experiment = 1,
                              seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  # The number of groups is 3 in Friedman (1989).
  num_groups <- 3
	
  stopifnot(is.element(experiment, 1:6), n1 >= 3, n2 >= 3, n3 >= 3)
          
  mean1 <- mean2 <- mean3 <- NULL
  cov1 <- cov2 <- cov3 <- NULL
          
  params <- NULL
          
  experiment1 <- function() {
    mean1 <- rep(0, p)
    mean2 <- c(3, rep(0, p - 1))
    mean3 <- c(0, 3, rep(0, p - 2))

    cov1 <- diag(p)
    cov2 <- diag(p)
    cov3 <- diag(p)
                    
    list(
      mean1 = mean1, mean2 = mean2, mean3 = mean3,
      cov1 = cov1, cov2 = cov2, cov3 = cov3
    )
  }
 
  experiment2 <- function() {
    mean1 <- rep(0, p)
    mean2 <- c(3, rep(0, p - 1))
    mean3 <- c(0, 4, rep(0, p - 2))

    cov1 <- diag(p)
    cov2 <- 2 * diag(p)
    cov3 <- 3 * diag(p)

    list(
      mean1 = mean1, mean2 = mean2, mean3 = mean3,
      cov1 = cov1, cov2 = cov2, cov3 = cov3
    )
  }

  experiment3 <- function() {
    eigenvals <- (9 * ((seq_len(p) - 1)/(p - 1)) + 1)^2

    mean1 <- rep(0, p)
    mean2 <- 2.5 * sqrt(eigenvals / p) * (p - seq_len(p)) / (p / 2 - 1)
    mean3 <- (rep(-1, p) ^ seq_len(p)) * mean2

    cov1 <- diag(eigenvals)
    cov2 <- cov1
    cov3 <- cov1

    list(
      mean1 = mean1, mean2 = mean2, mean3 = mean3,
      cov1 = cov1, cov2 = cov2, cov3 = cov3
    )
  }

  experiment4 <- function() {
    eigenvals <- (9 * ((seq_len(p) - 1)/(p - 1)) + 1)^2

    mean1 <- rep(0, p)
    mean2 <- 2.5 * sqrt(eigenvals/p) * (seq_len(p) - 1) / (p/2 - 1)
    mean3 <- (rep(-1, p)^seq_len(p)) * mean2

    cov1 <- diag(eigenvals)
    cov2 <- cov1
    cov3 <- cov1

    list(
      mean1 = mean1, mean2 = mean2, mean3 = mean3,
      cov1 = cov1, cov2 = cov2, cov3 = cov3
    )
  }
 
  experiment5 <- function() {
    eigenvals_1 <- (9 * ((seq_len(p) - 1)/(p - 1)) + 1)^2
    eigenvals_2 <- (9 * (p - seq_len(p))/(p - 1) + 1)^2
    eigenvals_3 <- (9 * (seq_len(p) - (p - 1)/2) / (p - 1))^2

    mean1 <- rep(0, p)
    mean2 <- mean1
    mean3 <- mean1

    cov1 <- diag(eigenvals_1)
    cov2 <- diag(eigenvals_2)
    cov3 <- diag(eigenvals_3)

    list(
      mean1 = mean1, mean2 = mean2, mean3 = mean3,
      cov1 = cov1, cov2 = cov2, cov3 = cov3
    )
  }

  experiment6 <- function() {
    eigenvals_1 <- (9 * ((seq_len(p) - 1)/(p - 1)) + 1)^2
    eigenvals_2 <- (9 * (p - seq_len(p))/(p - 1) + 1)^2
    eigenvals_3 <- (9 * (seq_len(p) - (p - 1)/2) / (p - 1))^2

    mean1 <- rep(0, p)
    mean2 <- rep(14 / sqrt(p), p)
    mean3 <- (rep(-1, p)^seq_len(p)) * mean2

    cov1 <- diag(eigenvals_1)
    cov2 <- diag(eigenvals_2)
    cov3 <- diag(eigenvals_3)

    list(
      mean1 = mean1, mean2 = mean2, mean3 = mean3,
      cov1 = cov1, cov2 = cov2, cov3 = cov3
    )
  }

  switch(experiment,
    params <- experiment1(),
    params <- experiment2(),
    params <- experiment3(),
    params <- experiment4(),
    params <- experiment5(),
    params <- experiment6()
  )
	
  # We use Friedman's original setup except we fix the number of observations
  # rather than randomly choosing the number of class observations.
  x <- rbind(
    rmvnorm(n1, mean = params$mean1, sigma = params$cov1),
    rmvnorm(n2, mean = params$mean2, sigma = params$cov2),
    rmvnorm(n3, mean = params$mean3, sigma = params$cov3)
  )
  y <- factor(c(rep(1, n1), rep(2, n2), rep(3, n3)))

  # The list contains three multivariate Gaussian populations of dimension p
  # with configurations detailed in Friedman (1989).
  list(x = x, y = y)
}
