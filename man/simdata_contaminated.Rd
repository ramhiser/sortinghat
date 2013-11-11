\name{simdata_contaminated}
\alias{simdata_contaminated}
\title{Generates random variates from K multivariate contaminated normal
populations.}
\usage{
  simdata_contaminated(n, mean, cov, epsilon = rep(0, K),
    kappa = rep(1, K), seed = NULL)
}
\arguments{
  \item{n}{a vector (of length K) of the sample sizes for
  each population}

  \item{mean}{a vector or a list (of length K) of mean
  vectors}

  \item{cov}{a symmetric matrix or a list (of length K) of
  symmetric covariance matrices.}

  \item{epsilon}{a vector (of length K) indicating the
  probability of sampling a contaminated population (i.e.,
  outlier) for each population}

  \item{kappa}{a vector (of length K) that determines the
  amount of scale contamination for each population}

  \item{seed}{seed for random number generation (If
  \code{NULL}, does not set seed)}
}
\value{
  named list containing: \describe{ \item{x:}{A matrix
  whose rows are the observations generated and whose
  columns are the \code{p} features (variables)}
  \item{y:}{A vector denoting the population from which the
  observation in each row was generated.} }
}
\description{
  We generate \eqn{n_k} observations \eqn{(k = 1, \ldots,
  K)} from each of \eqn{K} multivariate contaminated normal
  distributions. Let \eqn{N_p(\mu, \Sigma)} denote the
  p-dimensional multivariate normal distribution with mean
  vector \eqn{\mu} and positive-definite covariance matrix
  \eqn{\Sigma}.  Then, let the \eqn{k}th population have a
  \eqn{p}-dimensional multivariate contaminated normal
  distribution:
}
\details{
  \deqn{(1 - \epsilon_k) N_p(\mu_k, \Sigma_k) + \epsilon_k
  N_p(\mu_k, \kappa_k \Sigma_k),}

  where \eqn{\epsilon_k \in [0, 1]} is the probability of
  sampling from a contaminated population (i.e., outlier)
  and \eqn{\kappa_k \ge 1} determines the amount of scale
  contamination. The contaminated normal distribution can
  be viewed as a mixture of two multivariate normal random
  distributions, where the second has a scaled covariance
  matrix, which can introduce extreme outliers for
  sufficiently large \eqn{\kappa_k}.

  The number of populations, \code{K}, is determined from
  the length of the vector of sample sizes, code{n}. The
  mean vectors and covariance matrices each can be given in
  a list of length \code{K}. If one covariance matrix is
  given (as a matrix or a list having 1 element), then all
  populations share this common covariance matrix. The same
  logic applies to population means.

  The contamination probabilities in \code{epsilon} can be
  given as a numeric vector or a single value, in which
  case the degrees of freedom is replicated \code{K} times.
  The same idea applies to the scale contamination in the
  \code{kappa} argument.

  By default, \code{epsilon} is a vector of zeros, and
  \code{kappa} is a vector of ones. Hence, no contamination
  is applied by default.
}
\examples{
# Generates 10 observations from each of two multivariate contaminated normal
# populations with equal covariance matrices. Each population has a
# contamination probability of 0.05 and scale contamination of 10.
mean_list <- list(c(1, 0), c(0, 1))
cov_identity <- diag(2)
data <- simdata_contaminated(n = c(10, 10), mean = mean_list,
                             cov = cov_identity, epsilon = 0.05, kappa = 10,
                             seed = 42)
dim(data$x)
table(data$y)

# Generates 10 observations from each of three multivariate contaminated
# normal populations with unequal covariance matrices. The contamination
# probabilities and scales differ for each population as well.
set.seed(42)
mean_list <- list(c(-3, -3), c(0, 0), c(3, 3))
cov_list <- list(cov_identity, 2 * cov_identity, 3 * cov_identity)
data2 <- simdata_contaminated(n = c(10, 10, 10), mean = mean_list,
                              cov = cov_list, epsilon = c(0.05, 0.1, 0.2),
                              kappa = c(2, 5, 10))
dim(data2$x)
table(data2$y)
}

