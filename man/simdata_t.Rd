\name{simdata_t}
\alias{simdata_t}
\title{Generates random variates from K multivariate Student's t populations.}
\usage{
  simdata_t(n, centroid, cov, df, seed = NULL)
}
\arguments{
  \item{n}{a vector (of length K) of the sample sizes for
  each population}

  \item{centroid}{a vector or a list (of length K) of
  centroid vectors}

  \item{cov}{a symmetric matrix or a list (of length K) of
  symmetric covariance matrices.}

  \item{df}{a vector (of length K) of the degrees of
  freedom for each population}

  \item{seed}{seed for random number generation (If
  \code{NULL}, does not set seed) \describe{ \item{x:}{A
  matrix whose rows are the observations generated and
  whose columns are the \code{p} features (variables)}
  \item{y:}{A vector denoting the population from which the
  observation in each row was generated.} }}
}
\description{
  We generate \eqn{n_k} observations \eqn{(k = 1, \ldots,
  K_0)} from each of \eqn{K_0} multivariate Student's t
  distributions such that the Euclidean distance between
  each of the means and the origin is equal and scaled by
  \eqn{\Delta \ge 0}.
}
\details{
  Let \eqn{\Pi_k} denote the \eqn{k}th population with a
  \eqn{p}-dimensional multivariate Student's t
  distribution, \eqn{T_p(\mu_k, \Sigma_k, c_k)}, where
  \eqn{\mu_k} is the population location vector,
  \eqn{\Sigma_k} is the positive-definite covariance
  matrix, and \eqn{c_k} is the degrees of freedom.

  For small values of \eqn{c_k}, the tails are heavier,
  and, therefore, the average number of outlying
  observations is increased.

  The number of populations, \code{K}, is determined from
  the length of the vector of sample sizes, code{n}. The
  centroid vectors and covariance matrices each can be
  given in a list of length \code{K}. If one covariance
  matrix is given (as a matrix or a list having 1 element),
  then all populations share this common covariance matrix.
  The same logic applies to population centroids. The
  degrees of freedom can be given as a numeric vector or a
  single value, in which case the degrees of freedom is
  replicated \code{K} times.
}
\examples{
# Generates 10 observations from each of two multivariate t populations
# with equal covariance matrices and equal degrees of freedom.
centroid_list <- list(c(3, 0), c(0, 3))
cov_identity <- diag(2)
data_generated <- simdata_t(n = c(10, 10), centroid = centroid_list,
                            cov = cov_identity, df = 4, seed = 42)
dim(data_generated$x)
table(data_generated$y)

# Generates 10 observations from each of three multivariate t populations
# with unequal covariance matrices and unequal degrees of freedom.
set.seed(42)
centroid_list <- list(c(-3, -3), c(0, 0), c(3, 3))
cov_list <- list(cov_identity, 2 * cov_identity, 3 * cov_identity)
data_generated2 <- simdata_t(n = c(10, 10, 10), centroid = centroid_list,
                             cov = cov_list, df = c(4, 6, 10))
dim(data_generated2$x)
table(data_generated2$y)
}

