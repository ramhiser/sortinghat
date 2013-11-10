\name{simdata_normal}
\alias{simdata_normal}
\title{Generates random variates from K multivariate normal populations.}
\usage{
  simdata_normal(n, mean, cov, seed = NULL)
}
\arguments{
  \item{n}{a vector (of length K) of the sample sizes for
  each population}

  \item{mean}{a vector or a list (of length K) of vectors}

  \item{cov}{a symmetric matrix or a list (of length K) of
  symmetric matrices.}

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
  K)} from each of \eqn{K} multivariate normal
  distributions. Let the \eqn{k}th population have a
  \eqn{p}-dimensional multivariate normal distribution,
  \eqn{N_p(\mu_k, \Sigma_k)} with mean vector \eqn{\mu_k}
  and positive-definite covariance matrix \eqn{\Sigma_k}.
}
\details{
  The number of populations, \code{K}, is determined from
  the length of the vector of sample sizes, code{n}. The
  mean vectors and covariance matrices each can be given in
  a list of length \code{K}. If one covariance matrix is
  given (as a matrix or a list having 1 element), then all
  populations share this common covariance matrix. The same
  logic applies to population means.
}
\examples{
# Generates 10 observations from each of two multivariate normal populations
# with equal covariance matrices.
mean_list <- list(c = c(1, 0), c(0, 1))
cov_identity <- diag(2)
data_generated <- simdata_normal(n = c(10, 10), mean = mean_list,
                                 cov = cov_identity, seed = 42)
dim(data_generated$x)
table(data_generated$y)

# Generates 10 observations from each of three multivariate normal
# populations with equal covariance matrices.
set.seed(42)
mean_list <- list(c = c(-3, -3), c(0, 0), c(3, 3))
cov_list <- list(cov_identity, 2 * cov_identity, 3 * cov_identity)
data_generated2 <- simdata_normal(n = c(10, 10), mean = mean_list,
                                  cov = cov_list)
dim(data_generated2$x)
table(data_generated2$y)
}

