\name{cov_autocorrelation}
\alias{cov_autocorrelation}
\title{Constructs a p-dimensional covariance matrix with an autocorrelation
(autoregressive) structure.}
\usage{
  cov_autocorrelation(p = 100, rho = 0.9, sigma2 = 1)
}
\arguments{
  \item{p}{the size of the covariance matrix}

  \item{rho}{the autocorrelation value}

  \item{sigma2}{the variance of each feature}
}
\value{
  autocorrelated covariance matrix
}
\description{
  This function generates a \eqn{p \times p} autocorrelated
  covariance matrix with autocorrelation parameter
  \code{rho}. The variance \code{sigma2} is constant for
  each feature and defaulted to 1.
}
\details{
  The autocorrelated covariance matrix is defined as: The
  \eqn{(i,j)}th entry of the autocorrelated covariance
  matrix is defined as: \eqn{\rho^{|i - j|}}.

  The value of \code{rho} must be such that \eqn{|\rho| <
  1} to ensure that the covariance matrix is positive
  definite.
}

