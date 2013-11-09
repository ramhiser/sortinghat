\name{simdata_guo}
\alias{simdata_guo}
\title{Returns a data frame of two multivariate normal data sets with their population labels.
The data sets share a common covariance matrix defined in Guo (2007).}
\usage{
  simdata_guo(n = 100, p = 100, rho = 0.9,
    block_size = 100, seed = NULL)
}
\arguments{
  \item{n}{sample size of each population}

  \item{p}{dimension of the generated data}

  \item{rho}{autocorrelation of each block matrix; must be
  in [-1, 1].}

  \item{block_size}{size of each block in the covariance
  matrix.}

  \item{seed}{the random number seed of the data.}
}
\value{
  named list containing: \describe{ \item{x:}{A matrix
  whose rows are the observations generated and whose
  columns are the \code{p} features (variables)}
  \item{y:}{A vector denoting the population from which the
  observation in each row was generated.} }
}
\description{
  TODO: Import mvtnorm:::rmvnorm
}
\examples{
data_generated <- simdata_guo(seed = 42)
dim(data_generated$x)
}

