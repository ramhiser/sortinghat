\name{generate_guo}
\alias{generate_guo}
\title{Returns a data frame of two multivariate normal data sets with their population labels.
The data sets share a common covariance matrix defined in Guo (2007).}
\usage{
  generate_guo(n = 100, p = 100, rho = 0.9,
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
\description{
  Returns a data frame of two multivariate normal data sets
  with their population labels. The data sets share a
  common covariance matrix defined in Guo (2007).
}

