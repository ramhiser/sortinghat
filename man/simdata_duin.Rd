\name{simdata_duin}
\alias{simdata_duin}
\title{Generate mixture (non-Gaussian) data in the format described in Skurichina
and Duin (1998).}
\usage{
  simdata_duin(n = 100, p = 10, seed = NULL)
}
\arguments{
  \item{n}{sample size of population}

  \item{p}{dimension of the generated data}

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
  Generate mixture (non-Gaussian) data in the format
  described in Skurichina and Duin (1998).
}
\examples{
data_generated <- simdata_duin(seed = 42)
dim(data_generated$x)
}
\references{
  Skurichina and Duin (1998)
}

