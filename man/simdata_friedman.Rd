\name{simdata_friedman}
\alias{simdata_friedman}
\title{Function to generate data in the format described in Friedman (1989)}
\usage{
  simdata_friedman(n1 = 10, n2 = 10, n3 = 10, p = 2,
    experiment = 1, seed = NULL)
}
\arguments{
  \item{n1}{sample size of group 1}

  \item{n2}{sample size of group 2}

  \item{n3}{sample size of group 3}

  \item{p}{dimension of the generated data}

  \item{experiment}{the experiment number from the RDA
  paper}

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
  Function to generate data in the format described in
  Friedman (1989)
}
\examples{
data_generated <- simdata_friedman(seed = 42)
dim(data_generated$x)
}
\references{
  Friedman, J. H. (1989), "Regularized Discriminant
  Analysis," Journal of American Statistical Association,
  84, 405, 165-175.
}

