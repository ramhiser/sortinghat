\name{generate_friedman}
\alias{generate_friedman}
\title{Function to generate data in the format described in Friedman (1989)}
\usage{
  generate_friedman(n1 = 10, n2 = 10, n3 = 10, p = 2,
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
  list x and y
}
\description{
  Function to generate data in the format described in
  Friedman (1989)
}
\references{
  Friedman, J. H. (1989), "Regularized Discriminant
  Analysis," Journal of American Statistical Association,
  84, 405, 165-175.
}

