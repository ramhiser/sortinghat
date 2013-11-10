\name{all_equal}
\alias{all_equal}
\title{Function to ensure all elements in a vector are the same}
\usage{
  all_equal(x, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{vector}

  \item{tol}{tolerance value}
}
\value{
  logical value
}
\description{
  Function to ensure all elements in a vector are the same
}
\examples{
# Returns TRUE
all_equal(c(3, 3, 3))
# Returns FALSE
all_equal(c(3, 3, 2))
}

