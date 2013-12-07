\name{all_equal}
\alias{all_equal}
\title{Function to check whether all elements in a numeric vector are equal within
some tolerance}
\usage{
  all_equal(x, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{numeric vector}

  \item{tol}{tolerance value}
}
\value{
  logical value
}
\description{
  Function to check whether all elements in a numeric
  vector are equal within some tolerance
}
\examples{
# Returns TRUE
all_equal(c(3, 3, 3))
# Returns FALSE
all_equal(c(3, 3, 2))
}

