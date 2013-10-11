\name{which_min}
\alias{which_min}
\title{Helper function that determines which element in a vector is the minimum. Ties
can be broken randomly or via first/last ordering.}
\usage{
  which_min(x, break_ties = c("random", "first", "last"))
}
\arguments{
  \item{x}{vector}

  \item{break_ties}{method to break ties. The \code{random}
  method selects the index of the minimum elements
  randomly, while the \code{first} and \code{last} options
  imply that the first or last instance of the minimum
  element will be chosen, respectively.}
}
\value{
  location of the minimum element in the vector \code{x}.
  If there is a tie, we break the tie with the method
  specified in \code{break_ties}.
}
\description{
  The \code{which_min} function is intended to be an
  alternative to the base \code{which.min} function when a
  specific tie-breaking method is necessary.
}
\examples{
set.seed(42)
z <- runif(5)
z <- c(z[1], z[1], z)

which_min(z)
which_min(z, break_ties = "first")
which_min(z, break_ties = "last")
}

