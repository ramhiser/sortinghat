\name{check_arguments}
\alias{check_arguments}
\title{Checks the arguments passed to the error rate estimator functions.}
\usage{
  check_arguments(x, y, train, predict, ...)
}
\arguments{
  \item{x}{a matrix of n observations and p features}

  \item{y}{a vector of n class labels}

  \item{train}{a function that builds the classifier (See
  details)}

  \item{predict}{a function that classified observations
  from the constructed classifier from \code{train}. (See
  details.)}
}
\value{
  \code{TRUE} invisibly if no errors are encountered.
}
\description{
  This function is a helper function that checks the
  arguments passed to make sure the input is valid and
  consistent across all error rate estimators.
}
\details{
  We expect that the first two arguments of the classifier
  function given in \code{train} are \code{x} and \code{y},
  corresponding to the data matrix and the vector of their
  labels. Additional arguments can be passed to the
  \code{train} function. The returned object should be a
  classifier that will be passed to the function given in
  the \code{classify} argument.
}

