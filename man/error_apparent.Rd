\name{error_apparent}
\alias{error_apparent}
\title{Calculates the Apparent Error Rate for a specified classifier given a data
set.}
\usage{
  error_apparent(x, y, train, classify, ...)
}
\arguments{
  \item{x}{a matrix of n observations (rows) and p features
  (columns)}

  \item{y}{a vector of n class labels}

  \item{train}{a function that builds the classifier. (See
  details.)}

  \item{classify}{a function that classifies observations
  from the constructed classifier from \code{train}. (See
  details.)}

  \item{...}{additional arguments passed to the function
  specified in \code{train}.}
}
\value{
  object of class \code{errorest}. The object is a named
  \code{list} that contains the following elements:
  \itemize{ \item \code{classifier}: the classifier
  constructed from the given data set \item
  \code{classifications}: the classified class labels for
  the test data set \item \code{error}: the calculated
  error rate estimate }
}
\description{
  For a given data matrix and its corresponding vector of
  labels, we calculate the apparent error rate (AER) for a
  given classifier.
}
\details{
  The AER simply uses the data set as both the training and
  test data sets. The AER is well known to be biased
  downward in that it underestimates the true error rate of
  the classifier.

  We expect that the first two arguments of the classifier
  function given in \code{train} are \code{x} and \code{y},
  corresponding to the data matrix and the vector of their
  labels. Additional arguments can be passed to the
  \code{train} function. The returned object should be a
  classifier that will be passed to the function given in
  the \code{classify} argument.

  We stay with the usual R convention for the
  \code{classify} function. We expect that this function
  takes two arguments: 1. an \code{object} argument which
  contains the trained classifier returned from the
  function specified in \code{train}; and 2. a
  \code{newdata} argument which contains a matrix of
  observations to be classified -- the matrix should have
  rows corresponding to the individual observations and
  columns corresponding to the features (covariates).
}
\examples{
require('MASS')
iris_x <- data.matrix(iris[, -5])
iris_y <- iris[, 5]

# Because the \\code{classify} function returns multiples objects in a list,
# we provide a wrapper function that returns only the class labels.
lda_wrapper <- function(object, newdata) { predict(object, newdata)$class }
error_apparent(x = iris_x, y = iris_y, train = MASS:::lda, classify = lda_wrapper)
# Output: 0.02

The following code is equivalent for this example:
lda_classifications <- predict(MASS:::lda(x = iris_x, grouping = iris_y), newdata = iris_x)$class
mean(lda_predictions != iris_y)
# Output: 0.02
}

