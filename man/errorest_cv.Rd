\name{errorest_cv}
\alias{errorest_cv}
\title{Calculates the Cross-Validation Error Rate for a specified classifier given a
data set.}
\usage{
  errorest_cv(x, y, train, classify, num_folds = 10,
    hold_out = NULL, ...)
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

  \item{num_folds}{the number of cross-validation folds.
  Ignored if \code{hold_out} is not \code{NULL}. See
  Details.}

  \item{hold_out}{the hold-out size for cross-validation.
  See Details.}

  \item{...}{additional arguments passed to the function
  specified in \code{train}.}
}
\value{
  the calculated CV error rate estimate
}
\description{
  For a given data matrix and its corresponding vector of
  labels, we calculate the cross-validation (CV) error rate
  for a given classifier.
}
\details{
  To calculate the CV error rate, we partition the data set
  into 'folds'. For each fold, we consider the observations
  within the fold as a test data set, while the remaining
  observations are considered as a training data set. We
  then calculate the number of misclassified observations
  within the fold. We aggregate the number of misclassified
  observations across all of the folds via a weighted
  average to compute the CV error rate.

  Rather than partitioning the observations into folds, an
  alternative convention is to specify the 'hold-out' size
  for each test data set. Note that this convention is
  equivalent to the notion of folds. We allow the user to
  specify either option with the \code{hold_out} and
  \code{num_folds} arguments. The \code{num_folds} argument
  is the default option but is ignored if the
  \code{hold_out} argument is specified (i.e. is not
  \code{NULL}).

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
set.seed(42)
errorest_cv(x = iris_x, y = iris_y, train = MASS:::lda, classify = lda_wrapper)
# Output: 0.02
}

