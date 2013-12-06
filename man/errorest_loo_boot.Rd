\name{errorest_loo_boot}
\alias{errorest_loo_boot}
\title{Calculates the Leave-One-Out (LOO) Bootstrap Error Rate for a specified
classifier given a data set.}
\usage{
  errorest_loo_boot(x, y, train, predict,
    num_bootstraps = 50, ...)
}
\arguments{
  \item{x}{a matrix of n observations (rows) and p features
  (columns)}

  \item{y}{a vector of n class labels}

  \item{train}{a function that builds the classifier. (See
  details.)}

  \item{predict}{a function that classifies observations
  from the constructed classifier from \code{train}. (See
  details.)}

  \item{num_bootstraps}{the number of bootstrap
  replications}

  \item{...}{additional arguments passed to the function
  specified in \code{train}.}
}
\value{
  the LOO-Boot error rate estimate
}
\description{
  For a given data matrix and its corresponding vector of
  labels, we calculate the LOO bootstrap (LOO-Boot) error
  rate for a given classifier.
}
\details{
  To calculate the LOO-Boot error rate, we sample from the
  data with replacement to obtain a bootstrapped training
  data set. We then train the given classifier (given in
  \code{train}) on the bootstrapped training data set and
  classify the observations from the original data set
  given in the matrix \code{x} that are not contained in
  the current bootstrapped training data set. We repeat
  this process \code{num_bootstraps} times. Then, for each
  observation in the original data set, we compute the
  proportion of times the observation was misclassified,
  based on the true labels given in \code{y}. We report the
  average of these proportions as the LOO-Boot error rate.

  For the given classifier, two functions must be provided
  1. to train the classifier and 2. to classify (predict)
  unlabeled observations. The training function is provided
  as \code{train} and the prediction function as
  \code{predict}.

  We expect that the first two arguments of the
  \code{train} function are \code{x} and \code{y},
  corresponding to the data matrix and the vector of their
  labels, respectively. Additional arguments can be passed
  to the \code{train} function.

  We stay with the usual R convention for the
  \code{predict} function. We expect that this function
  takes two arguments: 1. an \code{object} argument which
  contains the trained classifier returned from the
  function specified in \code{train}; and 2. a
  \code{newdata} argument which contains a matrix of
  observations to be classified -- the matrix should have
  rows corresponding to the individual observations and
  columns corresponding to the features (covariates). For
  an example, see \code{\link[MASS]{lda}}.
}
\examples{
require('MASS')
iris_x <- data.matrix(iris[, -5])
iris_y <- iris[, 5]

# Because the \\code{predict} function returns multiples objects in a list,
# we provide a wrapper function that returns only the class labels.
lda_wrapper <- function(object, newdata) { predict(object, newdata)$class }
set.seed(42)
errorest_loo_boot(x = iris_x, y = iris_y, train = MASS:::lda, predict = lda_wrapper)
# Output: 0.02307171
}

