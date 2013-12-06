\name{errorest_bcv}
\alias{errorest_bcv}
\title{Calculates the Bootstrap Cross-Validation (BCV) Error Rate Estimator for a
specified classifier given a data set.}
\usage{
  errorest_bcv(x, y, train, predict, num_bootstraps = 50,
    num_folds = 10, hold_out = NULL, ...)
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

  \item{num_folds}{the number of cross-validation folds.
  Ignored if \code{hold_out} is not \code{NULL}. See
  Details.}

  \item{hold_out}{the hold-out size for cross-validation.
  See Details.}

  \item{...}{additional arguments passed to the function
  specified in \code{train}.}
}
\value{
  the BCV error rate estimate
}
\description{
  For a given data matrix and its corresponding vector of
  labels, we calculate the bootstrap cross-validation (BCV)
  error rate from Fu, Carroll, and Wang (2005) for a given
  classifier.
}
\details{
  To calculate the BCV error rate, we sample from the data
  with replacement to obtain a bootstrapped training data
  set. We then compute a cross-validation error rate with
  the given classifier (given in \code{train}) on the
  bootstrapped training data set. We repeat this process
  \code{num_bootstraps} times to obtain a set of
  bootstrapped cross-validation error rates. We report the
  average of these error rates. The
  \code{\link{errorest_cv}} function is used to compute the
  cross-validation (CV) error rate estimator for each
  bootstrap iteration.

  Fu et al. (2005) note that the BCV method works well
  because it is a bagging prediction error. Furthermore,
  consider the leave-one-out (LOO) error rate estimator.
  For small sample sizes, the data are sparse, so that the
  left out observation has a high probability of being far
  in distance from the remaining training data set. Hence,
  the LOO error rate estimator yields a large variance for
  small data sets.

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

# Because the \\code{predict} function returns multiples objects in a list,
# we provide a wrapper function that returns only the class labels.
lda_wrapper <- function(object, newdata) { predict(object, newdata)$class }
set.seed(42)
errorest_bcv(x = iris_x, y = iris_y, train = MASS:::lda,
             predict = lda_wrapper)
# Output: 0.02213333

errorest_bcv(x = iris_x, y = iris_y, train = MASS:::lda,
             num_bootstraps = 20, predict = lda_wrapper, hold_out = 1)
# Output: 0.022
}
\references{
  Fu, W.J., Carroll, R.J., and Wang, S. (2005), "Estimating
  misclassification error with small samples via bootstrap
  cross-validation," Bioinformatics, vol. 21, no. 9, pp.
  1979–1986.
}

