#' Calculates the .632 Error Rate for a specified classifier given a data set.
#'
#' For a given data matrix and its corresponding vector of labels, we calculate
#' the .632 error rate for a given classifier.
#'
#' To calculate the .632 error rate, we compute the leave-one-out (LOO) bootstrap
#' error rate and the apparent error rate. Then, we compute a convex combination
#' of these two error rates estimators.
#'
#' To calculate the apparent error rate, we use the \code{errorest_apparent}
#' function. Similarly, to calculate the LOO bootstrap (LOO-Boot) error rate, we
#' use the \code{errorest_loo_boot} function. In some cases (e.g. simulation study)
#' one, if not both, of these error rate estimators might already be computed.
#' Hence, we allow the user to provide these values if they are already computed;
#' by default, the arguments are \code{NULL} to indicate that they are
#' unavailable.
#'
#' We expect that the first two arguments of the classifier function given in
#' \code{train} are \code{x} and \code{y}, corresponding to the data matrix and
#' the vector of their labels. Additional arguments can be passed to the
#' \code{train} function. The returned object should be a classifier that will
#' be passed to the function given in the \code{classify} argument.
#'
#' We stay with the usual R convention for the \code{classify} function. We
#' expect that this function takes two arguments: 1. an \code{object} argument
#' which contains the trained classifier returned from the function specified in
#' \code{train}; and 2. a \code{newdata} argument which contains a matrix of
#' observations to be classified -- the matrix should have rows corresponding to
#' the individual observations and columns corresponding to the features
#' (covariates).
#'
#' @export
#' @param x a matrix of n observations (rows) and p features (columns)
#' @param y a vector of n class labels
#' @param train a function that builds the classifier. (See details.)
#' @param classify a function that classifies observations from the constructed
#' classifier from \code{train}. (See details.)
#' @param num_bootstraps the number of bootstrap replications
#' @param ... additional arguments passed to the function specified in
#' \code{train}.
#' @return the 632 error rate estimate
#' @examples
#' require('MASS')
#' iris_x <- data.matrix(iris[, -5])
#' iris_y <- iris[, 5]
#'
#' # Because the \code{classify} function returns multiples objects in a list,
#' # we provide a wrapper function that returns only the class labels.
#' lda_wrapper <- function(object, newdata) { predict(object, newdata)$class }
#' set.seed(42)
#'
#' # We compute the apparent and LOO-Boot error rates up front to demonstrate
#' # that they can be computed before the \code{errorest_632} function is called.
#'
#' set.seed(42)
#' apparent <- errorest_apparent(x = iris_x, y = iris_y, train = MASS:::lda,
#'                               classify = lda_wrapper)
#' set.seed(42)
#' loo_boot <- errorest_loo_boot(x = iris_x, y = iris_y, train = MASS:::lda,
#'                               classify = lda_wrapper)
#'
#' # Each of the following 3 calls should result in the same error rate.
#' # 1. The apparent error rate is provided, while the LOO-Boot must be computed.
#' set.seed(42)
#' errorest_632(x = iris_x, y = iris_y, train = MASS:::lda, classify = lda_wrapper,
#'              apparent = apparent)
#' # 2. The LOO-Boot error rate is provided, while the apparent must be computed.
#' set.seed(42)
#' errorest_632(x = iris_x, y = iris_y, train = MASS:::lda, classify = lda_wrapper,
#'              loo_boot = loo_boot)
#' # 3. Both error rates are provided, so the calculation is quick.
#' errorest_632(x = iris_x, y = iris_y, train = MASS:::lda, classify = lda_wrapper,
#'              apparent = apparent, loo_boot = loo_boot)
#'
#' # In each case the output is: 0.02194132
errorest_632 <- function(x, y, train, classify, num_bootstraps = 50,
                           apparent = NULL, loo_boot = NULL, ...) {
  x <- as.matrix(x)
  y <- as.factor(y)
  check_out <- check_arguments(x = x, y = y, train = train, classify = classify)

  if (is.null(apparent)) {
    apparent <- errorest_apparent(x = x, y = y, train = train, classify = classify,
                               ...)
  }
  
  if (is.null(loo_boot)) {
    loo_boot <- errorest_loo_boot(x = x, y = y, train = train, classify = classify,
                               num_bootstrap = num_bootstraps, ...)
  }
  0.368 * apparent + 0.632 * loo_boot
}
