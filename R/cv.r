#' Calculates the Cross-Validation Error Rate for a specified classifier given a
#' data set.
#'
#' For a given data matrix and its corresponding vector of labels, we calculate
#' the cross-validation (CV) error rate for a given classifier.
#'
#' To calculate the CV error rate, we partition the data set into 'folds'. For
#' each fold, we consider the observations within the fold as a test data set,
#' while the remaining observations are considered as a training data set. We
#' then calculate the number of misclassified observations within the fold. We
#' aggregate the number of misclassified observations across all of the folds
#' via a weighted average to compute the CV error rate.
#'
#' Rather than partitioning the observations into folds, an alternative
#' convention is to specify the 'hold-out' size for each test data set. Note that
#' this convention is equivalent to the notion of folds. We allow the user to
#' specify either option with the \code{hold_out} and \code{num_folds} arguments.
#' The \code{num_folds} argument is the default option but is ignored if the
#' \code{hold_out} argument is specified (i.e. is not \code{NULL}).
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
#' @param num_folds the number of cross-validation folds. Ignored if
#' \code{hold_out} is not \code{NULL}. See Details.
#' @param hold_out the hold-out size for cross-validation. See Details.
#' @param ... additional arguments passed to the function specified in
#' \code{train}.
#' @return the calculated CV error rate estimate
#' @examples
#' require('MASS')
#' iris_x <- data.matrix(iris[, -5])
#' iris_y <- iris[, 5]
#'
#' # Because the \code{classify} function returns multiples objects in a list,
#' # we provide a wrapper function that returns only the class labels.
#' lda_wrapper <- function(object, newdata) { predict(object, newdata)$class }
#' set.seed(42)
#' error_cv(x = iris_x, y = iris_y, train = MASS:::lda, classify = lda_wrapper)
#' # Output: 0.02
error_cv <- function(x, y, train, classify, num_folds = 10, hold_out = NULL, ...) {
  x <- as.matrix(x)
  y <- as.factor(y)
  check_out <- check_arguments(x = x, y = y, train = train, classify = classify)

  list_partitions <- cv_partition(y, num_folds = num_folds, hold_out = hold_out)

  # For each fold, we calculate the number of misclassified observations.
  num_misclassified <- sapply(list_partitions, function(fold) {
    classify_obj <- with(fold, train(x[training, ], y[training], ...))
    classifications <- with(fold, classify(object = classify_obj,
                                          newdata = x[-training, ]))
    with(fold, sum(classifications != y[-training]))
  })
  # To compute the CV error rate, we sum up the total number of misclassified
  # observations and then divide through by the sample size (the length of 'y').
  sum(num_misclassified) / length(y)
}

#' Partitions data for cross-validation.
#'
#' For a vector of training labels, we return a list of cross-validation folds,
#' where each fold has the indices of the observations to leave out in the fold.
#' In terms of classification error rate estimation, one can think of a fold as a
#' the observations to hold out as a test sample set. Either the \code{hold_out}
#' size or the number of folds, \code{num_folds}, can be specified. The number
#' of folds defaults to 10, but if the \code{hold_out} size is specified, then
#' \code{num_folds} is ignored.
#'
#' We partition the vector \code{y} based on its length, which we treat as the
#' sample size, 'n'. If an object other than a vector is used in \code{y}, its
#' length can yield unexpected results. For example, the output of
#' \code{length(diag(3))} is 9.
#'
#' @export
#' @param y a vector of the labels of the training data
#' @param num_folds the number of cross-validation folds. Ignored if
#' \code{hold_out} is not \code{NULL}. See Details.
#' @param hold_out the hold-out size for cross-validation. See Details.
#' @param seed optional random number seed for splitting the data for cross-validation
#' @return list the indices of the training and test observations for each fold.
#' @examples
#' library(MASS)
#' # The following three calls to \code{cv_partition} yield the same partitions.
#' set.seed(42)
#' cv_partition(iris$Species)
#' cv_partition(iris$Species, num_folds = 10, seed = 42)
#' cv_partitions(iris$Species, hold_out = 15, seed = 42)
cv_partition <- function(y, num_folds = 10, hold_out = NULL, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  n <- length(y)

  if (!is.null(hold_out)) {
    hold_out <- as.integer(hold_out)
    num_folds <- ceiling(n / hold_out)
  }
  folds <- split(sample(seq_len(n), n), gl(n = num_folds, k = 1, length = n))
  folds <- lapply(folds, function(fold) {
    list(
      training = which(!seq_along(y) %in% fold),
      test = fold
    )
  })
  names(folds) <- paste("Fold", names(folds), sep = "")
  folds
}

