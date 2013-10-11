#' Helper function that partitions a data set into training and test data sets.
#'
#' The function randomly partitions a data set into training and test data sets
#' with a specified percentage of observations assigned to the training data set.
#' The user can optionally preserve the proportions of the original data set.
#'
#' A named list is returned with the training and test data sets.
#'
#' @export
#' @param x a matrix of n observations (rows) and p features (columns)
#' @param y a vector of n class labels
#' @param split_pct the percentage of observations that will be randomly assigned
#' to the training data set. The remainder of the observations will be assigned
#' to the test data set.
#' @param preserve_proportions logical value. If \code{TRUE}, the training and
#' test data sets will be constructed so that the original proportions are
#' preserved.
#' @return named list containing the training and test data sets:
#' \itemize{
#'   \item \code{train_x}: matrix of the training observations
#'   \item \code{train_y}: vector of the training labels (coerced to factors).
#'   \item \code{test_x}: matrix of the test observations
#'   \item \code{test_y}: vector of the test labels (coerced to factors).
#' }
#' @examples
#' require('MASS')
#' x <- iris[, -5]
#' y <- iris[, 5]
#' set.seed(42)
#' data <- partition_data(x = x, y = y)
#' table(data$train_y)
#' table(data$test_y)
#'
#' data <- partition_data(x = x, y = y, preserve_proportions = TRUE)
#' table(data$train_y)
#' table(data$test_y)
partition_data <- function(x, y, split_pct = 2/3, preserve_proportions = FALSE) {
  x <- as.matrix(x)
  y <- as.factor(y)

  if (preserve_proportions) {
    train <- tapply(seq_along(y), y, function(i) {
      sample(i, size = length(i) * split_pct)
    })
    train <- do.call(c, train)
  } else {
    train <- sample(seq_along(y), size = length(y) * split_pct)
  }
  list(train_x = x[train, ], train_y = y[train], test_x = x[-train, ],
       test_y = y[-train])
}

