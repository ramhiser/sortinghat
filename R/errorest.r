#' Valid Error Rate Estimator Selections
ERROR_EST <- c("cv", "apparent")

#' Classification Error Rate Estimation for Statistical Learning
#'
#' TODO
#'
#' @param x a matrix of n observations and p features
#' @param y a vector of n class labels. (Must to be a 'factor'.)
#' @param train a function that builds the classifier. (See details.)
#' @param predict a function that classifies observations from the constructed classifier from 'train'. (See details.)
#' @param hold_out The hold-out size for cross-validation.
#' @param num_folds The number of cross-validation folds. Ignored if hold_out is not NULL.
#' @param ... additional arguments passed to the function specified in 'train'.
#' @return TODO
#' @export
#' @examples
#' TODO
errorest <- function(x, y, estimator = "cv", train, predict, hold_out = NULL,
                     num_folds = 10, ...) {
  x <- as.matrix(x)
  y <- as.factor(y)
  
  # Checks for valid arguments.
  check_out <- check_arguments(x = x, y = y, train = train, predict = predict,
                               ...)

  # Checks if the specified estimator matches the given ones.
  estimator <- match.arg(arg = estimator, choices = ERROR_EST)
  estimator_f <- paste("error", estimator, sep = "_")

  # Grabs the necessary arguments along with the optional arguments in '...'.
  args <- list(x = x, y = y, train = train, predict = predict)
  dots <- list(...)
  args <- c(args, dots)

  # Depending on the specified estimator, we add specific arguments to the list
  # of arguments to pass to the appropriate estimator.
  args <- switch(estimator,
                 "apparent" = args,
                 "cv" = c(args, hold_out = hold_out, num_folds = num_folds)
                 )
  # Calls the specified estimator with the appropriate function
  results <- do.call(estimator_f, args)
  
  list(results = results, estimator = estimator)
}
