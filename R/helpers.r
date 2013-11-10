#' Function to ensure all elements in a vector are the same
#'
#' @param x vector
#' @param tol tolerance value
#' @return logical value
#' @examples
#' # Returns TRUE
#' all_equal(c(3, 3, 3))
#' # Returns FALSE
#' all_equal(c(3, 3, 2))
all_equal <- function(x, tol = .Machine$double.eps^0.5) {
  diff(range(x)) <= tol
}
