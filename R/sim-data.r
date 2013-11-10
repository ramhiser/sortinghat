#' Wrapper function to generate data from a variety of data-generating models.
#'
#' We provide a wrapper function to generate from three data-generating models:
#' \describe{
#'   \item{\code{\link{simdata_unif}}}{Multivariate uniform distributions}
#'   \item{\code{\link{simdata_normal}}}{Multivariate normal distributions with
#' intraclass covariance matrices}
#'   \item{\code{\link{simdata_t}}}{Multivariate Student's t distributions each
#' with a common covariance matrix}
#' }
#'
#' For each data-generating model, we generate \eqn{n_k} observations \eqn{(k =
#' 1, \ldots, K_0)} from each of \eqn{K_0} multivariate distributions so that
#' the Euclidean distance between each of the population centroids and the
#' origin is equal and scaled by \eqn{\Delta \ge 0}. For each model, the
#' argument \code{delta} controls this separation.
#'
#' This wrapper function is useful for simulation studies, where the efficacy of
#' supervised and unsupervised learning methods and algorithms are evaluated as
#' a the population separation is increased.
#'
#' @param family the family of distributions from which to generate data
#' @param ... optional arguments that are passed to the data-generating function
#' @return named list containing:
#' \describe{
#'   \item{x:}{A matrix whose rows are the observations generated and whose
#'   columns are the \code{p} features (variables)}
#'   \item{y:}{A vector denoting the population from which the observation in
#'   each row was generated.}
#' }
#' @export
#' @examples
#' set.seed(42)
#' uniform_data <- simdata_data(family = "uniform")
#' normal_data <- simdata_data(family = "normal", delta = 2)
#' student_data <- simdata_data(family = "student", delta = 1, df = 1:5)
simdata_data <- function(family = c("uniform", "normal", "t"), ...) {
  family <- match.arg(family)
  switch(family,
    uniform = simdata_unif(...),
    normal = simdata_normal(...),
    student = simdata_t(...)
  )
}
