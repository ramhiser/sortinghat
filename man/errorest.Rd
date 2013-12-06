\name{errorest}
\alias{errorest}
\title{Wrapper function to estimate the error rate of a classifier}
\usage{
  errorest(x, y,
    estimator = c("cv", "boot", "632", "632+", "bcv", "loo-boot", "apparent"),
    train, predict, ...)
}
\arguments{
  \item{x}{a matrix of n observations and p features}

  \item{y}{a vector of n class labels. (Must to be a
  'factor'.)}

  \item{train}{a function that builds the classifier. (See
  details.)}

  \item{predict}{a function that classifies observations
  from the constructed classifier from \code{train}. (See
  Details.)}

  \item{...}{additional arguments passed to the error-rate
  estimation code}
}
\value{
  an estimate of the classifier's error rate
}
\description{
  We provide a wrapper function to estimate the error rate
  of a classifier using any of the following estimators:#'
  \describe{
  \item{\code{\link{errorest_cv}}:}{Cross-validation Error
  Rate} \item{\code{\link{errorest_boot}}:}{Bootstrap Error
  Rate} \item{\code{\link{errorest_632}}:}{.632 Estimator
  from Efron (1983)}
  \item{\code{\link{errorest_632plus}}:}{.632+ Estimator
  from Efron and Tibshirani (1997)}
  \item{\code{\link{errorest_bcv}}:}{Bootstrap
  Cross-validation from Fu et al. (2005)}
  \item{\code{\link{errorest_loo_boot}}:}{Leave-One-Out
  Bootstrap Error Rate}
  \item{\code{\link{errorest_apparent}}:}{Apparent Error
  Rate} }
}
\details{
  This wrapper function provides a common means to estimate
  classification error rates and is useful for simulation
  studies where multiple error-rate estimators are being
  considered.

  For details about an individual error-rate estimator,
  please see its respective documentation.
}
\examples{
require('MASS')
iris_x <- data.matrix(iris[, -5])
iris_y <- iris[, 5]

# Because the \\code{predict} function returns multiples objects in a list,
# we provide a wrapper function that returns only the class labels.
lda_wrapper <- function(object, newdata) { predict(object, newdata)$class }

# Cross-Validation (default)
errorest(x = iris_x, y = iris_y, train = MASS:::lda, predict = lda_wrapper)

# .632
errorest(x = iris_x, y = iris_y, estimator = "632", train = MASS:::lda,
         predict = lda_wrapper)

# Bootstrap Error Rate
# The argument 'num_bootstraps' is passed on to 'errorest_boot'
errorest(x = iris_x, y = iris_y, estimator = "boot", train = MASS:::lda,
         predict = lda_wrapper, num_bootstraps = 42)
}

