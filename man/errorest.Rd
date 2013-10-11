\name{errorest}
\alias{errorest}
\title{Classification Error Rate Estimation for Statistical Learning}
\usage{
  errorest(x, y, estimator = "cv", train, predict,
    hold_out = NULL, num_folds = 10, ...)
}
\arguments{
  \item{x}{a matrix of n observations and p features}

  \item{y}{a vector of n class labels. (Must to be a
  'factor'.)}

  \item{train}{a function that builds the classifier. (See
  details.)}

  \item{predict}{a function that classifies observations
  from the constructed classifier from 'train'. (See
  details.)}

  \item{hold_out}{The hold-out size for cross-validation.}

  \item{num_folds}{The number of cross-validation folds.
  Ignored if hold_out is not NULL.}

  \item{...}{additional arguments passed to the function
  specified in 'train'.}
}
\value{
  TODO
}
\description{
  TODO
}
\examples{
TODO
}

