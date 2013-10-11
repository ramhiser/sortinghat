\name{generate_intraclass}
\alias{generate_intraclass}
\title{Generates p-dimensional multivariate normal data with an intraclass covariance
matrix.}
\usage{
  generate_intraclass(n, mean, rho_k)
}
\arguments{
  \item{n}{vector of sample sizes. Will generate n
  observations for the kth class.}

  \item{p}{the feature space dimension for all K classes.}

  \item{mean}{K x p matrix whose columns contain the mean
  of the kth class.}

  \item{rho_k}{vector of correlations. If this is a
  constant, each class will use the same rho.}
}
\description{
  Generates p-dimensional multivariate normal data with an
  intraclass covariance matrix.
}

