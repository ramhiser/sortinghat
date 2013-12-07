# classify 0.1

- Initial release of `classify`

## New Features

- Simulated data sets and configurations are each available in functions
  prefaced with `simdata_`. The `simdata` function is a wrapper around each of
  these. See `?simdata` for a list of all the available simulated data sets and
  the implementation details.

- Several error-rate estimators are available, including cross-validation .632,
  and several others. The name of each estimator's function is prefaced with
  `errorest_`. Also, `errorest` is a wrapper function around the error-rate
  estimators implemented. See `?errorest` for a list of all available error-rate
  estimators and the implementation details.

## Miscellaneous

- `cv_partition`: Partitions data for cross-validation.

- `partition_data`: Randomly partitions data sets into training and test data
  sets with a specified percentage in each.

- `which_min`: Determines the index (location) of the minimum element in a
  vector. Breaks ties in a variety of way -- in particular, at random. This
  function is intended to replace the base 'which.min' function.

- `cov_intraclass`: Constructs a p-dimensional covariance matrix.

- `cov_autocorrelation`: Constructs a p-dimensional covariance matrix with an
  autocorrelation (autoregressive) structure.

- `cov_block_autocorrelation`: Constructs a p-dimensional block-diagona
  covariance matrix with autocorrelated blocks. Based on Guo, Hastie, and
  Tibshirani (2007).