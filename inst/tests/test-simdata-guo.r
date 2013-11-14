library('testthat')
library('classify')
library('mvtnorm')

context("Generate normal populations with Guo et al.'s (2007) covariance structure")

test_that("Two multivariate normal populations are generated correctly", {
  seed <- 42
  
  # Generates 10 observations from two multivariate normal populations having a
  # block-diagonal autocorrelation structure.
  sample_sizes <- c(10, 10)

  # Data from classify
  block_size <- 3
  num_blocks <- 3
  rho <- 0.9
  p <- block_size * num_blocks
  means <- list(seq_len(p), -seq_len(p))

  data <- simdata_guo(n = sample_sizes, mean = means, block_size = block_size,
                      num_blocks = num_blocks, rho = rho, seed = seed)

  # Manually generated test data
  cov_guo <- cov_block_autocorrelation(num_blocks = num_blocks,
                                       block_size = block_size,
                                       rho = rho)

  set.seed(seed)
  x1 <- rmvnorm(n = sample_sizes[1], mean = means[[1]], sigma = cov_guo)
  x2 <- rmvnorm(n = sample_sizes[2], mean = means[[2]], sigma = cov_guo)
  x <- rbind(x1, x2)

  y1 <- rep.int(1, times = sample_sizes[1])
  y2 <- rep.int(2, times = sample_sizes[2])
  y <- factor(c(y1, y2))
  
  # Tests that both the features and labels are equal
  expect_equal(data$x, x)
  expect_equal(data$y, y)
})

test_that("Three multivariate normal populations are generated correctly", {
  seed <- 42

  # Generates observations from each of three multivariate normal populations
  # with unequal covariance matrices.
  sample_sizes <- c(10, 20, 30)
  p <- 16
  block_size <- c(2, 4, 8)
  num_blocks <- p / block_size
  rho <- c(0.1, 0.5, 0.9)
  sigma2 <- 1:3
  means <- list(rep.int(-5, p), rep.int(0, p), rep.int(5, p))

  # Data from classify
  data <- simdata_guo(n = sample_sizes, mean = means,
                      block_size = block_size, num_blocks = num_blocks,
                      rho = rho, sigma2 = sigma2, seed = seed)

  # Manually generated test data
  cov_list <- lapply(1:3, function(k) {
      cov_block_autocorrelation(num_blocks = num_blocks[k],
                                block_size = block_size[k],
                                rho = rho[k],
                                sigma2 = sigma2[k])
  })

  set.seed(seed)
  x1 <- rmvnorm(n = sample_sizes[1], mean = means[[1]], sigma = cov_list[[1]])
  x2 <- rmvnorm(n = sample_sizes[2], mean = means[[2]], sigma = cov_list[[2]])
  x3 <- rmvnorm(n = sample_sizes[3], mean = means[[3]], sigma = cov_list[[3]])
  x <- rbind(x1, x2, x3)

  y1 <- rep.int(1, times = sample_sizes[1])
  y2 <- rep.int(2, times = sample_sizes[2])
  y3 <- rep.int(3, times = sample_sizes[3])
  y <- factor(c(y1, y2, y3))
  
  # Tests that both the features and labels are equal
  expect_equal(data$x, x)
  expect_equal(data$y, y)
})


test_that("Zero autocorrelation yields simdata_normal with identity covariance matrices", {
  seed <- 42
  
  # Generates 50 observations from two multivariate normal populations having a
  # block-diagonal autocorrelation structure.
  sample_sizes <- c(50, 50)

  block_size <- 3
  num_blocks <- 3
  rho <- 0
  p <- block_size * num_blocks
  cov_identity <- diag(p)
  
  means <- list(seq_len(p), -seq_len(p))

  # Data from classify's simdata_guo
  data_guo <- simdata_guo(n = sample_sizes, mean = means, block_size = block_size,
                          num_blocks = num_blocks, rho = rho, seed = seed)

  # Data from classify's simdata_normal
  data_normal <- simdata_normal(n = sample_sizes, mean = means,
                                cov = cov_identity, seed = seed) 

  # Tests that both the features and labels are equal
  expect_equal(data_guo$x, data_normal$x)
  expect_equal(data_guo$y, data_normal$y)
})
