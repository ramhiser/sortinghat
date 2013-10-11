library('testthat')
library('classify')

context("Partitioning a Data Set for Cross-validation")

# Sample size of class k
n_k <- 10
p <- 5
num_classes <- 2
n <- num_classes * n_k

x <- replicate(p, rnorm(n))
y <- gl(num_classes, n_k)

# Below we repeatedly use 'e_msg' as the error message that should be thrown.
# Also, 'w_msg' refers to warning messages.
test_that("At least one of hold_out and num_folds is specified", {
  e_msg <- "At least one of 'hold_out' and 'num_folds' must be specified"

  expect_true(FALSE)
  # expect_error(error_apparent(x = x, y = y[-1], train = MASS:::lda), e_msg)  
})


test_that("The number of folds is inclusively between 2 and TODO", {
  e_msg <- "The 'num_folds' must be inclusively between 2 and TODO"

  expect_true(FALSE)
  # expect_error(error_apparent(x = x, y = y[-1], train = MASS:::lda), e_msg)  
})


test_that("The hold_out size is inclusively between 1 and n - 2", {
  e_msg <- "The 'hold_out' size must be inclusively between 1 and n - 2"

  expect_true(FALSE)
  # expect_error(error_apparent(x = x, y = y[-1], train = MASS:::lda), e_msg)  
})

test_that("The random number seed is either NULL or an integer", {
  e_msg <- "The random number seed must be either NULL or an integer"

  expect_error(cv_partition(y = gl(2, 5), seed = "yes"), e_msg)
  expect_error(cv_partition(y = gl(2, 5), hold_out = 3, seed = "yes"), e_msg)  
  expect_error(cv_partition(y = gl(2, 5), seed = diag(3)), e_msg)
  expect_error(cv_partition(y = gl(2, 5), hold_out = 3, seed = diag(3)), e_msg)  
})

test_that("A warning is thrown if a partition results in less than 2 training observations in a class.", {
  w_msg <- "No class has less than 2 training observations"

  expect_true(FALSE)
  # expect_error(error_apparent(x = x, y = y[-1], train = MASS:::lda), e_msg)  
})

e_msg <- "TODO"

