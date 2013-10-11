library('testthat')
library('classify')

context("Cross-Validation Error Rate")

# Sample size of class k
n_k <- 10
p <- 5
num_classes <- 2
n <- num_classes * n_k
num_folds <- 10
hold_out <- 1

x <- replicate(p, rnorm(n))
y <- gl(num_classes, n_k)

# Below we repeatedly use 'e_msg' as the error message that should be thrown.

test_that("The number of training observations matches the number of class labels", {
  e_msg <- "The number of observations must match the number of class labels."

  expect_error(error_cv(x = x, y = y[-1], train = MASS:::lda), e_msg)
  expect_error(error_cv(x = x[-1,], y = y, train = MASS:::lda), e_msg)
})

test_that("Class labels are provided as factors.", {
  e_msg <- "Class labels should be provided as factors."

  expect_error(error_cv(x = x, y = as.vector(y), train = MASS:::lda), e_msg)
  expect_error(error_cv(x = x, y = as.numeric(y), train = MASS:::lda), e_msg)
  expect_error(error_cv(x = x, y = as.integer(y), train = MASS:::lda), e_msg)
  expect_error(error_cv(x = x, y = as.character(y), train = MASS:::lda), e_msg)
})

test_that("There is at least one class present.", {
  e_msg <- "There must be at least two classes given in the class labels vector 'y'."

  expect_error(error_cv(x = x, y = factor(y, levels = 1), train = MASS:::lda), e_msg)
  expect_error(error_cv(x = x, y = gl(1, n), train = MASS:::lda), e_msg)
})

test_that("Classes have at least two observations.", {
  e_msg <- "Each class should have at least two observations."

  y_tmp <- replace(y, which(y == 1), 2)
  expect_error(error_cv(x = x, y = y_tmp, train = MASS:::lda), e_msg)

  y_tmp <- replace(y, which(y == 2), 1)
  expect_error(error_cv(x = x, y = y_tmp, train = MASS:::lda), e_msg)

  y_tmp <- y
  y_tmp[which(y_tmp != 1)[-1]] <- 1
  expect_error(error_cv(x = x, y = y_tmp, train = MASS:::lda), e_msg)
})

test_that("The functions 'train' and 'predict' are specified correctly.", {
  e_msg <- "The 'train' function must be specified."
  expect_error(error_cv(x = x, y = y, predict = "awesome"), e_msg)
  expect_error(error_cv(x = x, y = y, train = NULL, predict = "awesome"), e_msg)
  expect_error(error_cv(x = x, y = y), e_msg)
  expect_error(error_cv(x = x, y = y, train = NULL), e_msg)

  e_msg <- "The 'train' function does not exist."
  expect_error(error_cv(x = x, y = y, train = "awesomeness"), e_msg)
  expect_error(error_cv(x = x, y = y, train = "awesomeness", predict = NULL), e_msg)
  expect_error(error_cv(x = x, y = y, train = "awesomeness", predict = "awesome"), e_msg)

  e_msg <- "The 'predict' function does not exist."
  expect_error(error_cv(x = x, y = y, train = MASS:::lda, predict = "awesome"), e_msg)
})

test_that("The matrix 'x' and the class labels vector 'y' are specified.", {
  e_msg <- "The matrix 'x' must be specified."
  expect_error(error_cv(y = y, train = "awesomeness"), e_msg)
  expect_error(error_cv(x = NULL, y = y, train = "awesomeness"), e_msg)
  expect_error(error_cv(y = y, train = "awesomeness", predict = "yes!"), e_msg)
  expect_error(error_cv(x = NULL, y = y, train = "awesomeness", predict = "yes!"), e_msg)

  e_msg <- "The class labels vector 'y' must be specified."
  expect_error(error_cv(x = x, train = "awesomeness"), e_msg)
  expect_error(error_cv(x = x, y = NULL, train = "awesomeness"), e_msg)
  expect_error(error_cv(x = x, train = "awesomeness", predict = "yes!"), e_msg)
  expect_error(error_cv(x = x, y = NULL, train = "awesomeness", predict = "yes!"), e_msg)
})




