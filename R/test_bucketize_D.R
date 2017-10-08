context("test bucketize function")

# Create dummy vectors to test the function
data_vector <- c(runif(10, min = -10, max = 10))
row_factor <- as.factor(base::sample(x = c("row1", "row2", "row3"), size = 10, replace = TRUE))
col_factor <- as.factor(base::sample(x = c("col1", "col2", "col3"), size = 10, replace = TRUE))

test_that("bucketize performs what is supposed to", {
  
  # expected result
  expect <- tapply(data_vector, list(col_factor, row_factor), FUN = mean)
  
  # result from the function
  result <- backtest:::bucketize(data_vector, row_factor, col_factor, compute = mean)
  
  # expect both results to be identical
  expect_identical(expect, result)
})


## test stopifnot
test_that("stopifnots in bucketize work", {
  
  # non-numeric x
  bad <- sample(x = c("A", "B", "C"), size = 10, replace = TRUE)
  expect_error(backtest:::bucketize(bad, row_factor, col_factor, compute = mean))
  
  # non-factor data_factor
  bad <- sample(x = c("A", "B", "C"), size = 10, replace = TRUE)
  expect_error(backtest:::bucketize(data_vector, bad, col_factor, compute = mean))
  expect_error(backtest:::bucketize(data_vector, row_factor, bad, compute = mean))
  
  # unequal-legth inputs
  expect_error(backtest:::bucketize(data_vector[1:9], row_factor, col_factor, compute = mean))
  expect_error(backtest:::bucketize(data_vector, row_factor[1:9], col_factor, compute = mean))
  expect_error(backtest:::bucketize(data_vector, row_factor, col_factor[1:9], compute = mean))
  
  # non-function compute
  expect_error(backtest:::bucketize(data_vector, row_factor, col_factor, compute = 2))
})