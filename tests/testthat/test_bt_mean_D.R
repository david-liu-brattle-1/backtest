context("test function bt.mean")

## a matrix to test a function
test_matrix <- matrix(runif(15, min = -1, max = 1), nrow = 5)

test_that("function bt.mean works properly", {
  
  ## How bt.mean should work
  by_hand_result <- matrix(apply(test_matrix, 2 , mean), nrow = 1)
  rownames(by_hand_result) <- c("MEAN")
  
  
  ## How bt.mean actually works
  fun_result <- backtest:::.bt.mean(test_matrix)
  
  
  ## Expect 2 resutls to be identical
  expect_identical(by_hand_result, fun_result)

})