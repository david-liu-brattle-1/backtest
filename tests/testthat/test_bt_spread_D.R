context("test function .bt.spread")


# test matrix with means of each(4) in.var/by.var
m_test_matrix <- matrix(runif(20), nrow = 4)
rownames(m_test_matrix) <- c("a", "b", "c", "d")

# test matrix with counts of each(4) in.var/byvar
n_test_matrix <- matrix(sample(c(800:1100), 20, replace = TRUE), nrow = 4)
rownames(n_test_matrix) <- c("a", "b", "c", "d")

# test standard deviation of ret.var
sd = 10

test_that("function .bt.spread returns right calculations", {
  
  # Expected result
  spread <- m_test_matrix[, 5] - m_test_matrix[, 1]
  se <- sd/sqrt(n_test_matrix[, 5] + n_test_matrix[, 1])
  
  # bt.spread uses z-score of 2
  ci_upper <- spread + 2*se
  ci_lower <- spread - 2*se
  
  by_hand_result <- cbind(spread, ci_lower, ci_upper)
  colnames(by_hand_result) <- c("spread", "CI(low)", "CI(high)")
  
  
  # Result from function
  fun_result <- backtest:::.bt.spread(m_test_matrix, n_test_matrix, sd)
  
  
  # Expect both results to be identical
  expect_identical(by_hand_result, fun_result)
  
})