################################################################################
##
## $Id: test-categorize.R
##
## Tests for function "categorize"
##
################################################################################

context("categorize")

test_that("categorize1", {
  ## save(tmp.1, tmp.1.n, truth.1, tmp.2, truth.2, file = "categorize.test.RData", compress = TRUE)
  load("test_data/categorize.test.RData")
  result <- backtest:::categorize(tmp.1, n = tmp.1.n)
  expect_equal(truth.1, result)
})

test_that("categorize2", {
  ## save(tmp.1, tmp.1.n, truth.1, tmp.2, truth.2, file = "categorize.test.RData", compress = TRUE)
  load("test_data/categorize.test.RData")
  result <- backtest:::categorize(tmp.2)
  expect_equal(truth.2, result)
})
