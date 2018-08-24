################################################################################
##
## $Id: test-calc-turnover.R 1300 2008-08-27 21:01:11Z zhao $
##
## Tests for function "calc.turnover"
##
################################################################################

context("calcs")

test_that("calc.turnover", {
  ## save(x.id, x.bucket, x.date, x.truth, file = "calc.turnover.test.RData", compress = TRUE)
  load("test_data/calc.turnover.test.RData")
  result <- backtest:::calc.turnover(x.id, x.bucket, x.date)
  expect_equal(x.truth, result)
})
