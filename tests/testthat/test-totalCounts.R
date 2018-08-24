################################################################################
##
## $Id: test-totalCounts.R 1300 2008-08-27 21:01:11Z zhao $
##
## Test-case for the totalCounts method of the backtest object.
##
################################################################################

context("function")

test_that("totalCounts", {
  # save(x, true.tc, file = "backtest.totalCounts.test.RData")
  load("test_data/backtest.totalCounts.test.RData")
  bt <- backtest(x,
    in.var = c("ret_12_0_m", "vim"), ret.var = "ret_0_1_y",
    by.period = FALSE, date.var = "date"
  )
  expect_equal(true.tc, totalCounts(bt))
})
