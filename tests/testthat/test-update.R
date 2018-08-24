################################################################################
##
## $Id: test-update.R
##
## Tests for update consistency
##
################################################################################


test_that("backtest.update.identity", {
  load("test_data/backtest.function.test.RData")
  bt <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  bt.new <- backtest:::backtest.update(bt)
  expect_equal(results(bt), results(bt.new))
})

test_that("backtest.update", {
  load("test_data/backtest.function.test.RData")
  bt.original <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)
  bt.mutated <- backtest.update(bt = bt.original, ret.var = "ret.var.2")
  bt.control <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.2", by.period = FALSE)

  expect_equal(results(bt.mutated), results(bt.control))

  # Not Equal
  expect_error(expect_equal(results(bt.original), results(bt.control)))
})
