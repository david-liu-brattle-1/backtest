################################################################################
##
## $Id: test-plots.R
##
## Check that the plots execute without error
##
################################################################################

context("Plots")

test_that("plot.return", {
  pdf(NULL)
  on.exit(dev.off())
  load("test_data/backtest.function.test.RData")
  bt.6 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", natural = TRUE, id = "id", buckets = 2)
  suppressWarnings(plot(bt.6, type = "return"))

  bt.7 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  suppressWarnings(plot(bt.7, type = "return"))

  expect_equal(1, 1)
})

test_that("plot.turnover", {
  pdf(NULL)
  on.exit(dev.off())
  load("test_data/backtest.function.test.RData")
  bt.6 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", natural = TRUE, id = "id", buckets = 2)
  suppressWarnings(plot(bt.6, type = "turnover"))

  bt.7 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  suppressWarnings(plot(bt.7, type = "turnover"))

  expect_equal(1, 1)
})


test_that("plot.cumreturn", {
  pdf(NULL)
  on.exit(dev.off())
  load("test_data/backtest.function.test.RData")
  bt.6 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", natural = TRUE, id = "id", buckets = 2)
  suppressWarnings(plot(bt.6, type = "cumreturn"))

  bt.7 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  suppressWarnings(plot(bt.7, type = "cumreturn"))

  expect_equal(1, 1)
})


test_that("plot.cumreturn.split", {
  pdf(NULL)
  on.exit(dev.off())
  load("test_data/backtest.function.test.RData")
  bt.6 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", natural = TRUE, id = "id", buckets = 2)
  suppressWarnings(plot(bt.6, type = "cumreturn.split"))

  bt.7 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  suppressWarnings(plot(bt.7, type = "cumreturn.split"))

  expect_equal(1, 1)
})
