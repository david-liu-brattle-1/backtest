################################################################################
##
## $Id: test-internal-functions.R 1300 2008-08-27 21:01:11Z zhao $
##
## Tests for internal functions
##
################################################################################

context("calcs")

test_that(".bt.mean", {
  ## save(x, truth, file = "bt.mean.test.RData", compress = TRUE)
  load("test_data/bt.mean.test.RData")
  expect_equal(truth, backtest:::.bt.mean(x))
})

test_that(".bt.spread", {
  ## save(m, n, sd, truth, file = "bt.spread.test.RData", compress = TRUE)
  load("test_data/bt.spread.test.RData")
  expect_equal(truth, backtest:::.bt.spread(m, n, sd))
})

test_that(".bt.sharpe", {
  ## save(m, n, sd, truth, file = "bt.sharpe.test.RData", compress = TRUE)
  load("test_data/bt.sharpe.test.RData")
  bt.1 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE, date.var = "date", id.var = "id", natural = TRUE)
  expect_equal(truth.1, backtest:::.bt.sharpe(bt.1))
})

test_that("bucketize", {
  ## save(tmp.1, tmp.1.x, tmp.1.y, truth.1, file = "bucketize.test.RData", compress = TRUE)
  load("test_data/bucketize.test.RData")
  result.1 <- backtest:::bucketize(tmp.1, tmp.1.x, tmp.1.y, compute = length)
  expect_equal(truth.1, result.1)
})

test_that("bucketize2", {
  ## save(tmp.2, tmp.2.x, tmp.2.y, truth.2, file = "bucketize.test2.RData")
  load("test_data/bucketize.test2.RData")
  result.2 <- backtest:::bucketize(tmp.2, tmp.2.x, tmp.2.y, compute = mean)
  expect_equal(truth.2, result.2)
})
