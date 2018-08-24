################################################################################
##
## $Id: test-functions.R 1300 2008-08-27 21:01:11Z zhao $
##
## Tests for function "backtest.function"
##
################################################################################

context("function")

check_truth <- function(truth, bt) {
  expect_equal(truth, list(results(bt), bt@buckets, ret.stats(bt), bt@ret.var, bt@calculated.data$turnover, summaryStats(bt)))
}

test_that("Function produces expected output", {
  ## save(x, truth.1, truth.2, truth.3, truth.4.A, truth.4.B, file = "backtest.function.test.RData", compress = TRUE)
  load("test_data/backtest.function.test.RData")

  bt.1 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)
  check_truth(truth.1, bt.1)

  bt.2 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.var = "country", by.period = FALSE)
  check_truth(truth.2, bt.2)

  bt.3 <- backtest(x, id.var = "id", in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", by.period = FALSE)
  check_truth(truth.3, bt.3)

  bt.4 <- backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = "ret.var.1", by.var = "country", by.period = FALSE)
  check_truth(truth.4, bt.4)

  bt.5 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", buckets = 2)
  check_truth(truth.5, bt.5)

  bt.6 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", natural = TRUE, id = "id", buckets = 2)
  check_truth(truth.6, bt.6)

  bt.7 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  check_truth(truth.7, bt.7)
})
