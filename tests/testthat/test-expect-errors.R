################################################################################
##
## $Id: test-expect-errors.R
##
## Tests for function "backtest.function"
##
################################################################################

context("Errors")


test_that("Error: Only one in.var of class factor or character allowed", {
  load("test_data/backtest.function.test.RData")
  ## Only one in.var of class factor or character allowed
  expect_error(
    backtest(x, in.var = c("country", "sector"), ret.var = "ret.var.1", by.period = TRUE, date.var = "date", id.var = "id", natural = TRUE)
  )
})

test_that("Error: Only one by.var allowed per backtest", {
  load("test_data/backtest.function.test.RData")
  ## Only one by.var allowed per backtest.
  expect_error(
    backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = "ret.var.1", by.var = c("country", "sector"), by.period = FALSE)
  )
})

test_that("Error: only one id.var allowed", {
  load("test_data/backtest.function.test.RData")
  ## only one id.var allowed.
  expect_error(
    backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = "ret.var.1", by.var = c("country", "sector"), id.var = c("id", "country"), by.period = FALSE)
  )
})

test_that("Error: only one by.var allowed when using multiple ret.var", {
  load("test_data/backtest.function.test.RData")
  ## only one by.var allowed when using multiple ret.var
  expect_warning(
    backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = c("ret.var.1", "ret.var.2"), by.var = "country", by.period = FALSE)
  )
})

test_that("Error: Must provide minimum of one in.var and one ret.var", {
  load("test_data/backtest.function.test.RData")
  ## Must provide minimum of one in.var and one ret.var
  expect_error(
    backtest(x, ret.var = "ret.var.1", by.var = c("country", "sector"), id.var = c("id", "country"), by.period = FALSE)
  )

  expect_error(
    backtest(x, in.var = "in.var.1", by.var = c("country", "sector"), id.var = c("id", "country"), by.period = FALSE)
  )
})

test_that("Error: Only one date.var is allowed", {
  load("test_data/backtest.function.test.RData")
  ## Only one date.var is allowed
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = c("date", "id"), natural = TRUE, id = "id", buckets = 2)
  )
})

test_that("Error: Natural backtests must have dates and ids", {
  load("test_data/backtest.function.test.RData")
  ## Natural backtests must have dates and ids
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, natural = TRUE, id = "id", buckets = 2)
  )

  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", natural = TRUE, buckets = 2)
  )
})

test_that("Error: ret.var columns must be numeric", {
  load("test_data/backtest.function.test.RData")
  ## ret.var columns must be numeric
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "country", by.period = FALSE)
  )
})

test_that("Error: Use by.var or date.var, not both", {
  load("test_data/backtest.function.test.RData")
  ## Use by.var or date.var, not both
  expect_error(
    backtest(x, id.var = "id", in.var = "in.var.1", ret.var = "ret.var.1", by.var = "country", date.var = "date", by.period = FALSE)
  )
})

test_that("Error: Overlap option must include id.var", {
  load("test_data/backtest.function.test.RData")
  ## Overlap option must include id.var
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  )
})

test_that("Error: ", {
  load("test_data/backtest.function.test.RData")
  ## Overlaps must be less than the number of periods
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 30)
  )
})

test_that("Error: If overlaps > 1, can only use one ret.var", {
  load("test_data/backtest.function.test.RData")
  ## If overlaps > 1, can only use one ret.var
  expect_warning(expect_error(
    backtest(x, in.var = "in.var.1", ret.var = c("ret.var.1", "ret.var.2"), date.var = "date", id.var = "id", buckets = 3, natural = TRUE, by.period = T, overlaps = 2)
  ))
})

test_that("Error: The number of non-NA in.var must be at least 5
          more than the number of buckets", {
  load("test_data/backtest.function.test.RData")
  ## The number of non-NA in.var must be at least 5 more than the # of buckets.
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE, buckets = 200)
  )
})

test_that("Error: date.var required if by.period = TRUE", {
  load("test_data/backtest.function.test.RData")
  ## date.var required if by.period = TRUE
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", buckets = 2)
  )
})

test_that("Error: Not enough observations to fill each bucket by period", {
  load("test_data/backtest.function.test.RData")
  ## Not enough observations to fill each bucket by period.
  expect_error(
    backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", id.var = "id", natural = TRUE)
  )
})
