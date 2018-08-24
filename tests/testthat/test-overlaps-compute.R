###################################################
##
##
## Tests for function "overlaps.compute"
##
##################################################


context("overlaps")


test_that("overlaps.compute", {
  ## save(over.data, weight.truth, file = "overlaps.compute.test.RData")
  load("test_data/overlaps.compute.test.RData")

  result <- backtest:::overlaps.compute(over.data, "in.factor", "date", "id", 2)
  expect_equal(weight.truth, result$weight)
})
