###################################################
##
##
## Tests for function "calc.true.weight"
##
##################################################

test_that("calc.true.weight", {
  ## save(calc.data, calc.truth, file = "calc.true.weight.RData")
  load("test_data/calc.true.weight.test.RData")
  result <- backtest:::calc.true.weight(calc.data, "date", "id", 2)
  expect_equal(calc.truth, result$weight)
})
