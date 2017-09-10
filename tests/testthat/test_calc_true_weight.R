###################################################
##
##
## Tests for function "calc.true.weight"
##
##################################################

library(backtest)

load("test_calc_true_weight.RData")

## save(calc.data, calc.truth, file = "test_calc_true_weight.RData")


result <- backtest:::calc.true.weight(calc.data, "date", "id", 2)
true.weight.result <- result$weight


stopifnot(
          isTRUE(all.equal(true.weight.result, calc.truth))
          )
             
