###################################################
##
##
## Tests for function "overlaps.compute"
##
##################################################

library(backtest)

load("test_overlaps_compute.RData")

## save(over.data, weight.truth, file = "test_overlaps_compute.RData")


result <- backtest:::overlaps.compute(over.data, "in.factor", "date", "id", 2)
weight.result <- result$weight


stopifnot(
          isTRUE(all.equal(weight.truth, weight.result))
          )


