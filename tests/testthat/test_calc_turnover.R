################################################################################
##
## $Id: calc.turnover.test.R 1300 2008-08-27 21:01:11Z zhao $
##
## Tests for function "calc.turnover"
##
################################################################################

library(backtest)

load("test_calc_turnover.RData")

## save(x.id, x.bucket, x.date, x.truth, file = "test_calc_turnover.RData", compress = TRUE)

x.result <- backtest:::calc.turnover(x.id, x.bucket, x.date)

stopifnot(
          isTRUE(all.equal(x.result, x.truth))
          )
