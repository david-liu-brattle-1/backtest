################################################################################
##
## $Id: backtest.totalCounts.test.R 1300 2008-08-27 21:01:11Z zhao $
##
## Test-case for the totalCounts method of the backtest object.
##
################################################################################

library(backtest)

# save(x, true.tc, file = "test_backtest_totalCounts.RData")
load("test_backtest_totalCounts.RData")

bt <- backtest(x, in.var = c("ret_12_0_m", "vim"), ret.var = "ret_0_1_y",
               by.period = FALSE, date.var = "date")

stopifnot(all.equal(totalCounts(bt), true.tc))
