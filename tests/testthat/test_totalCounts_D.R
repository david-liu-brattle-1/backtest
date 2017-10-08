context("test totalCount in backtest package")

library(dplyr)

test_bt_object <- data.frame(date = as.Date(c(rep("2000-1-31", 50), rep("2000-2-29", 50)))) %>%
  mutate(smi = sample(1:100, 100, replace = TRUE)) %>%
  mutate(in.var.2 = runif(100, min = -1, max = 1)) %>%
  mutate(ret.0.1.m = runif(100, min = -10, max = 10)) %>%
  backtest(in.var = c("smi", "in.var.2"), ret.var = "ret.0.1.m", by.period = FALSE, date.var = "date")

test_that("totalCount returns the rigth results", {
  
  expect <- as.data.frame(lapply(X = counts(test_bt_object), FUN = rowSums))
  result <- totalCounts(test_bt_object)
  
  expect_identical(expect, result)
})


# # low.high.only in totalCount is not working. FIx the code??
# test_that("low.high.only option works fine",{
#   
#   # expect result
#   expect <- lapply(X = counts(test_bt_object), FUN = function(x){
#     x <- as.matrix(x[, c("low", "high")])
#     rowSums(x)
#   })
#   
#   # result form function
#   result <- totalCounts(test_bt_object, low.high.only = TRUE)
#   
#   # expect both results to be identical
#   expect_identical(expect, result)
#   
# })