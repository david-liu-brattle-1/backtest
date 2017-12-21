## Test backtest functions by comparing a) results from backtest functions and 
## b) results calculated manually in the way that bachtest functions are
## expected to behave.
library(data.table)
library(dplyr)
library(tidyr)

# A data frame to apply backtest function on
# Quantile is manually assinged by arranging 'in.var.1' ascendingly and
# assigning quantile from 1 - 5
test.df <- data.frame(in.var.1 = runif(20, min = -10, max = 10),
                      in.var.2 = runif(20, min = -2, max = 2),
                      ret.var = runif(20, min = -1, max = 1),
                      country = sample(c("USA", "HKG", "ESP", "JPN"), size = 20, replace = TRUE),
                      date = as.Date(rep(c("2008-3-31", "2008-4-30"),10)),
                      id = c(1:20)) %>%
  arrange(in.var.1)


context("test backtest functions")
###########################################test1#####################################
## Test: backtest(df, in.var, ret.var, by.period = FALSE)
# which sort and bucketize rows by 'in.var' and average the values of
# 'ret.var' in each bucket.
test_that("backtest: test simple backtest())", {
  
  # Expected result: index of quantile is hard-coded
  expected_result1 <- test.df %>% arrange(in.var.1) %>%
    mutate(quantile = sapply(c(1:5), rep, 4) %>% as.numeric()) %>%
    group_by(quantile) %>%
    summarize(val = mean(ret.var)) %>% t()
  
  # Actual result from backtest
  actual_result1 <- backtest(test.df, in.var = "in.var.1", 
                             ret.var = "ret.var", by.period = FALSE)
  
  # Expectation should be equal to actaul result
  expect_equal(as.numeric(expected_result1[2,]), 
               as.numeric(actual_result1@results[1,1 , , ,"means"]),
               tolerance = 1e-4)
})

###########################################test2###############################
# Test: backtest(df, in.var, ret.var, by.var, by.period = FALSE) which is a 
# normal backtest but it does further by breaking the quantiles into rows 
# based on "by.var"
test_that("backtest: test 'by.var'", {
  
  # Expected result: index of quantile is hard-coded
  expected_result2 <- test.df %>% mutate(quantile = sapply(c(1,2,3,4,5), 
                                                           rep, 4) %>% 
                                           as.numeric()) %>%
    group_by(quantile, country) %>%
    summarise(val = mean(ret.var)) %>%
    tidyr::spread(key = quantile, value = val) %>%
    arrange(country) %>%
    as.matrix
  
  # Actual result from backtest
  actual_result2 <- backtest(test.df, in.var = "in.var.1", 
                             ret.var = "ret.var", 
                             by.var = "country", 
                             by.period = FALSE)
  
  ## suppress too small difference
  expect_equal(as.numeric(expected_result2[,-1]), 
               as.numeric(actual_result2@results[1,1 , , ,"means"]),
               tolerance = 1e-4)
})


######################################## test 3 ###############################
#test: backtest(x, id.var = "id", in.var = "in.var.1", ret.var = "ret.var.1", 
# date.var = "date", by.period = FALSE) which is to compute 5 quantiles over 
# all observations. Then average obs in each quantile with specific date
test_that("backtest: test date.var", {
  
  expected_result3 <- test.df %>% mutate(quantile = sapply(c(1,2,3,4,5), 
                                                           rep, 4) %>% 
                                           as.numeric()) %>%
    group_by(quantile, date) %>%
    summarise(val = mean(ret.var)) %>%
    tidyr::spread(key = quantile, value = val) %>%
    as.matrix()
  
  actual_result3 <- backtest(test.df, id.var = "id", in.var = "in.var.1", 
                             ret.var = "ret.var", date.var = "date", 
                             by.period = FALSE)
  
  ## suppress too small difference
  expect_equal(as.numeric(expected_result3[,-1]), 
               as.numeric(actual_result3@results[1,1 , , ,"means"]),
               tolerance = 1e-4)
})

##################################### test 4 ##################################
# Test: backtest(test.df, in.var = c("in.var.1", "in.var.2"), 
# ret.var = "ret.var", by.var = "by.var", by.period = FALSE) which returns 
# spreads of 2 backtest objects. Each of which is a normal backtest with
# each in.var. This test only makes sure that the results from the functino
# is the same as calling notmal backtest twice with difference in.var
test_that("backtest: test 2 'in.var's", {
  
  ############## in.var.1 ####################
  expected_result4_invar1 <- test.df %>% 
    mutate(quantile = sapply(c(1,2,3,4,5), rep, 4) %>% as.numeric()) %>%
    group_by(quantile, country) %>%
    summarise(val = mean(ret.var)) %>%
    tidyr::spread(key = quantile, value = val) %>%
    arrange(country) %>%
    as.matrix()
  
  actual_result4_invar1 <- backtest(test.df, in.var = c("in.var.1", "in.var.2"), 
                                    ret.var = "ret.var", by.var = "country", 
                                    by.period = FALSE)
  
  ##suppress too small difference
  expect_equal(as.numeric(expected_result4_invar1[,-1]), 
               as.numeric(actual_result4_invar1@results["ret.var","in.var.1", , ,"means"]),
               tolerance = 1e-4)
  
  ############## in.var.2 ####################
  expected_result4_invar2 <- test.df %>% 
    arrange(in.var.2) %>%
    mutate(quantile = sapply(c(1,2,3,4,5), rep, 4) %>% as.numeric()) %>%
    group_by(quantile, country) %>%
    summarise(val = mean(ret.var)) %>%
    tidyr::spread(key = quantile, value = val) %>%
    arrange(country) %>%
    as.matrix()
  
  actual_result4_invar2 <- backtest(test.df, in.var = c("in.var.1", "in.var.2"), 
                                    ret.var = "ret.var", by.var = "country", 
                                    by.period = FALSE)
  
  ##suppress too small difference
  expect_equal(as.numeric(expected_result4_invar2[,-1]), 
               as.numeric(actual_result4_invar2@results["ret.var","in.var.2", , ,"means"]),
               tolerance = 1e-4)
})


##################################### test 5 ##################################
## Test: backtest(test.df, in.var = "in.var.1", ret.var = "ret.var",
## by.period = TRUE, date.var = "date", buckets = 2) which calculating
## quantile after seperating by date already
test_that("backtest: by.period works properly", {
  
  #expect
  expected_result5 <- test.df %>% 
    ungroup() %>% group_by(date) %>% 
    arrange(in.var.1, .by_group = TRUE) %>% 
    mutate(quantile = sapply(c(1,2), rep, 5)) %>%
    group_by(date, quantile) %>% 
    summarise(val = mean(ret.var)) %>%
    tidyr::spread(key = quantile, value = val) %>%
    arrange(date) %>%
    as.matrix()
  
  actual_result5 <- backtest(test.df, in.var = "in.var.1", 
                             ret.var = "ret.var", by.period = TRUE, 
                             date.var = "date", buckets = 2)

  ##suppress too small difference
  expect_equal(as.numeric(expected_result5[,-1]), 
               as.numeric(actual_result5@results["ret.var","in.var.1", , ,"means"]),
               tolerance = 1e-4)
})


##################################### test 6 ##################################
## Test: backtest(test.df, in.var = "in.var.1", ret.var = "ret.var",
## by.period = TRUE,  date.var = "date", natural = TRUE, id = "id", 
## buckets = 2) which is to recalculate recalculate quantile every [month]
## where by.period = get mean in that period that falls in each quantile
test_that("backtest: Natural = TRUE", {
  
  # expect
  expected_result6 <- test.df %>% 
    ungroup() %>% group_by(date) %>% 
    arrange(in.var.1, .by_group = TRUE) %>% 
    mutate(quantile = sapply(c(1,2), rep, 5)) %>%
    group_by(date, quantile) %>% 
    summarise(val = mean(ret.var)) %>%
    tidyr::spread(key = quantile, value = val) %>%
    arrange(date) %>%
    as.matrix()
  
  actual_result6 <- backtest(test.df, in.var = "in.var.1", 
                             ret.var = "ret.var", by.period = TRUE, 
                             date.var = "date", natural = TRUE, 
                             id = "id", buckets = 2)
  
  ##how to ignore the too small difference
  expect_equal(expected_result6[,-1] %>% as.numeric(), actual_result6@results[1,1 , , ,"means"] %>% as.numeric(),
               tolerance = 1e-4)
  
  
  ### 5 and 6 are return the exact same result. They are exact same thing
})

#bt.7 <- backtest(test.df, id.var = "id", in.var = "in.var.1", ret.var = "ret.var", by.period = FALSE, date.var = "date", overlaps = 2)

