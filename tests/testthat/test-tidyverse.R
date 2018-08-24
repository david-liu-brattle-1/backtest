################################################################################
##
## $Id: test-tidyverse.R
##
## Tests for function tidayverse compliance
##
################################################################################

context("Tidyverse")

test_that("group_by", {
  load("test_data/backtest.function.test.RData")
  bt.original <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)
  bt.mutated <- bt.original %>%
    group_by(sector, date)

  expect_equal(results(bt.mutated), results(bt.original))
})

test_that("group_by %>% summarise", {
  load("test_data/backtest.function.test.RData")
  bt.original <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)
  bt.mutated <- bt.original %>%
    group_by(sector, date) %>%
    summarise(in.var.1 = mean(in.var.1), ret.var.1 = mean(ret.var.1))

  bt.control <- x %>%
    group_by(sector, date) %>%
    summarise(in.var.1 = mean(in.var.1), ret.var.1 = mean(ret.var.1)) %>%
    backtest(in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)

  expect_equal(results(bt.mutated), results(bt.control))

  # Not Equal
  expect_error(expect_equal(results(bt.original), results(bt.control)))
})

test_that("mutate", {
  load("test_data/backtest.function.test.RData")
  bt.original <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)
  bt.mutated <- bt.original %>% mutate(ret.var.1 = ret.var.1^2)
  bt.control <- x %>% mutate(ret.var.1 = ret.var.1^2) %>% backtest(in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)

  expect_equal(results(bt.mutated), results(bt.control))

  # Not Equal
  expect_error(expect_equal(results(bt.original), results(bt.control)))
})

test_that("filter", {
  load("test_data/backtest.function.test.RData")
  bt <- backtest(x,
    in.var = c("in.var.1", "in.var.2"),
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  ) %>%
    filter(country == "USA")

  bt2 <- backtest(x %>% filter(country == "USA"),
    in.var = c("in.var.1", "in.var.2"),
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )

  expect_equal(results(bt), results(bt2))
  expect_equal(summary(bt), summary(bt2))

  expect_warning(bt3 <- backtest(x,
    in.var = c("in.var.1", "in.var.2"),
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE,
    universe = country == "USA"
  ))

  expect_equal(results(bt), results(bt3))
  expect_equal(summary(bt), summary(bt3))
})

test_that("Unquoted string arguments", {
  load("test_data/backtest.function.test.RData")
  bt <- backtest(x,
    in.var = c("in.var.1", "in.var.2"),
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )
  invar <- c("in.var.1", "in.var.2")
  bt2 <- backtest(x,
    in.var = invar,
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )

  expect_equal(results(bt), results(bt2))
  expect_equal(summary(bt), summary(bt2))

  bt3 <- backtest(x,
    in.var = c(in.var.1, in.var.2),
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )
  expect_equal(results(bt), results(bt3))
  expect_equal(summary(bt), summary(bt3))
})

test_that("Unquoted string arguments", {
  load("test_data/backtest.function.test.RData")
  bt <- backtest(x,
    in.var = "in.var.1",
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )
  invar <- c("in.var.1")
  bt2 <- backtest(x,
    in.var = invar,
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )
  expect_equal(results(bt), results(bt2))
  expect_equal(summary(bt), summary(bt2))

  bt3 <- backtest(x,
    in.var = c(in.var.1),
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )
  expect_equal(results(bt), results(bt3))
  expect_equal(summary(bt), summary(bt3))

  bt4 <- backtest(x,
    in.var = in.var.1,
    ret.var = ret.var.1, date.var = "date",
    id.var = "id", buckets = 2,
    natural = TRUE, by.period = FALSE
  )
  expect_equal(results(bt), results(bt4))
  expect_equal(summary(bt), summary(bt4))
})
