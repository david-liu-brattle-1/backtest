################################################################################
##
## $Id: backtest.plot.R
##
## Result object for a backtest
##
################################################################################

#' @describeIn backtest
#' Plots returns, cumulative returns, or turnover, when passed a
#'       \code{type} argument of \code{return}, \code{cumreturn}, or
#'       \code{turnover}, respectively.
#' @param x A backtest object
#' @param type A character equal to "return", "turnover", "cumreturn", or "cumreturn.split"
#' @param ... additional parameters
setMethod(
  "plot",
  signature(x = "backtest", y = "missing"),
  function(x, type = "return") {
    if (!x@natural) {
      stop("Can only plot natural backtests")
    }

    if (type == "return") {
      btplot <- .plot.return(x)
    }
    else if (type == "turnover") {
      btplot <- .plot.turnover(x)
    }
    else if (type == "cumreturn") {
      btplot <- .plot.cumreturn(x)
    }
    else if (type == "cumreturn.split") {
      btplot <- .plot.cumreturn.split(x)
    }
    else {
      stop("Unknown type specified.")
    }

    invisible(btplot)
  }
)

## Plots turnover for each period specified by "date.var"

.plot.turnover <- function(object, main = "Turnover",
                           xtext = "Date", ytext = "Turnover (%)") {
  turnovers <- turnover(object)[-1, ]

  turnovers <- 100 * as.data.frame(turnovers)

  ## Stop informatively if we can't call as.Date on what was the
  ## contents of the date.var column.

  if (inherits(try(as.Date(rownames(turnovers)), silent = TRUE), "try-error")) {
    stop(paste(
      "Error converting dates: Please ensure contents of date.var",
      "column cate be converted to Date's via as.Date."
    ))
  }
  invars <- names(turnovers)

  turnovers$date <- rownames(turnovers)
  turnovers <- gather(turnovers, control = invars, key = "Category")

  btplot <- ggplot(turnovers, aes_string(
    x = "date", y = "value",
    group = "Category", color = "Category"
  )) +
    geom_line() + geom_point() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "bottom"
    ) + xlab(xtext) + ylab(ytext) + ggtitle(main)

  print(btplot)

  invisible(btplot)
}

## Plots returns for each period specified by "date.var"

.plot.return <- function(object, main = "Spread Return",
                         xtext = "Date", ytext = "Return (%)") {
  spreads <- results(object)[1, , , dim(results(object))[4], 1] -
    results(object)[1, , , 1, 1]

  ## Fix for 1D array
  if (is.null(dim(spreads))) {
    spreads <- array(spreads,
      dim = c(1, length(spreads)),
      dimnames = list(
        object@in.var,
        names(spreads)
      )
    )
  }

  spreads <- 100 * data.frame(t(spreads))
  invars <- names(spreads)

  spreads$date <- rownames(spreads)
  spreads <- gather(spreads, control = invars, key = "Category")

  btplot <- ggplot(spreads, aes_string(x = "date", y = "value")) +
    geom_col(aes_string(fill = "Category"), position = "dodge") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "bottom"
    ) + xlab(xtext) + ylab(ytext) + ggtitle(main)
  print(btplot)

  invisible(btplot)
}

.plot.cumreturn <- function(object, main = "Backtest Fanplot", xtext = "Date",
                            ytext = "Cumulative Returns") {

  ## Function to compute cumsums of a matrix, by columns

  cum.matrix <- function(x) {
    for (i in 1:dim(x)[2]) {
      x[, i] <- cumsum(x[, i])
    }
    x
  }

  returns <- means(object)

  ## Calculate cumulative returns

  returns <- lapply(returns, cum.matrix)

  new.returns <- data.frame()

  for (i in 1:length(returns)) {
    this.returns <- data.frame(returns[[i]])

    ## We need quantile names that don't begin with a number so that
    ## the formula used with lattice graphics works.  But, the "Xn"
    ## naming scheme that is data.frame's default needs to get changed
    ## to "Qn".

    names(this.returns) <- gsub("X([1-9]+)", "Q\\1",
      names(this.returns),
      perl = TRUE
    )

    this.returns$group <- names(returns)[i]

    if (inherits(try(as.Date(rownames(this.returns)), silent = TRUE), "try-error")) {
      stop(paste(
        "Error converting dates: Please ensure contents of date.var",
        "column cate be converted to Date's via as.Date."
      ))
    }

    this.returns$date <- as.Date(rownames(this.returns))
    new.returns <- rbind(new.returns, this.returns)
  }

  colorder <- rev(setdiff(names(new.returns), c("group", "date")))
  new.returns <- gather(new.returns, control = setdiff(names(new.returns), c("group", "date")), key = "Category")
  new.returns$Category <- factor(new.returns$Category, levels = colorder)

  btplot <- ggplot(new.returns, aes_string(x = "date", y = "value", color = "Category")) +
    geom_line() + facet_wrap(facets = "group") + xlab(xtext) +
    ylab(ytext) + ggtitle(main)

  print(btplot)

  invisible(btplot)
}

.plot.cumreturn.split <- function(object,
                                  xtext = "Date",
                                  ytext = "Return (%)") {

  ## No notion of spread return without 2 or more buckets.

  stopifnot(isTRUE(object@buckets[1] >= 2))

  ## Data setup.  Calculate cumulative returns and spread returns
  ## before plotting.

  ## Function to compute the cumulative return of each quantile, and
  ## the spread cumulative return.  Since we're dealing with portfolio
  ## return and fixed theoretical assets, we take a cumulative sum
  ## without compounding.

  ## Also, convert return into percent.

  cumulate <- function(x) {
    x <- data.frame(x)

    for (i in 1:ncol(x)) {
      x[[i]] <- 100 * cumsum(x[[i]])
    }

    x$spread <- x[[ncol(x)]] - x[[1]]

    x
  }

  returns <- means(object)

  ## Calculate cumulative returns

  returns <- lapply(returns, cumulate)

  new.returns <- data.frame()

  for (i in 1:length(returns)) {
    this.returns <- returns[[i]]

    ## We need quantile names that don't begin with a number so that
    ## the formula used with lattice graphics works.  But, the "Xn"
    ## naming scheme that is data.frame's default needs to get changed
    ## to "Qn".

    names(this.returns) <- gsub("X([1-9]+)", "Q\\1",
      names(this.returns),
      perl = TRUE
    )

    this.returns$group <- names(returns)[i]

    if (inherits(try(as.Date(rownames(this.returns)), silent = TRUE), "try-error")) {
      stop(paste(
        "Error converting dates: Please ensure contents of date.var",
        "column cate be converted to Date's via as.Date."
      ))
    }

    this.returns$date <- as.Date(rownames(this.returns))
    new.returns <- rbind(new.returns, this.returns)
  }

  ## Plotting
  ## There are two regions to this plot, the top plot of spread return
  ## and the bottom, larger plot of cumulative quantile return.

  ## Calc period drawdown and greatest drawdown periods
  new.returns <- new.returns %>%
    group_by_("group") %>%
    mutate_(dd = "1 - (spread / 100 + 1) / cummax(spread / 100 + 1)") %>%
    mutate_(
      dd.max = "max(dd)",
      dd.end.idx = "max(which.max(dd), length(dd) - which.max(rev(dd)) + 1)",
      dd.start.idx = "max(which(dd == 0 & seq(dd) < dd.end.idx))",
      dd.start = "date[dd.start.idx]",
      dd.end = "date[dd.end.idx]",
      dd.ret = "spread[dd.end.idx] - spread[dd.start.idx]"
    )

  subreturns <- new.returns %>%
    filter_("date >= dd.start & date <= dd.end") %>%
    filter_("any(dd != 0)") %>%
    ungroup()

  y.max <- max(new.returns$spread)
  y.min <- min(new.returns$spread)
  y.dist <- y.max - y.min
  y.lim <- c(y.min - 0.05 * y.dist, y.max + 0.05 * y.dist)

  plotout.top <-
    ggplot(new.returns, aes_string(x = "date", y = "spread")) + geom_point() + geom_line() +
    geom_line(data = subreturns, aes_string(x = "date", y = "spread"), color = "red") + ylim(y.lim) +
    xlab("Date") + ylab("Spread") + ggtitle("Cumulative Spread Return") +
    facet_wrap("group")


  invars <- names(new.returns)[1:(length(names(new.returns)) - 10)]
  new.returns.long <- gather(new.returns[, c("date", invars, "group")],
    control = invars,
    key = "Quantile"
  )

  plotout.bot <- ggplot(new.returns.long, aes_string(x = "date", y = "value", color = "Quantile")) + geom_line() +
    facet_wrap("group") + xlab(xtext) + ylab(xtext) + theme(legend.position = "bottom") +
    ggtitle("Cumulative Quantile Return")
  btplot <- grid.arrange(plotout.top, plotout.bot, ncol = 1)
  print(btplot)
  invisible(btplot)
}
