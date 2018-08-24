################################################################################
##
## $Id: backtest.R 1622 2010-02-18 17:50:34Z enos $
##
## Result object for a backtest
##
################################################################################


#' @title backtest class
#' @aliases summaryStats naCounts totalCounts ci
#'   counts marginals means turnover
#' @description Contains results from the backtest function.
#' @slot in.var Object of class \code{"character"} specifying
#'   the \code{in.var} values for this backtest.
#' @slot ret.var Object of class \code{"character"} containing
#'   the \code{ret.var} values for this backtest.
#' @slot by.var Object of class \code{"character"} containing
#'   the \code{by.var}, if specified, for this backtest.
#' @slot date.var Object of class \code{"character"} containing
#'   the \code{date.var}, if specified, for this backtest.
#' @slot buckets Object of class \code{"numeric"} containing
#'   the number(s) of buckets used create quantiles from the
#'   \code{in.var} and \code{by.var} values.
#' @slot natural Object of class \code{"logical"} expressing
#'   whether or not the intervals between observations, as specified by
#'   \code{date.var}, and returns, as specified by \code{ret.var},
#'   match.  If the interval between dates is one month, the interval
#'   between returns should also be one month.
#' @slot do.spread Object of class \code{"logical"}. If TRUE the \code{summary}
#'   method displays information about the spread between the extreme
#'   quantiles.  If FALSE this information is suppressed.  Defaults to TRUE.
#' @slot by.period Object of class \code{"logical"}. If TRUE the quantiles are
#'   recalculated within each date period.  If FALSE the quantiles are
#'   calculated all at once.  Defaults to TRUE.
#' @slot overlaps An object of class \code{"numeric"} which specifies
#'   the number of prior periods to include in the current period's
#'   portfolio weights calculation. If \code{overlaps} is the default of 1,
#'   backtest behaves as usual and only uses a periods own data to
#'   determine its portfolio.  If \code{overlaps} is set to
#'   n > 1, a period's portfolio comprises the weighted mean of
#'   portfolio weights from the previous n periods, with period n
#'   having a weight of 1/n.
#' @slot id.var Object of class \code{"character"} specifying
#'   the \code{id.var} value for this backtest.
#' @slot universe Object of class \code{"logical"} with length equal to the input data, with values TRUE if the corresponding row is included in the analysis and FALSE otherwise.
#' @slot data.original Object of class \code{"data.frame"} containing the original input data.
#' @slot calculated.data Object of class \code{"environment"} containing the delayed assignment promises for the "results", "turnover" and "ret.stats" variables.
#'
#' @details
#'   The primary method for accessing the \code{backtest} results is through
#'   the \code{summary} method.  \code{summary} provides different displays
#'   depending on the type of \code{backtest} object.  These displays are
#'   shown in the examples section.  Accessor methods such as \code{means},
#'   \code{counts}, \code{marginals}, \code{naCounts}, \code{turnover}, and
#'   \code{ci} may be used to extract other types of information from the object.
#'
#'   A \code{backtest} object with a \code{natural} value of TRUE may be
#'   graphed by calling the \code{plot} method.  The default \code{plot}
#'   method graphs return.  The other plots, turnover and
#'   cumulative return, must be explicitly specified as \code{plot(object,
#'     type = "turnover")} or \code{plot(object, type = "cumreturn")}.
#'
#'   The \code{backtest} object does not store the data frame used to create
#'   the \code{backtest.}  It only stores the results and the names of the
#'   vectors used in calculating these results.
#'
#'   The results of a \code{backtest} are stored in a 5-dimensional array,
#'   \code{results}.  The 1st dimension contains one value for every element
#'   of \code{ret.var}.  The 2nd dimension contains one value for
#'   every element of \code{in.var}.  The 3rd dimension contains one value
#'   for every element in \code{1:buckets[1]}, a vector from 1 through the
#'   number of \code{by.var} buckets.  The 4th dimension contains one value
#'   for every element in \code{1:buckets[2]}, a vector from 1 through the
#'   number of \code{in.var} buckets.  The 5th dimension contains 4
#'   elements: \code{means}, \code{counts}, \code{trim.means}, and
#'   \code{NAs}.
#'
#' @seealso \code{\link{backtest}}
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' 
#' ## Summary for a pooled backtest
#' 
#' summary(bt)
#' 
#' ## A natural backtest
#' 
#' bt <- backtest(starmine,
#'   in.var = "smi", ret.var = "ret.0.1.m",
#'   date.var = "date", id.var = "id", natural = TRUE, by.period = FALSE
#' )
#' 
#' ## Summary for a natural backtest
#' 
#' summary(bt)
#' 
#' ## Other access methods
#' 
#' means(bt)
#' counts(bt)
#' marginals(bt)
#' naCounts(bt)
#' 
#' ## Plotting methods
#' \dontrun{
#' plot(bt, type = "turnover")
#' plot(bt, type = "return")
#' plot(bt, type = "cumreturn")
#' plot(bt, type = "cumreturn.split")
#' }
setClass("backtest",
  slots = c(
    in.var = "character",
    ret.var = "character",
    by.var = "character",
    date.var = "character",
    buckets = "numeric",
    natural = "logical",
    do.spread = "logical",
    by.period = "logical",
    overlaps = "numeric",
    id.var = "character",
    calculated.data = "environment",
    universe = "logical",
    data.original = "data.frame"
  ),

  validity = function(object) {
    if (length(object@in.var) == 0) {
      return("in.var not specified")
    }

    if (length(object@ret.var) == 0) {
      return("ret.var not specified")
    }

    if (length(object@by.var) > 0 && length(object@date.var) > 0) {
      return("Both by.var and date.var specified")
    }

    return(TRUE)
  }
)

setMethod(
  "initialize", "backtest",
  function(.Object, ...) {
    .Object <- callNextMethod()
    .Object
  }
)

setMethod(
  "results", signature(object = "backtest"),
  function(object) {
    object@calculated.data$results
  }
)

setMethod(
  "ret.stats", signature(object = "backtest"),
  function(object) {
    object@calculated.data$ret.stats
  }
)


setMethod(
  "show",
  signature(object = "backtest"),
  function(object) {
    cat("An object of class backtest:\nin.vars: ",
      paste(object@in.var, collapse = ", "), " (buckets: ",
      object@buckets[1], ")\n",
      sep = ""
    )

    if (length(object@by.var) > 0) {
      cat("by.var: ", object@by.var, " (buckets: ",
        object@buckets[2], ")\n",
        sep = ""
      )
    }

    if (length(object@date.var) > 0) {
      cat("date.var: ", object@date.var, " (buckets: ",
        object@buckets[2], ")\n",
        sep = ""
      )
    }

    cat("ret.vars: ", paste(object@ret.var, collapse = ", "),
      "\n",
      sep = ""
    )
  }
)

#' @describeIn backtest
#' Prints a header listing the .vars, then prints the table returned
#' by the "spreads" method.  Returns a list of result tables
#' @param object A backtest object
setMethod(
  "summary",
  signature(object = "backtest"),
  function(object) {

    ## Header
    out <- list()

    out[["summary"]] <- paste("Backtest conducted with:\n\n",
      length(object@in.var), ifelse(length(object@in.var) == 1,
        " in.var: ", " in.vars: "
      ),
      paste(object@in.var, collapse = ", "), ";\n",
      length(object@ret.var), ifelse(length(object@ret.var) == 1,
        " ret.var: ", " ret.vars: "
      ),
      paste(object@ret.var, collapse = ", "), ";\nand ",
      ifelse(length(object@by.var) > 0,
        paste("by.var: ", object@by.var, sep = ""), "no by.var"
      ), ";\n",
      ifelse(isTRUE(object@do.spread),
        "do.spread: TRUE", "do.spread: FALSE"
      ), ";\n",
      ifelse(isTRUE(object@by.period),
        "by.period: TRUE", "by.period: FALSE"
      ),
      ".\n\n",
      sep = ""
    )
    cat(out[["summary"]])

    ## Matrix

    out[["returns"]] <- summaryStats(object)[, !names(summaryStats(object))
    %in% c(
        "CI(low)",
        "CI(high)",
        "TURNOVER"
      )]
    print(out[["returns"]])
    cat("\n")

    if (isTRUE(object@do.spread)) {
      if (object@natural) {
        if (length(object@in.var) == 1) {
          out[["secondarystats"]] <- paste("average turnover: ", .bt.mean(turnover(object)),
            "\nmean spread: ", mean(summaryStats(object)[, "spread"]),
            "\nsd spread: ", sd(summaryStats(object)[, "spread"]),
            "\nraw sharpe ratio: ", .bt.sharpe(object), "\n\n",
            sep = ""
          )
          cat(out[["secondarystats"]])
        }
        else {
          out[["secondarystats"]] <- list()
          for (i in object@in.var) {
            x <- summaryStats(object)
            out[["secondarystats"]][[i]] <- paste("summary stats for in.var = ", i,
              ":\n\naverage turnover: ",
              .bt.mean(turnover(object))[1, i],
              "\nmean spread: ",
              colMeans(summaryStats(object), na.rm = TRUE)[i],
              "\nsd spread: ", sd(x[, i], na.rm = TRUE),
              "\nraw sharpe ratio: ", .bt.sharpe(object)[, i],
              "\n\n",
              sep = ""
            )
            cat(out[["secondarystats"]][[i]])
          }
        }
      }
    }
    invisible(out)
  }
)



#' @describeIn backtest
#' Returns a data frame summarizing the results of the backtest.  The
#' entries of the data frame contain means in cases 1, 2, and 4, and
#' spreads in cases 3 and 5.  When a table of means is returned, the
#' ".bt.spread" function is called and summary spread data is attached
#' to the right side of the data frame.  If a date.var is used,
#' ".bt.mean" is called and mean summary data are attached to bottom
#' of the data frame.
setMethod(
  "summaryStats",
  signature(object = "backtest"),
  function(object) {
    num.in <- length(object@in.var)
    num.ret <- length(object@ret.var)

    if (length(object@by.var > 0)) {
      by.present <- TRUE
      date.present <- FALSE
    }
    else {
      by.present <- FALSE
      if (length(object@date.var > 0)) {
        date.present <- TRUE
      }
      else {
        date.present <- FALSE
      }
    }

    ## Case 1: 1 in.var, 1 ret.var

    if (num.in == 1 && num.ret == 1) {
      output <- results(object)[1, 1, , , "means"]

      n <- results(object)[1, 1, , , "counts"]

      ## When one dimension of a 2D matrix has length 1,
      ## the subsets above return vectors.

      if (is.null(dim(output))) {
        output <- array(output,
          dim = c(1, length(output)),
          dimnames = list("pooled", names(output))
        )

        n <- array(n,
          dim = dim(output),
          dimnames = dimnames(output)
        )
      }
      if (isTRUE(object@do.spread)) {
        spread <- .bt.spread(output, n, ret.stats(object)[1, "sd"])
        output <- cbind(output, spread)
      }
      if (object@natural) {
        turnover <- turnover(object)
        dimnames(turnover)[[2]] <- "TURNOVER"
        output <- cbind(output, turnover)

        output <- rbind(output, .bt.mean(output))
      }
    }

    ## Case 2: multiple in.vars, no by.var

    if (num.in > 1 && num.ret == 1 && !by.present &&
      !date.present) {
      output <- results(object)[1, , 1, , "means"]

      n <- results(object)[1, , 1, , "counts"]

      spread <- .bt.spread(output, n, ret.stats(object)[1, "sd"])

      output <- cbind(output, spread)
    }

    ## Case 3: multiple in.vars, with by.var or date.var

    if (num.in > 1 && num.ret == 1 && (by.present || date.present)) {
      output <- t(results(object)[1, , , dim(results(object))[4], 1] -
        results(object)[1, , , 1, 1])



      if (date.present) {
        output <- rbind(output, .bt.mean(output))
      }
    }

    ## Case 4: single in.var, multiple ret.vars

    if (num.in == 1 && num.ret > 1) {
      output <- results(object)[, 1, 1, , "means"]

      n <- results(object)[, 1, 1, , "counts"]
      if (isTRUE(object@do.spread)) {
        spread <- .bt.spread(output, n, ret.stats(object)[, "sd"])

        output <- cbind(output, spread)
      }
    }

    ## Case 5: multiple in.vars and ret.vars

    if (num.in > 1 && num.ret > 1) {
      output <- results(object)[, , 1, dim(results(object))[4], 1] -
        results(object)[, , 1, 1, 1]
    }

    x <- data.frame(output)
    names(x) <- dimnames(output)[[2]]
    x
  }
)


#' @describeIn backtest
#' Returns a list of matrices, with one matrix for each in.var,
#' containing the means data.
setMethod(
  "means",
  signature(object = "backtest"),
  function(object) {
    mean.list <- list()

    for (i in object@in.var) {
      mean.list <- append(
        mean.list,
        list(results(object)[, i, , , "means"])
      )
    }

    names(mean.list) <- object@in.var

    mean.list
  }
)


#' @describeIn backtest
#' Returns a list of matrices, one matrix for each in.var, containing
#' the counts data
setMethod(
  "counts",
  signature(object = "backtest"),
  function(object) {
    count.list <- list()

    for (i in object@in.var) {
      count.list <- append(
        count.list,
        list(results(object)[1, i, , , "counts"])
      )
    }

    names(count.list) <- object@in.var

    count.list
  }
)



#' @describeIn backtest
#' This function computes the counts of non-NA values that went into
#' the calculation of spreads displayed by backtests's summary()
#' function. It is different from counts because it displays the sum
#' of counts from all buckets (or lowest and highest only), thus
#' allowing for output that matches the format of spreads output.
#' @param low.high.only A logical value indicating whether to omit
#'   all but the highest and lowest buckets.
setMethod(
  "totalCounts",
  signature(object = "backtest"),
  function(object, low.high.only = FALSE) {
    counts <- data.frame(do.call(
      "cbind",
      lapply(counts(object), function(x) {
        if (isTRUE(low.high.only)) {
          x <- x[c("low", "high")]
        }
        rowSums(x)
      })
    ))

    counts
  }
)

#' @describeIn backtest
#' Same as the "counts" method, except that the total counts for each
#' row and column are added to the margins.
setMethod(
  "marginals",
  signature(object = "backtest"),
  function(object) {
    body <- counts(object)

    for (i in 1:length(body)) {
      if (is.null(dim(body[[i]]))) {
        total <- sum(body[[i]], na.rm = TRUE)
        body[[i]] <- append(body[[i]], total)
        names(body[[i]])[length(body[[i]])] <- "TOTAL"
      }
      else {
        total <- rowSums(body[[i]], na.rm = TRUE)
        body[[i]] <- cbind(body[[i]], TOTAL = total)

        total <- colSums(body[[i]], na.rm = TRUE)
        body[[i]] <- rbind(body[[i]], TOTAL = total)
      }
    }

    body
  }
)


#' @describeIn backtest
#' Returns a list of matrices, one matrix for each in.var, containing
#' the NAs data
setMethod(
  "naCounts",
  signature(object = "backtest"),
  function(object) {
    na.list <- list()

    for (i in object@in.var) {
      na.list <- append(na.list, list(results(object)[, i, , , "NAs"]))
    }

    names(na.list) <- object@in.var

    na.list
  }
)

## Accessor Methods



#' @describeIn backtest
#' Returns the turnover for natural portfolios.  Passing a "mean"
#' argument will append the mean of the turnover(s) as the last row of
#' the matrix
#' @param mean A logical value indicating whether mean turnover should be appended
setMethod(
  "turnover",
  signature(object = "backtest"),
  function(object, mean = FALSE) {
    if (!isTRUE(object@natural)) {
      stop("Cannot calculate turnover if not a natural backtest.")
    }

    if (isTRUE(mean)) {
      return(rbind(object@calculated.data$turnover, .bt.mean(turnover(object))))
    }

    object@calculated.data$turnover
  }
)


#' @describeIn backtest Returns the confidence intervals for each spread for all cases
#'   except: (multiple in.var & any by.var) or (multiple in.var & multiple
#'   ret.var).  Must vectorize or loop to get these.
setMethod(
  "ci",
  signature(object = "backtest"),
  function(object) {
    if ((length(object@in.var) == 1 && length(object@ret.var) == 1)
    || (length(object@in.var) == 1 && length(object@ret.var > 1))) {
      summaryStats(object)[, c("CI(low)", "spread", "CI(high)")]
    }
  }
)

#' Update existing backtest objects with new parameters
#' @description Takes an existing backtest object and applies new parameters or data.
#' @param bt A backtest object to act as the template
#' @param x An optional \code{data.frame} to replace the data used in \code{bt}
#' @param ... Any number of arguments that are accepted by the backtest function.
#'   Any arguments in this list override the parameters of \code{bt}
#' @return Returns an object of class \code{backtest}
#' @export
#' @examples
#' data(starmine)
#' bt <- backtest(starmine,
#'   in.var = smi,
#'   ret.var = c(ret.0.1.m, ret.0.6.m), by.period = FALSE
#' )
#' summary(bt)
#' 
#' bt2 <- backtest.update(bt, ret.var = ret.0.6.m)
#' summary(bt2)
#' ## bt2 is identical to creating backtest(starmine, in.var = smi,
#' ##   ret.var = ret.0.6.m, by.period = FALSE)
#' 
#' bt3 <- backtest.update(bt2, by.period = TRUE, date.var = date)
#' summary(bt3)
backtest.update <- function(bt, x, ...) {
  if (missing(x)) {
    x <- bt@data.original
  }
  defaultArgs <- list(x = x, in.var = bt@in.var, ret.var = bt@ret.var, by.var = bt@by.var, date.var = bt@date.var, id.var = bt@id.var, buckets = bt@buckets, natural = bt@natural, do.spread = bt@do.spread, by.period = bt@by.period, overlaps = bt@overlaps, universe = bt@universe)

  args <- match.call()

  for (i in setdiff(names(args), c("bt", "x"))) {
    defaultArgs[[i]] <- args[[i]]
  }

  return(do.call(backtest, defaultArgs))
}

filter.backtest <- function(.data, ...) {
  return(backtest.update(.data, filter(.data@data.original, ...)))
}

mutate.backtest <- function(.data, ...) {
  return(backtest.update(.data, mutate(.data@data.original, ...)))
}

select.backtest <- function(.data, ...) {
  return(backtest.update(select(.data, .data@data.original, ...)))
}

summarise.backtest <- function(.data, ...) {
  return(backtest.update(.data, summarise(.data@data.original, ...)))
}

arrange.backtest <- function(.data, ...) {
  return(backtest.update(.data, arrange(.data@data.original, ...)))
}

group_by.backtest <- function(.data, ...) {
  return(backtest.update(.data, group_by(.data@data.original, ...)))
}
