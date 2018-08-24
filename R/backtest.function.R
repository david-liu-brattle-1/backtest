################################################################################
##
## $Id: backtest.function.R
##
## Returns an object of class backtest
##
################################################################################

#' Creating an Object of Class Backtest
#' @description Conducts a backtest and returns the results as an object of class
#'   \code{backtest}.
#' @param x A data frame containing the data to be analysed in the backtest.  The
#'   details of what this data frame must contain aregiven below.
#' @param in.var A character vector which indicates the name of the column or columns
#'   in \code{x} to be used as input variables.
#' @param ret.var A character vector which indicates the name of the column or columns
#'   in \code{x} to be used as return variables.
#' @param by.var An optional character value, specifying a second variable in \code{x}
#'   to be used for categorising the data.  The details of how categories are created are
#'   given below.
#' @param id.var An optional character value which indicates the name of  the column
#'   in \code{x} containing a unique identifier for each  observation.  \code{id.var}
#'   must be specified if \code{natural} is  TRUE.
#' @param date.var An optional character vector which indicates the name of the column
#'   in \code{x} to be used as a date for each observation. \code{date.var} must be specified
#'   if \code{natural} is TRUE.  In order to call \code{plot}, the contents of \code{date.var}
#'   must be of class \code{Date} or be coercible to an object of class \code{Date} via
#'   \code{as.Date}.
#' @param buckets An optional numeric vector which specifies how many quantiles to create
#'   according to \code{in.var} and \code{by.var}.
#' @param universe An optional expression or logical vector for selecting a subset of \code{x}. The details of
#'   how this expression may be constructed are given below.
#' @param natural An optional \code{"logical"} value.  If TRUE, the \code{summary} method
#'   returns additional information and the backtest object may be plotted.  The details of
#'   how a natural backtest differs from a pooled backtest are given below.
#' @param do.spread Object of class \code{"logical"}. If TRUE the \code{summary} method
#'   displays information about the spread between the extreme quantiles.  If FALSE this
#'   information is suppressed.  Defaults to TRUE.
#' @param by.period Object of class \code{"logical"}. If TRUE the quantiles are recalculated
#'   within each date period.  If FALSE the quantiles are calculated all at once.  Defaults to
#'   TRUE.
#' @param overlaps An object of class \code{"numeric"} which specifies the number of prior
#'   periods to include in the current period's portfolio weights calculation. If
#'   \code{overlaps} is the default of 1, backtest behaves as usual and only uses a periods
#'   own data to determine its portfolio.  If \code{overlaps} is set ton > 1, a period's
#'   portfolio comprises the weighted mean of portfolio weights from the previous n periods,
#'   with period n having a weight of 1/n.
#' @return \code{backtest} returns an object of class \code{backtest}
#' @details Data frames for \code{backtest} must, at a minimum, contain a column of class
#'   numeric to be referenced by the \code{in.var} and \code{ret.var} arguments.
#'   The \code{in.var} is the primary variable by which the backtest categorises
#'   observations.  It must reference a numeric column in \code{x}.  Using the
#'   values in \code{x}, \code{backtest} breaks the values into equal sized
#'   quantiles, or \code{buckets}.
#'
#'   The \code{by.var} is the secondary variable by which the backtest categorises
#'   observations.  When specifying both \code{in.var} and \code{by.var}, \code{backtest}
#'   organises the observations into a \code{n} by \code{j} matrix where \code{n} is the
#'   number of quantiles or categories created for the \code{by.var} and \code{j} is
#'   the number of quantiles created for the \code{in.var}.  By default,
#'   \code{backtest} creates 5 quantiles.
#'   If \code{natural} is TRUE, the data and arguments must meet certain
#'   requirements.  First, the frequency of the observations and \code{ret.var}
#'   must be the same.  Second, an \code{id.var} and \code{date.var} are
#'   required.  Third, a \code{by.var} is not allowed.  Note that the code
#'   does not verify that the backtest is truly natural; \code{backtest}
#'   accepts the value passed by the user as valid.
#'   \code{universe} is an optional variable which can either be a vector of \code{"logical"}
#'   values indicating the rows of \code{x} to include in the analysis or an expression to be
#'   evaluated in the namespace of \code{x}.  An example of the latter is
#'   \code{universe = country == "USA"} when \code{country} is a column of \code{x}.
#'
#'   The functions \code{show} and \code{summary} are used to obtain and
#'   print a short description and longer summary of the results of the
#'   \code{backtest}.  The accessor functions \code{counts}, \code{totalCounts},
#'   \code{marginals}, \code{means}, \code{naCounts}, and \code{turnover}
#'   extract different parts of the value returned by \code{backtest}.
#' @author Kyle Campbell \email{kyle.w.campbell@williams.edu} and Jeff Enos
#'   \email{jeff@kanecap.com}
#' @seealso \code{\link{backtest.update}}
#'
#' \code{\link{backtest-class}}
#' @examples
#' data(starmine)
#' 
#' ## Backtest with 1 'in.var' and 1 'ret.var'
#' 
#' bt <- backtest(starmine, in.var = smi, ret.var = ret.0.1.m, by.period = FALSE)
#' summary(bt)
#' 
#' ## Backtest with 2 'in.var' values, 1 'ret.var', and a 'by.var'
#' 
#' bt <- backtest(starmine,
#'   in.var = c(smi, cap.usd),
#'   ret.var = ret.0.1.m, by.var = sector, by.period = FALSE
#' )
#' summary(bt)
#' 
#' ## Backtest with 1 'in.var', 1 'by.var', and 1 'ret.var'.  Number of
#' ## buckets changed from default of 5 to 4.  Change in number of buckets
#' ## only affects the 'in.var' because the 'by.var' column in 'starmine'
#' ## contains character data. For each value in this column there is a
#' ## unique category.
#' 
#' bt <- backtest(starmine,
#'   in.var = smi, by.var = sector,
#'   ret.var = ret.0.1.m, buckets = 4, by.period = FALSE
#' )
#' summary(bt)
#' 
#' ## Backtest with 1 'in.var', multiple 'ret.var', and a
#' ## universe restriction
#' 
#' bt <- backtest(starmine,
#'   in.var = smi,
#'   ret.var = c(ret.0.1.m, ret.0.6.m),
#'   universe = sector == "HiTec", by.period = FALSE
#' )
#' summary(bt)
#' 
#' ## Multiple backtests can be run with incremental modifications through
#' ## the dplyr grammar and backtest.update.
#' 
#' bt <- backtest(starmine,
#'   in.var = smi,
#'   ret.var = c(ret.0.1.m, ret.0.6.m), by.period = FALSE
#' )
#' summary(bt)
#' 
#' bt2 <- bt %>%
#'   filter(sector == "HiTec")
#' summary(bt2)
#' 
#' bt3 <- bt %>%
#'   mutate(ret.0.1.m = ret.0.1.m^2)
#' summary(bt3)
#' 
#' bt4 <- backtest.update(bt, ret.var = "ret.0.6.m")
#' summary(bt4)
#' 
#' ## Running a natural backtest with 2 'in.vars', 1 'ret.var'
#' ## 10 buckets
#' 
#' bt <- backtest(starmine,
#'   in.var = c(smi, cap.usd),
#'   ret.var = ret.0.1.m, date.var = date,
#'   id.var = id, buckets = 10,
#'   natural = TRUE, by.period = FALSE
#' )
#' summary(bt)
#' 
#' ## The same backtest, but calculating quantiles within periods.
#' 
#' bt <- backtest(starmine,
#'   in.var = c(smi, cap.usd),
#'   ret.var = ret.0.1.m, date.var = date,
#'   id.var = id, buckets = 10,
#'   natural = TRUE, by.period = TRUE
#' )
#' summary(bt)
#' \dontrun{
#' plot(bt, type = "turnover")
#' plot(bt, type = "return")
#' plot(bt, type = "cumreturn")
#' plot(bt, type = "cumreturn.split")
#' }
backtest <- function(x,
                     in.var,
                     ret.var,
                     by.var = character(),
                     id.var = character(),
                     date.var = character(),
                     buckets = 5,
                     universe = NULL,
                     natural = FALSE,
                     do.spread = TRUE,
                     by.period = TRUE,
                     overlaps = 1) {
  args <- match.call()

  ## Replace any non-string arguments with their string equivalents
  for (callvar in c("in.var", "ret.var", "by.var", "date.var", "id.var")) {
    if (class(args[[callvar]]) == "name" && !as.character(args[[callvar]]) %in% names(x)) {

      # Called with backtest(x, in.var = invar) where invar = c('smi', 'date')
      args[[callvar]] <- eval.parent(args[[callvar]])
    } else if (class(args[[callvar]]) == "call" && as.character(args[[callvar]][[1]]) == "c") {

      # Called with backtest(x, in.var = c(smi, date))
      for (n in 2:length(args[[callvar]])) {
        if (class(args[[callvar]][[n]]) == "name") {
          stopifnot(as.character(args[[callvar]][[n]]) %in% names(x))
          args[[callvar]][[n]] <- as.character(args[[callvar]][[n]])
        }
      }
      assign(callvar, eval(args[[callvar]]))
    } else if (class(args[[callvar]]) == "name") {

      # Called with backtest(x, in.var = smi)
      stopifnot(as.character(args[[callvar]]) %in% names(x))
      assign(callvar, as.character(args[[callvar]]))
    }
  }

  data.original <- x

  ## Corner Case: only one in.var of class factor allowed per backtest

  if (length(in.var) > 1) {
    if (any(sapply(x[in.var], class) == "factor") ||
      any(sapply(x[in.var], class) == "character")) {
      stop("Only one in.var of class factor or character allowed.")
    }
  }

  ## Corner Case: only one by.var allowed

  if (length(by.var) > 1) {
    stop("Only one by.var allowed per backtest.")
  }

  ## Corner Case: only one id.var allowed

  if (length(id.var) > 1) {
    stop("Only one id.var allowed per backtest.")
  }

  ## Corner Case: only one by.var allowed when using multiple ret.var

  if (length(ret.var) > 1 && (length(by.var) > 0 || length(date.var) > 0)) {
    warning("Specifying by.var with multiple ret.vars is not supported. Proceed with caution.")
  }

  ## Must provide minimum of one in.var and one ret.var

  if (length(in.var) < 1 || length(ret.var) < 1) {
    stop("At least one in.var and ret.var required.")
  }

  ## Only one date.var is allowed

  if (length(date.var) > 1) {
    stop("Only one date.var is allowed.")
  }

  ## Natural backtests must have dates and ids.

  if (natural && (length(date.var) == 0 || length(id.var) == 0)) {
    stop("Must specify date.var and id.var for a natural backtest.")
  }

  ## ret.var columns must be numeric

  if (!all(sapply(x[ret.var], class) == "numeric")) {
    stop("All ret.var columns must be numeric")
  }

  ## Use by.var or date.var?

  if (length(date.var) > 0) {
    if (length(by.var) > 0) {
      stop("Cannot specify both by.var and date.var.")
    }
    else {
      by.or.date <- date.var
    }
  } else {
    by.or.date <- by.var
  }


  ## Check "buckets"

  if (length(by.or.date) == 0) {
    buckets[2] <- 1
  }
  else {
    if (length(buckets) == 1) {
      buckets[2] <- buckets[1]
    }
  }

  ## If overlaps is greater than 1, a date.var is required.

  if (overlaps > 1 && length(date.var) == 0) {
    stop("If overlaps is greater than 1, a date.var is required.")
  }

  ## If overlaps is greater than 0, in.var must be of length one and numeric.

  if (overlaps > 1 && (length(in.var) != 1 || !is.numeric(x[[in.var]]))) {
    stop("If overlaps is greater than 1, in.var must be of length one and numeric.")
  }

  ## Overlap option must include id.var

  if (overlaps > 1 && length(id.var) < 1) {
    stop("If overlaps is greater than 1, there must be an id.var.")
  }


  ## Overlaps must be less than the number of periods

  if (overlaps != 1 && overlaps > length(unique(x[[date.var]]))) {
    stop("Overlaps must be less than the number of periods.")
  }

  ## If overlaps > 1, can only use one ret.var

  if (overlaps > 1 && length(ret.var) > 1) {
    stop("The multiple overlap option can only accept one ret.var.")
  }

  ## At a minimum, the length of in.var must be greater than the
  ## number of buckets.  Otherwise, we will get an error when we try
  ## to create more quantiles then there are observations.

  if (any(sapply(x[in.var], function(x) {
    sum(!is.na(x))
  }) < (buckets[1] - 5))) {
    stop("The number of non-NA in.var must be at least 5 more than the number of buckets.")
  }

  ## The length of by.or.date must also be greater than the number of
  ## buckets to properly create quantiles

  if (length(by.or.date) > 0 && sum(!is.na(x[by.or.date])) < (buckets[2] - 5)) {
    stop("The number of non-NA by.or.date must be at least 5 more than the number of buckets.")
  }

  ## Check for by.period and date

  if (isTRUE(by.period) && length(date.var) == 0) {
    stop("date.var required if by.period = TRUE (the default)")
  }
  if (!missing(universe)) {
    univ <- eval(substitute(universe), data.original, parent.frame())
    univ <- univ & !is.na(univ)
    if (length(univ) > 0) {
      warning("universe argument is deprecated.  Use filter(bt) instead")
    }
  } else {
    univ <- logical()
  }
  if (by.period) {
    x <- x %>% group_by(get(date.var))
    for (i in in.var) {
      if (!is.factor(x[[i]])) {
        counts <- x %>% summarise(c = sum(!is.na(get(i))))
        if (any(counts$c < buckets[1]) | nrow(counts) < length(unique(x[[date.var]]))) {
          stop("Not enough observations to fill each bucket by period.")
        }
      }
    }
  }
  delayedAssign("in.factor", {
    in.factor <- data.frame(array(dim = c(nrow(data.original), length(in.var)), dimnames = list(NULL, in.var)))

    for (i in in.var) {

      ## If in.var is not a factor (is numeric)

      if (!is.factor(data.original[[i]])) {
        ## If we are doing by period, form buckets for every date

        if (by.period) {
          in.factor[[i]] <- as.factor(unsplit(
            lapply(
              split(data.original[[i]], data.original[[date.var]]),
              function(x) {
                categorize(x, n = buckets[1])
              }
            ),
            data.original[[date.var]]
          ))
        }

        ## If we are not doing by period bucketizing, form all buckets simultaneously

        else {
          in.factor[[i]] <- categorize(data.original[[i]], n = buckets[1])
        }


        ## Name levels, the lowest bucket is the short part of the
        ## portfolio, the highest bucket is the long part

        levels(in.factor[[i]])[1] <- "low"
        levels(in.factor[[i]])[buckets[1]] <- "high"

        ## Make sure there is data in every bucket

        if (length(levels(in.factor[[i]])) != buckets[1]) {
          stop(paste(
            "Encountered quantiles with no observations.  This can",
            "occur with very little data or very regular",
            "(usually synthesized) data."
          ))
        }

        ## Recalculate weights based on overlaps

        if (overlaps > 1) {
          levels(in.factor[[i]])[1] <- "low"
          levels(in.factor[[i]])[buckets[1]] <- "high"
          levels(in.factor[[i]])[2:(buckets[1] - 1)] <- "mid"
        }
      }


      ## If the in.var is a factor, just set in.factor to the in.var

      else {
        in.factor[[i]] <- x[[i]]
      }
    }

    if (!isTRUE(length(unique(sapply(
      in.factor,
      function(x) {
        length(levels(x))
      }
    ))) == 1)) {
      stop("All in.var's must have the same number of buckets.")
    }

    in.factor
    ## ret.var columns must be numeric
  })


  delayedAssign("x", {
    if (length(univ) > 0) {
      data.original <- data.original[univ, ]
    }
    ## data.original is only modified in local context
    data.original$weight <- 1

    if (overlaps > 1) {
      for (i in in.var) {

        ## If in.var is not a factor (is numeric)

        if (!is.factor(data.original[[i]])) {
          in.factor.col <- paste("in.factor.", i, sep = "")
          data.original[[in.factor.col]] <- in.factor[[i]]

          data.original <- overlaps.compute(data.original, in.factor.col, date.var, id.var, overlaps)
        }
      }
    }
    if (length(by.or.date) == 0) {
      data.original$by.factor <- rep(factor(1), times = nrow(data.original))
    }
    else {
      if (length(date.var) == 0) {
        data.original$by.factor <- categorize(data.original[[by.or.date]], n = buckets[2])
      }
      else {
        data.original$by.factor <- categorize(data.original[[by.or.date]], n = buckets[2], is.date = TRUE)
      }
    }

    ## Factor a character in.var
    if (length(in.var) == 1 && is.character(data.original[in.var])) {
      data.original[in.var] <- as.factor(data.original[in.var])
    }
    for (r in ret.var) {
      ## Trim most extreme .5% of ret.var values
      data.original[[r]] <- data.original[[r]] * data.original[["weight"]]
    }

    data.original
  })

  calculation.env <- new.env()
  delayedAssign("ret.stats", assign.env = calculation.env, value = {
    ret.stats <- array(
      dim = c(length(ret.var), 6),
      dimnames = list(ret.var, c("min", "max", "mean", "median", "sd", "NA"))
    )
    for (r in ret.var) {
      ret.stats[r, "min"] <- min(x[[r]], na.rm = TRUE)
      ret.stats[r, "max"] <- max(x[[r]], na.rm = TRUE)
      ret.stats[r, "mean"] <- mean(x[[r]], na.rm = TRUE)
      ret.stats[r, "median"] <- median(x[[r]], na.rm = TRUE)
      ret.stats[r, "sd"] <- sd(x[[r]], na.rm = TRUE)
      ret.stats[r, "NA"] <- sum(is.na(x[[r]]))
    }

    ret.stats
  })

  delayedAssign("turnover", assign.env = calculation.env, value = {
    if (natural) {
      turnover <- array(
        dim = c(length(levels(x$by.factor)), length(colnames(in.factor))),
        dimnames = list(levels(x$by.factor), colnames(in.factor))
      )
      for (r in ret.var) {
        for (i in colnames(in.factor)) {
          turnover[, i] <- calc.turnover(x[[id.var]],
            portfolio.factor = in.factor[[i]],
            date.factor = x$by.factor
          )
        }
      }
    } else {
      turnover <- array()
    }

    turnover
  })

  delayedAssign("results", assign.env = calculation.env, value = {
    na.count <- function(x) {
      return(sum(is.na(x)))
    }

    ## Creating names

    by.names <- levels(x$by.factor)

    ## Construct the empty array.  The dimensions are ordered as follows:
    ## 1: ret.var(s)
    ## 2: in.var(s)
    ## 3: by.or.date buckets
    ## 4: in.var buckets
    ## 5: means/counts/trim.means/NAs
    results <- array(
      dim = c(
        length(ret.var), length(colnames(in.factor)),
        length(levels(x$by.factor)), length(levels(in.factor[[1]])), 4
      ),
      dimnames = list(
        ret.var, colnames(in.factor), by.names,
        levels(in.factor[[1]]), c("means", "counts", "trim.means", "NAs")
      )
    )

    ## Construct ret.stats array

    for (r in ret.var) {

      ## Trim most extreme .5% of ret.var values

      trim.range <- quantile(x[[r]], c(0.0025, 0.9975), na.rm = TRUE)

      trim.x <- subset(x, trim.range[[1]] < x[[r]] &
        x[[r]] < trim.range[[2]])

      trim.in.factor <- subset(in.factor, trim.range[[1]] < x[[r]] &
        x[[r]] < trim.range[[2]])

      ## Store ret.stats


      ## Select in.var
      for (i in colnames(in.factor)) {

        ## Bucketize means

        results[r, i, , , "means"] <- bucketize(x[[r]],
          x.factor = in.factor[[i]],
          y.factor = x$by.factor,
          compute = weighted.mean, na.rm = TRUE
        )

        ## Bucketize counts

        results[r, i, , , "counts"] <- bucketize(x[[r]],
          x.factor = in.factor[[i]],
          y.factor = x$by.factor,
          compute = length
        )

        ## Bucketize trim.means

        results[r, i, , , "trim.means"] <- bucketize(trim.x[[r]],
          x.factor =
            trim.in.factor[[i]], y.factor =
            trim.x$by.factor, compute
          = weighted.mean, na.rm = TRUE
        )

        ## Bucketize NAs

        results[r, i, , , "NAs"] <- bucketize(x[[r]],
          x.factor = in.factor[[i]],
          y.factor = x$by.factor,
          compute = na.count
        )
      }
    }
    results
  })

  invisible(new("backtest",
    in.var = in.var, ret.var = ret.var, by.var = by.var, date.var = date.var, natural = natural, do.spread = do.spread, by.period = by.period, buckets = buckets[1], overlaps = overlaps, id.var = id.var,
    calculated.data = calculation.env, data.original = data.original, universe = univ
  ))
}
