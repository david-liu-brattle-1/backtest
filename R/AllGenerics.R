################################################################################
##
## $Id: AllGenerics.R 1300 2008-08-27 21:01:11Z zhao $
##
## Generic functions for the backtest class
##
################################################################################
#' @keywords internal
#' @aliases backtest
setGeneric("summaryStats", function(object, ...) standardGeneric("summaryStats"))

#' @keywords internal
#' @aliases backtest
setGeneric("ci", function(object, ...) standardGeneric("ci"))

#' @keywords internal
#' @aliases backtest
setGeneric("turnover", function(object, ...) standardGeneric("turnover"))

#' @keywords internal
#' @aliases backtest
setGeneric("means", function(object, ...) standardGeneric("means"))

#' @keywords internal
#' @aliases backtest
setGeneric("counts", function(object, ...) standardGeneric("counts"))

#' @keywords internal
#' @aliases backtest
setGeneric("totalCounts", function(object, ...) standardGeneric("totalCounts"))

#' @keywords internal
#' @aliases backtest
setGeneric("marginals", function(object, ...) standardGeneric("marginals"))

#' @keywords internal
#' @aliases backtest
setGeneric("naCounts", function(object, ...) standardGeneric("naCounts"))

setGeneric("ret.stats", function(object, ...) standardGeneric("ret.stats"))

setGeneric("results", function(object, ...) standardGeneric("results"))
