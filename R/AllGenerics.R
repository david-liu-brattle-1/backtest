################################################################################
##
## $Id: AllGenerics.R 1300 2008-08-27 21:01:11Z zhao $
##
## Generic functions for the backtest class
##
################################################################################

#' @aliases backtest
setGeneric("summaryStats", function(object, ...) standardGeneric("summaryStats"))

#' @aliases backtest
setGeneric("ci", function(object, ...) standardGeneric("ci"))

#' @aliases backtest
setGeneric("turnover", function(object, ...) standardGeneric("turnover"))

#' @aliases backtest
setGeneric("means", function(object, ...) standardGeneric("means"))

#' @aliases backtest
setGeneric("counts", function(object, ...) standardGeneric("counts"))

#' @aliases backtest
setGeneric("totalCounts", function(object, ...) standardGeneric("totalCounts"))

#' @aliases backtest
setGeneric("marginals", function(object, ...) standardGeneric("marginals"))

#' @aliases backtest
setGeneric("naCounts", function(object, ...) standardGeneric("naCounts"))

#' @aliases backtest
setGeneric("ret.stats", function(object, ...) standardGeneric("ret.stats"))

#' @aliases backtest
setGeneric("results", function(object, ...) standardGeneric("results"))
