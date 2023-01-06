#'Internal functions used in FSAWs.
#'
#'Internal functions used in FSAWs
#'
#'@rdname FSAWs-internals
#'@keywords internal
#'@aliases .onAttach
#'

## Sends a start-up message to the console when the package is loaded.
.onAttach <- function(lib,pkg,...) {
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),
                   fields="Version")
  msg <- paste0("## FSAWs v",vers,". See citation('FSAWs') if used in publication.\n")
  packageStartupMessage(msg)
}

#' @title Fitted model plots.
#' @description A generic function for constructing a fitted model plot.
#' @details Specifics described relative to specific object (e.g., see \code{\link{emp}}).
#' 
#' @param object An object (see details).
#' @param \dots Other arguments to be passed to the plot functions.
#'
#' @return None. However, a fitted-line plot is produced.
#
#' @export
fitPlot <- function (object, ...) {
  UseMethod("fitPlot")
}
