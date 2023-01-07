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

# Create a list of colors from among a variety of color palettes.
chooseColors <- function(pal=paletteChoices(),num,...) {
  pal <- match.arg(pal)
  switch(pal,
         cm={clrs <- grDevices::cm.colors(num,...)},
         default={clrs <- 1:num},
         gray=,grey={clrs <- grDevices::grey.colors(num)},
         heat={clrs <- grDevices::heat.colors(num,...)},
         rainbow={clrs <- grDevices::rainbow(num,...)},
         topo={clrs <- grDevices::topo.colors(num,...)},
         terrain={clrs <- grDevices::terrain.colors(num,...)}
  )
  clrs
}

# Provides a vector of possible color palettes.
paletteChoices <- function() {
  c("rainbow","heat","topo","terrain","cm","default","grey","gray")
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
