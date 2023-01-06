#' @title Create a list of colors from among a variety of color palettes.
#' @description Create a list of colors from among a variety of color palettes.
#' 
#' @param pal A character that is the name of a palette. Must be one of \dQuote{cm}, \dQuote{default}, \dQuote{grey}, \dQuote{gray}, \dQuote{heat}, \dQuote{rainbow}, \dQuote{topo}, or \dQuote{terrain}.
#' @param num The number of colors to be returned.
#' @param \dots Other arguments to the various palette functions.
#' 
#' @return A vector of colors of length \code{num}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @seealso \code{\link{cm.colors}}, \code{\link{heat.colors}}, \code{\link{topo.colors}}, \code{\link{terrain.colors}}, \code{\link{rainbow}}, \code{\link{colorRampPalette}}, and \code{\link{colors}}.
#' 
#' @export
#' 
#' @keywords manip
#' 
#' @examples
#' n <- 20
#' # Color Wheels
#' pie(rep(1,n), col=chooseColors("rainbow",n))
#' pie(rep(1,n), col=chooseColors("heat",n))
#' pie(rep(1,n), col=chooseColors("topo",n))
#' pie(rep(1,n), col=chooseColors("gray",n))
#' 
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


#' @title Provides a vector of possible color palettes.
#' @description Provides a vector of possible color palettes.
#' @details Generally serves as an internal function and should be called by the user.
#' @return A single string with a palette name.
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' @keywords misc
#' @export
paletteChoices <- function() {
  c("rainbow","heat","topo","terrain","cm","default","grey","gray")
}
