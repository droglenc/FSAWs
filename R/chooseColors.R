#' @title Create a list of colors from among a variety of color palettes.
#' 
#' @description Create a list of colors from among a variety of color palettes.
#' 
#' @param pal A character that is the name of a palette.  Must be one of \dQuote{rich}, \dQuote{cm}, \dQuote{default}, \dQuote{grey}, \dQuote{gray}, \dQuote{heat}, \dQuote{jet}, \dQuote{rainbow}, \dQuote{topo}, or \dQuote{terrain}.
#' @param num The number of colors to be returned.
#' @param \dots Other arguments to the various palette functions.
#' 
#' @return A vector of colors of length \code{num}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @seealso \code{\link{rich.colors}} in \pkg{gplots}, \code{\link{cm.colors}}, \code{\link{heat.colors}}, \code{\link{topo.colors}}, \code{\link{terrain.colors}}, \code{\link{rainbow}}, \code{\link{colorRampPalette}}, and \code{\link{colors}}.
#' 
#' @export
#' 
#' @keywords manip
#' 
#' @examples
#' n <- 20
#' # Color Wheels
#' pie(rep(1,n), col=chooseColors("rich",n))
#' pie(rep(1,n), col=chooseColors("rainbow",n))
#' pie(rep(1,n), col=chooseColors("topo",n))
#' pie(rep(1,n), col=chooseColors("gray",n))
#' 
chooseColors <- function(pal=paletteChoices(),num,...) {
  pal <- match.arg(pal)
  jet.colors <- grDevices::colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  grey.colors <- grDevices::colorRampPalette(c("grey20","grey80"))
  switch(pal,
    rich={clrs <- gplots::rich.colors(num,...)},
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
  
