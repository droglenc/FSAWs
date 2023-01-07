#' @title Computes the standard weight equation using the methods described in Froese (2006).
#' 
#' @description Computes the standard weight equation using the geometric mean of \eqn{a} and the mean of \eqn{b} from weight-length regression equations as described in Froese (2006).
#' 
#' @param log.a A numeric vector that contains the \eqn{log_{10}(a)} values for the population of length-weight regression equations.
#' @param b A numeric vector that contains the \eqn{b} values for the population of length-weight regression equations
#' @param x,object An object saved from the \code{FrowseWs()} call (i.e., of class \code{Froese}).
#' @param min A number that indicates the midpoint value of the smallest X-mm length to model.
#' @param max A number that indicates the midpoint value of the largest X-mm length category.
#' @param what A string that indicates the type of plot to produce. See details.
#' @param col.pop A string that indicates the type of color or palette to use for the population of length-weight regression lines. See details.
#' @param order.pop A logical that indicates whether the populations should be plotted from the smallest to largest weight in the initial length category. See details.
#' @param lwd.pop A numeric that indicates the width of the line to use for the population of length-weight regression lines.
#' @param lty.pop A numeric that indicates the type of line to use for the population of length-weight regression lines.
#' @param col.Ws A string that indicates the type of color to use for the standard length-weight regression line.
#' @param lwd.Ws A numeric that indicates the width of the line to use for the standard length-weight regression line.
#' @param lty.Ws A numeric that indicates the type of line to use for the standard length-weight regression line.
#' @param \dots Additional arguments for methods.
#' 
#' @details The main function computes the mean of the \eqn{log_{10}(a)} and \eqn{b} values for the standard weight equation as detailed in Froese (2006). Note that  \eqn{log_{10}(a)} and \eqn{b} must be from the regression of \eqn{log_{10}(W)} on \eqn{log_{10}(L)} where \eqn{W} is measured in grams and \eqn{L} is the total length measured in mm.
#' 
#' The \code{what} argument in the \code{plot} method can be set to \code{"both"}, \code{"log"}, or \code{"raw"}. The \code{"raw"} plot shows lines on the length-weight scale for each population with the resultant standard weight equation superimposed in red. The \code{"log"} plot constructs a similar plot but on the \eqn{log_{10}(weight)}-\eqn{log_{10}(length)} scale. The \code{"both"} option produces both plots side-by-side. If the \code{col.pop} argument is one of \code{"rainbow"}, \code{"heat"}, \code{"topo"}, \code{"terrain"}, \code{"cm"}, \code{"default"}, or \code{"grey"} and \code{order.pop=TRUE} then the populations plotted should form a general color gradient from smallest to largest weight in the initial length category. This will make it easier to identify populations that \dQuote{cross over} other populations.
#' 
#' \code{coef} returns the geometric mean of \eqn{a} and the mean of \eqn{b} to serve as the standard weight equation as described in Froese (2006).
#' 
#' @return A list is returned with the following items:
#' \itemize{
#'   \item \code{log.a} is a numeric vector of the observed \eqn{log_{10}(a)} values sent in the \code{log.a} argument.
#'   \item \code{b} is a numeric vector of the observed \eqn{b} values sent in the \code{b} argument.
#'   \item \code{gm.a} is a numeric that contains the geometric mean of the \eqn{a} parameter. This is simply the back-transformed mean \eqn{log_{10}(a)} value -- i.e., \eqn{10^{log_{10}(a)}}.
#'   \item \code{mn.b} is the arithmetic mean of the \eqn{b} parameter. 
#'   \code{mn.log.a} is the arithmetic mean of \eqn{log_{10}(a)}.
#' }
#' 
#' @seealso \code{\link{rlp}}, \code{\link{emp}}, and \code{\link{wsValidate}}
#'
#' @references Froese, R. 2006. Cube law, condition factor and weight-length relationships: history, meta-analysis and recommendations. Journal of Applied Ichthyology 22:241-253.
#' 
#' @aliases FroeseWs plot.FroeseWs coef.FroeseWs
#' 
#' @keywords manip hplot
#' 
#' @examples
#' #See examples in RuffeWs.
#' 
#' @rdname FroeseWs
#' @export
FroeseWs <- function(log.a,b) {
  mn.log.a <- mean(log.a)
  gm.a <- 10^mn.log.a
  mn.b <- mean(b)
  res <- list(log.a=log.a,b=b,gm.a=gm.a,mn.b=mn.b,mn.log.a=mn.log.a)
  class(res) <- "FroeseWs"
  res
}

#' @rdname FroeseWs
#' @export
coef.FroeseWs <- function(object,...) {
  res <- c(object$gm.a,object$mn.b)
  names(res) <- c("gm.a","mn.b")
  res
}

#' @rdname FroeseWs
#' @export
plot.FroeseWs <- function(x,min,max,what=c("both","raw","log"),
                          col.pop="rainbow",lwd.pop=1,lty.pop=1,order.pop=TRUE,
                          col.Ws="black",lwd.Ws=3,lty.Ws=1,...) {
  object <- x
  what <- match.arg(what)
  if (col.pop %in% paletteChoices()) col.pop <- chooseColors(col.pop,length(object$b))
  len <- seq(min,max,length.out=200)
  # predict weight for each population
  for (i in 1:length(object$log.a)) {                                     
    w <- 10^(object$log.a[i]+object$b[i]*log10(len))
    # store in a matrix called pred.w
    if (i==1) pred.w <- w                                          
    else pred.w <- cbind(pred.w,w)
  }
  # rename columns to numbers that correspond to populations
  colnames(pred.w) <- seq(1:dim(pred.w)[2])                        
  if (order.pop) pred.w <- pred.w[,order(pred.w[1,])]
  if (what=="both") old.par <- graphics::par(mar=c(3.5,3.5,1,1),
                                             mfcol=c(1,2),mgp=c(2,0.75,0))
    else old.par <- graphics::par(mar=c(3.5,3.5,1,1), mgp=c(2,0.75,0))
  on.exit(graphics::par(old.par))
  if (what=="raw" | what=="both") {
    graphics::matplot(len,pred.w,type="l",lty=lty.pop,lwd=lwd.pop,col=col.pop,
                      xlab="Length (mm)",ylab="Weight (g)")
    graphics::curve(object$gm.a*x^object$mn.b,min(len),max(len),
                    lty=lty.Ws,lwd=lwd.Ws,col=col.Ws,add=TRUE)
  }
  if (what=="log" | what=="both") {
    graphics::matplot(log10(len),log10(pred.w),type="l",
                      lty=lty.pop,lwd=lwd.pop,col=col.pop,
                      xlab="log10(Length (mm))",ylab="log10(Weight (g))")
    graphics::curve(log10(object$gm.a)+object$mn.b*x,
                    min(log10(len)),max(log10(len)),
                    lty=lty.Ws,lwd=lwd.Ws,col=col.Ws,add=TRUE)
  }
}
