#' @title Computes the standard weight equation using the regression-line-percentile method.
#' 
#' @description Computes the standard weight equation using the regression-line-percentile (RLP) method when given \eqn{log_{10}(a)} and \eqn{b} values from (log-transformed) length-weight regressions fit to several populations.
#' 
#' @param log.a A numeric vector that contains the \eqn{log_{10}(a)} values for the population of length-weight regression equations.
#' @param b A numeric vector that contains the \eqn{b} values for the population of length-weight regression equations
#' @param min A number that indicates the midpoint value of the smallest X-mm length category.
#' @param max A number that indicates the midpoint value of the largest X-mm length category.
#' @param w A number that indicates the widths for which to create length categories.
#' @param qtype Type of quantile method to use. See description of types of quantile calculation methods in \code{\link{quantile}}.
#' @param probs A number that indicates the probability of the quantile. Must be between 0 and 1.
#' @param digits Number of digits to round predicted weights. If \code{NULL} (default), no rounding will be used.
#' @param x,object An object saved from the \code{rlp} call (i.e., of class \code{rlp}).
#' @param what A string that indicates the type of plot to produce. See details.
#' @param col.pop A string that indicates the type of color or palette to use for the population of length-weight regression lines. See details.
#' @param order.pop A logical that indicates whether the populations should be plotted from the smallest to largest weight in the initial length category. See details.
#' @param lwd.pop A numeric that indicates the width of the line to use for the population of length-weight regression lines.
#' @param lty.pop A numeric that indicates the type of line to use for the population of length-weight regression lines.
#' @param col.Ws A string that indicates the type of color to use for the standard length-weight regression line.
#' @param lwd.Ws A numeric that indicates the width of the line to use for the standard length-weight regression line.
#' @param lty.Ws A numeric that indicates the type of line to use for the standard length-weight regression line.
#' @param pch A single numeric that indicates what plotting character codes should be used for the points in \code{fitPlot}.
#' @param col.pt A string used to indicate the color of the plotted points.
#' @param xlab A label for the x-axis of \code{fitPlot}.
#' @param ylab A label for the y-axis of \code{fitPlot}.
#' @param main A label for the main title of \code{fitPlot}.
#' @param \dots Additional arguments for methods.
#' 
#' @details The main function follows the steps of the regression-line-percentile (RLP) method detailed in Murphy \emph{et al.} (1990). In summary, the given \eqn{log_{10}(a)} and \eqn{b} values are used to predict a weight at the midpoint of each length class defined by \code{w} (e.g., 10 mm length classes if \code{w=10}) for each population; the predicted weight at the \code{prob}th percentile (wq) across all populations is then identified; and a linear regression equation is fit to the \eqn{log_{10}(wq)} and \eqn{log_{10}(midpoint length)} data.
#' 
#' Note that \eqn{log_{10}(a)} and \eqn{b} must be from the regression of \eqn{log_{10}(W)} on \eqn{log_{10}(L)} where \eqn{W} is measured in grams and \eqn{L} is the length (traditionally total length) measured in mm.
#' 
#' It appears that Murphy \emph{et al.} (1990) used \code{qtype=6} in their SAS program.
#' 
#' The \code{what} argument in the \code{plot} method can be set to \code{"both"}, \code{"log"}, or \code{"raw"}. The \code{"raw"} plot shows lines on the length-weight scale for each population with the resultant standard weight equation superimposed in red. The \code{"log"} plot constructs a similar plot but on the \eqn{log_{10}(weight)}-\eqn{log_{10}(length)} scale. The \code{"both"} option produces both plots side-by-side. If the \code{col.pop} argument is one of \code{"rainbow"}, \code{"heat"}, \code{"topo"}, \code{"terrain"}, \code{"cm"}, \code{"default"}, or \code{"grey"} and \code{order.pop=TRUE} then the populations plotted should form a general color gradient from smallest to largest weight in the initial length category. This will make it easier to identify populations that \dQuote{cross over} other populations.
#' 
#' \code{fitPlot} shows the log-transformed linear regression result; i.e., fitted line superimposed on the log-transformed \code{prob}the percentile predicted weights versus log-transformed midpoint length category value. The examples show how to make a corresponding residual plot.
#'  
#' \code{coef} returns \eqn{log_{10}(a)} and \eqn{b} values for the resultant standard weight equation. Similarly, \code{summary}, \code{anova}, and \code{predict} returns the typical results for the linear regression model used to create the standard weight equation.
#' 
#' @return The main function returns a list with the following items:
#' \itemize{
#'   \item \code{log.a} is a numeric vector of the observed \eqn{log_{10}(a)} values sent in the \code{log.a} argument.
#'   \item \code{b} is a numeric vector of the observed \eqn{b} values sent in the \code{b} argument.
#'   \item \code{data.pred} is a matrix of the predicted weight at length for all populations.
#'   \item \code{data.reg} contains a data frame with the \code{prob}th quartile of predicted weights and the midpoint lengths.
#'   \item \code{Ws} is an \code{lm} object that contains the results of the regression of \eqn{log_{10}(wq)} on \eqn{log_{10}(midpoint length)}.
#' }
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @seealso \code{\link{emp}}, \code{\link{FroeseWs}}, and \code{\link{wsValidate}}; and \code{quantile} in \pkg{stats}
#' 
#' @references Murphy, B.R., M.L. Brown, and T.A. Springer. 1990. Evaluation of the relative weight (Wr) index, with new applications to walleye. North American Journal of Fisheries Management, 10:85-97.
#' 
#' @aliases rlp plot.rlp anova.rlp coef.rlp predict.rlp fitPlot.rlp summary.rlp
#' 
#' @keywords manip hplot
#' 
#' @examples
#' ## Recreate Murphy et al. (1990) results for Largemouth Bass
#' # min and max lengths were 152 and 816
#' lmb.rlp <- rlp(LMBassWs$log.a,LMBassWs$b,155,815,qtype=6)
#' coef(lmb.rlp)
#' # compare to log.a=-5.379 and b=3.221
#' 
#' ## Examples of the other extractor functions
#' summary(lmb.rlp)
#' anova(lmb.rlp)
#' predict(lmb.rlp)
#' 10^predict(lmb.rlp,data.frame(logmidpt=log10(c(200,400))))
#' 
#' plot(lmb.rlp)
#' fitPlot(lmb.rlp)
#' # a residual plot for the linear regression
#' plot(lmb.rlp$Ws$residuals~lmb.rlp$Ws$fitted.values,pch=19)
#' abline(h=0,lty=3)
#' 
#' @rdname rlp
#' @export
rlp <- function(log.a,b,min,max,w=10,qtype=8,probs=0.75,digits=NULL) {
  # create length-class midpoints
  midpt <- seq(min,max,w)                                          
  # predict weight at each 1-cm midpoint for each population
  for (i in 1:length(log.a)) {                                     
    w <- 10^(log.a[i]+b[i]*log10(midpt))
    if (!is.null(digits)) w <- round(w,digits)
    # store in a matrix called pred.w
    if (i==1) pred.w <- w                                          
    else pred.w <- cbind(pred.w,w)
  }
  # rename columns to numbers that correspond to populations
  colnames(pred.w) <- seq(1:dim(pred.w)[2])                        
  # rename rows to correspond to midpt lengths
  rownames(pred.w) <- midpt                                        
  # find qth percentile of predicted weights
  wq <- apply(pred.w,MARGIN=1,FUN=stats::quantile,probs=probs,type=qtype) 
  logwq <- log10(wq)
  logmidpt <- log10(midpt)
  # regression of qth weights on lengths to get Ws equation                              
  Ws <- stats::lm(logwq~logmidpt)                                            
  z <- list(log.a=log.a,b=b,data.pred=pred.w,
            regdata=data.frame(midpt=midpt,wq=wq,logmidpt=logmidpt,logwq=logwq),
            Ws=Ws,probs=probs)
  class(z) <- "rlp"
  z
}

#' @rdname rlp
#' @export
plot.rlp <- function(x,what=c("both","raw","log"),
                     col.pop="rainbow",lwd.pop=1,lty.pop=1,order.pop=TRUE,
                     col.Ws="black",lwd.Ws=3,lty.Ws=1,...) {
  object <- x
  what <- match.arg(what)
  if (col.pop %in% paletteChoices())
    col.pop <- chooseColors(col.pop,length(object$b))
  ml <- object$regdata$midpt
  pw <- object$data.pred
  if (order.pop) pw <- pw[,order(pw[1,])]
  if (what=="both") old.par <- graphics::par(mar=c(3.5,3.5,1,1),
                                             mfcol=c(1,2),mgp=c(2,0.75,0))
    else old.par <- graphics::par(mar=c(3.5,3.5,1,1),mgp=c(2,0.75,0))
  on.exit(graphics::par(old.par))
  if (what=="raw" | what=="both") {
    graphics::matplot(ml,pw,type="l",lty=lty.pop,lwd=lwd.pop,col=col.pop,
                      xlab="Length (mm)",ylab="Weight (g)")
    graphics::curve(10^stats::coef(object$Ws)[1]*x^stats::coef(object$Ws)[2],
                    min(ml),max(ml),lty=lty.Ws,lwd=lwd.Ws,col=col.Ws,add=TRUE)
  }
  if (what=="log" | what=="both") {
    graphics::matplot(log10(ml),log10(pw),type="l",
                      lty=lty.pop,lwd=lwd.pop,col=col.pop,
                      xlab="log10(Length (mm))",ylab="log10(Weight (g))")
    graphics::abline(object$Ws,lty=lty.Ws,lwd=lwd.Ws,col=col.Ws)
  }
}

#' @rdname rlp
#' @export
coef.rlp <- function(object,...) {
  stats::coef(object$Ws,...)
}

#' @rdname rlp
#' @export
summary.rlp <- function(object,...) {
  summary(object$Ws,...)
}

#' @rdname rlp
#' @export
predict.rlp <- function(object,...) {
  stats::predict(object$Ws,...)
}

#' @rdname rlp
#' @export
anova.rlp <- function(object,...) {
  stats::anova(object$Ws,...)
}

#' @rdname rlp
#' @export
fitPlot.rlp <- function(object,
                        pch=16,col.pt="black",col.Ws="red",lwd.Ws=3,lty.Ws=1,
                        xlab="log10(midpt Length)",
                        ylab=paste0("log10(",100*object$prob,
                                    " Percentile of Predicted Weight)"),
                        main="RLP Equation Fit",...) {
  graphics::plot(object$regdata$logwq~object$regdata$logmidpt,
                 pch=pch,col=col.pt,xlab=xlab,ylab=ylab,main=main,...)
  graphics::abline(object$Ws,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
}
