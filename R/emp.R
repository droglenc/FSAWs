#' @title Computes the standard weight equation using the empirical-percentile method.
#' 
#' @description Computes the standard weight equation using the empirical-percentile (EmP) method when given populations of length-weight data.
#' 
#' @param df A data frame that contains the length-weight data for each population.
#' @param pops A string or numeric that indicates which column in \code{df} contains the variable that identifies the different populations.
#' @param len A string or numeric that indicates which column in \code{df} contains the variable with the length data.
#' @param wt A string or numeric that indicates which column in \code{df} contains the variable with the weight data.
#' @param min A number that indicates the midpoint value of the smallest X-mm length category.
#' @param max A number that indicates the midpoint value of the largest X-mm length category.
#' @param w A number that indicates the widths for which to create length categories.
#' @param n.cutoff A numeric that indicates the minimum sample size (i.e., number of populations) in a length category that should be included in the regression.
#' @param cutoff.tail A logical that indicates if all length categories larger than the lowest length category with a number of populations below \code{n.cutoff} should be excluded \code{=TRUE} or just those length categories with sample sizes lower than \code{n.cutoff}.
#' @param qtype Type of quantile method to use. See description of types of quantile calculation methods in \code{\link{quantile}}.
#' @param probs A number that indicates the probability of the quantile. Must be between 0 and 1.
#' @param method A string that indicates whether a linear regression (\code{lm} or quantile regression (\code{rq}) should be used to construct the standard weight equation. See details.
#' @param quadratic A logical that indicates whether a quadratic regression should be fit (\code{=TRUE} or not.
#' @param x,object An object saved from the \code{emp} call (i.e., of class \code{emp}).
#' @param pch A single numeric that indicates what plotting character codes should be used for the points in \code{plot} or \code{fitPlot}.
#' @param col.pop A string that indicates the type of color or palette to use for the population of length-weight regression lines. See details.
#' @param col.Ws A string that indicates the type of color to use for the standard length-weight regression line.
#' @param lwd.Ws A numeric that indicates the width of the line to use for the standard length-weight regression line.
#' @param lty.Ws A numeric that indicates the type of line to use for the standard length-weight regression line.
#' @param jitterX A logical that indicates whether the x values in plot should be jittered.
#' @param jitter.factor A numeric that indicates the relative magnitude of the jittering in x (sent to \code{factor} argument in \code{\link{jitter}}.
#' @param col.pt A string used to indicate the color of the plotted points.
#' @param xlab A label for the x-axis of \code{fitPlot}.
#' @param ylab A label for the y-axis of \code{fitPlot}.
#' @param main A label for the main title of \code{fitPlot}.
#' @param \dots Additional arguments for methods.
#' 
#' @details The main function follows the steps of the empirical percentile method (EmP) detailed in Gerow \emph{et al.} (2005). In general, the mean \eqn{log_{10}} weight for each population within all length categories is computed, length categories from fewer than \code{ncutoff} populations are eliminated (see \code{cutoff.tail} description above), the 100\code{prob}th quantile of mean \eqn{log_{10}} weights for the remaining categories are found, and a \eqn{n}-weighted regression (quadratic regression if \code{quadratic=TRUE}) is then fit to the 100\code{probs}th quantile of mean \eqn{log_{10}} weights and the length category midpoint value.
#' 
#' Gerow \emph{et al.} (2005) suggested using a quantile definition that is basically the same as \code{qtype=9}.
#' 
#' \code{coef} returns \eqn{log_{10}(a)} and \eqn{b} values for the resultant standard weight equation. Similarly, \code{summary}, \code{anova}, and \code{predict} returns the typical results for the linear regression model used to create the standard weight equation.
#' 
#' \code{plot} method plots the mean log10 weights versus length category midpoint for each population represented in the data frame with the resultant standard weight equation superimposed in black. If the \code{col.pop} argument is is one of \code{"rainbow"}, \code{"heat"}, \code{"topo"}, \code{"terrain"}, \code{"cm"}, \code{"default"}, or \code{"grey"} and \code{order.pop=TRUE} then the populations plotted should form a general color gradient from smallest to largest weight in the initial length category. This will make it easier to identify populations that \dQuote{cross over} other populations.
#' 
#' \code{fitPlot} shows the log-transformed linear regression result; i.e., fitted line superimposed on the log-transformed 100\code{prob}th percentile predicted weights versus log-transformed midpoint length category value. The examples show how to make a corresponding residual plot.
#'  
#' @return The main function returns a list with the following items:
#' \itemize{
#'   \item \code{pop.by.len} is a table of the number of populations represented in each length category.
#'   \item \code{ind.by.len} is a table of the number of individual fish in each length category.
#'   \item \code{sumdata} is a data.frame of the mean \eqn{log_{10}} weight and length category midpoints for each population.
#'   \item \code{regdata} is the data.frame used for the Ws regression (i.e., 100\code{prob}th quantiles of mean \eqn{log_{10}} weight and length category midpoints).
#'   \item \code{quadratic} is a logical that indicates whether the quadratic regression was used.
#'   \item \code{probs} the numeric given in \code{probs}.
#'   \item \code{Ws} is the Ws regression model results.
#' }
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @seealso \code{\link{rlp}}, \code{\link{FroeseWs}}, and \code{\link{wsValidate}}.
#' 
#' @references Gerow, K.G., R.C. Anderson-Sprecher, and W.A. Hubert. 2005. A new method to compute standard-weight equations that reduces length-related bias. North American Journal of Fisheries Management 25:1288-1300.
#' 
#' @aliases emp coef.emp summary.emp predict.emp anova.emplm plot.emplm fitPlot.emplm fitPlot.emprq
#' 
#' @keywords manip hplot
#' 
#' @examples
#' ## Walleye Ws equation for comparison to Gerow's Excel Tool
#' # Gerow's results were -- -4.624269, 2.477718, and 0.1461490 for the intercept,
#' #   linear term, and quadratic term for 75th percentile Ws equation
#' # and -- -4.950281, 2.698470, and 0.1052352 for the intercept,
#' #   linear term, and quadratic term for 50th percentile Ws equation
#' 
#' # Ws75 results
#' wae1 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE)
#' coef(wae1)
#' # Ws75 results -- using same quantile type?
#' wae2 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,qtype=9,cutoff.tail=FALSE)
#' coef(wae2)
#' 
#' # Ws50 results; note use of all length categories
#' wae1a <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,n.cutoff=1,probs=0.5)
#' coef(wae1a)
#' # Ws50 results -- using same quantile type?
#' wae2a <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,qtype=9,n.cutoff=1,probs=0.5)
#' coef(wae2a)
#' 
#' # It appears that the quantiles computed here are different than Gerow's quantiles.
#' #   This is seen by comparing regdata to his 'summarized' worksheet. From
#' #   Gerow et al. (2005) it appears that he used 'qtype=9'; however, 'qtype=5'
#' #   provides the closest values to his Excel worksheet.
#' wae1$regdata
#' 
#' ## Demonstrate other extractor functions
#' summary(wae1)
#' anova(wae1)
#' predict(wae1)
#' 10^predict(wae1,data.frame(logmidpt=log10(c(200,400))))
#' 
#' plot(wae1)
#' fitPlot(wae1)
#' # a residual plot for the linear regression
#' plot(wae1$Ws$residuals~wae1$Ws$fitted.values,pch=19)
#' abline(h=0,lty=3)

#' ## use quantile regression method
#' wae1rq <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE,method="rq") 
#' coef(wae1rq)
#' plot(wae1rq)
#' 
#' @rdname emp
#' @export
emp <- function(df,pops,len,wt,min,max,w=10,n.cutoff=3,cutoff.tail=TRUE,qtype=8,
                probs=0.75,method=c("lm","rq"),quadratic=TRUE) {
   fLCat <- n <- NULL  # attempting to get by bindings warning in RCMD CHECK
   method <- match.arg(method)
   # create data frame with mean logW, midpoints, and n to be used for EmP method
   df$logwt <- log10(df[,wt])
   # adds lwr bnd len cat to df -- not factor for midpoint correction next
   df$LCat <- FSA::lencat(df[,len],w=w,startcat=min-w/2,as.fact=FALSE)
   # converts lower bound of category to a midpoint value
   df$LCat <- df$LCat+w/2
   # finds n and mean for each popn and length category
   df1 <- stats::aggregate(stats::as.formula(paste("logwt~",pops,"+LCat",sep="")),
                           data=df,FUN=function(x) cbind(length(x),mean(x)))
   # puts results into a useful data frame ... a hack
   df1 <- data.frame(as.matrix(df1))
   names(df1)[3:4] <- c("n","mn.logW")
   # creates a factored version of LCat -- for summary tables
   df1$fLCat <- factor(df1$LCat)
   # add log midpt for quantile regression method 
   df1$logmidpt <- log10(df1$LCat) 
   # find table of populations in each length category
   p.n.tbl <- table(df1$fLCat)                      
   # determine which length categories have n less than n.cutoff. Must handle
   #  differently depending on if cutoff.tail is TRUE or not
   if (!cutoff.tail) {
     ## simply find those length categories less than n.cutoff
     p.n.lows <- which(p.n.tbl < n.cutoff)
   } else {
     n.lcats <- length(p.n.tbl)
     ## find last low in first half and treat all before that as low
     ##    *  must be careful to deal with when a low does not exist
     p.n.tbl.first <- p.n.tbl[1:floor(n.lcats/2)]
     low.first <- which(p.n.tbl.first<n.cutoff)
     if (length(low.first)>0) {
       low.first <- 1:max(low.first)
     } else low.first <- NULL
     ## find first low in second half and treat all after that as low
     p.n.tbl.second <- p.n.tbl[ceiling(n.lcats/2):n.lcats]
     low.second <- which(p.n.tbl.second<n.cutoff)
     if (length(low.second)>0) {
       low.second <- (ceiling(n.lcats/2)-1+min(low.second)):n.lcats
     } else low.second <- NULL
     ## put together the lows from the first and second half
     p.n.lows <- c(low.first,low.second)
   }
   ## put an asterisk on the category names that had n below n.cutoff
   names(p.n.tbl)[p.n.lows] <- paste0(names(p.n.tbl)[p.n.lows],"*")
   # make new data frame with length categories deleted
   df2 <- droplevels(subset(df1,!fLCat %in% names(p.n.tbl)[p.n.lows]))

   # find the given quantile of mean logW in each length category
   logWq <- tapply(df2$mn.logW,df2$fLCat,stats::quantile,probs=probs,type=qtype)
   regdf <- data.frame(midpt=as.numeric(names(logWq)),
                       Wq=10^(logWq),logmidpt=log10(as.numeric(names(logWq))),
                       logwq=logWq,n=as.numeric(p.n.tbl[names(logWq)]))

   # fit the regression to define the Ws equation
   if (method=="lm") Ws <- stats::lm(logwq~logmidpt,data=regdf,weights=n)
     else Ws <- quantreg::rq(mn.logW~logmidpt,tau=probs,data=df1)
   if (quadratic) Ws <- stats::update(Ws,.~.+I(logmidpt^2)) 

  # return lots of parts
  ifelse(method=="lm",retdf <- regdf,retdf <- df1)
  res <- list(pop.by.len=p.n.tbl,ind.by.len=with(df1,tapply(n,fLCat,sum,na.rm=TRUE)),
              sumdata=df2,regdata=retdf,quadratic=quadratic,probs=probs,Ws=Ws)
  ifelse(method=="lm", class(res) <- c("emplm","emp"), class(res) <- c("emprq","emp"))
  res
} 

#' @rdname emp
#' @export
coef.emp <- function(object,...) {
  stats::coef(object$Ws,...)
}

#' @rdname emp
#' @export
summary.emp <- function(object,...) {
  summary(object$Ws,...)
}

#' @rdname emp
#' @export
predict.emp <- function(object,...) {
  stats::predict(object$Ws,...)
}

#' @rdname emp
#' @export
anova.emplm <- function(object,...) {
  stats::anova(object$Ws,...)
}

#' @rdname emp
#' @export
plot.emp <- function(x,pch=16,col.pop="rainbow",
                       col.Ws="black",lwd.Ws=3,lty.Ws=1,
                       jitterX=TRUE,jitter.factor=3,...) {
  old.par <- graphics::par(mar=c(3.5,3.5,1,1), mgp=c(2,0.75,0))
  on.exit(graphics::par(old.par))
  object <- x
  pops <- factor(unique(object$sumdata[,1]))
  if (col.pop %in% paletteChoices())
    col.pop <- chooseColors(col.pop,length(pops))
  ifelse(jitterX,x <- jitter(object$sumdata$logmidpt,factor=jitter.factor),
         x <- object$sumdata$logmidpt) 
  graphics::plot(object$sumdata$mn.logW~x,pch=pch,col=col.pop,
                 xlab="log10(Length (mm))",ylab="mean log10(Weight (g))")
  if (!object$quadratic) graphics::abline(object$Ws,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    else {
      x <- seq(min(object$regdata$logmidpt),max(object$regdata$logmidpt),
               length.out=500)
      y <- stats::predict(object$Ws,data.frame(logmidpt=x))
      graphics::lines(y~x,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    }
}

#' @rdname emp
#' @export
fitPlot.emp <- function(object,
                        pch=16,col.pt="black",col.Ws="red",lwd.Ws=3,lty.Ws=1,
                        xlab="log10(midpt Length)",
                        ylab=paste(100*object$probs,
                                   "Percentile of mean log10(Weight)"),
                        main="EMP Equation Fit",...) {
  if ("emprq" %in% class(object))
    stop("fitPlot() not implement with quantile regressions.",call.=FALSE) 
  graphics::plot(object$regdata$logwq~object$regdata$logmidpt,
                 pch=pch,col=col.pt,xlab=xlab,ylab=ylab,main=main,...)
  if (!object$quadratic) graphics::abline(object$Ws,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    else {
      x <- seq(min(object$regdata$logmidpt),max(object$regdata$logmidpt),
               length.out=500)
      y <- stats::predict(object$Ws,data.frame(logmidpt=x))
      graphics::lines(y~x,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    }
}
