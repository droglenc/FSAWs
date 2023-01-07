#' @title Raw length-weight data that can be used to compute the Ws for Ruffe.
#' 
#' @description Raw length-weight data from a variety of populations used for computing the standard weight (Ws) equation for Ruffe (\emph{Gymnocephalus cernuus}). Data are from Ogle and Winfield (2009).
#' 
#' @name RuffeWs
#' 
#' @docType data
#' 
#' @format A data frame with 20005 observations on the following 13 variables:
#' \describe{
#'  \item{regrnum}{A unique numeric identifier for each separate regression -- should match one-to-one with \code{locShort}.}
#'  \item{fishID}{A numeric identifier for each fish that is not unique.}
#'  \item{country}{Country of data set.}
#'  \item{locShort}{Name (and possibly year) of data set.}
#'  \item{watertype}{Waterbody type descriptor.} 
#'  \item{year}{Year of collection.}
#'  \item{month}{Month of collection.} 
#'  \item{day}{Day of collection.} 
#'  \item{tl}{Total length (mm) of fish.}
#'  \item{fl}{Fork length (mm) of fish.} 
#'  \item{sl}{Standard length (mm) of fish.}
#'  \item{wt}{Weight (g) of fish.}
#'  \item{use}{Use of data set -- either \code{develop} or \code{validate}.} 
#' }
#' 
#' @seealso \code{\link{rlp}}, \code{\link{emp}}, and \code{\link{wsValidate}}.
#' 
#' @source From Ogle, D.H. and I.J. Winfield. 2009. Ruffe length-weight relationships with a proposed standard weight equation. North American Journal of Fisheries Management 29:850-85.
#' 
#' @keywords datasets
#' 
#' @examples
#' ## Create log10 TL and Wt
#' RuffeWs$logtl <- log10(RuffeWs$tl)
#' RuffeWs$logwt <- log10(RuffeWs$wt)
#' 
#' ## Isolate development and validation data sets
#' rWs.d <- droplevels(subset(RuffeWs,use=="develop"))
#' str(rWs.d)
#' rWs.v <- droplevels(subset(RuffeWs,use=="validate"))
#' str(rWs.v)
#' 
#' ## Loop through all regressions (for use with rlp())
#' ### First make a function that performs a regression on one regrnum
#' ###   and creates a data.frame of desired results
#' indivreg <- function(d,alpha=0.05) {
#'   tmp.lm <- lm(logwt~logtl,data=d)
#'   tmp.smry <- summary(tmp.lm)
#'   tmp.cf <- tmp.smry$coefficients[,"Estimate"]
#'   tmp.bt3 <- abs((tmp.cf[[2]]-3)/tmp.smry$coefficients["logtl","Std. Error"])
#'   tmp.bp3 <- 2*stats::pt(tmp.bt3,df=tmp.smry$df[2],lower.tail=FALSE)
#'   data.frame(regrnum=unique(d$regrnum),reg.lbls=unique(d$loc),
#'              minTL=min(d$tl,na.rm=TRUE),maxTL=max(d$tl,na.rm=TRUE),
#'              minWT=min(d$wt,na.rm=TRUE),maxWT=max(d$wt,na.rm=TRUE),
#'              n=dim(tmp.lm$model)[1],r2=tmp.smry$r.squared,
#'              loga=tmp.cf[[1]],b=tmp.cf[[2]],sigb3=tmp.bp3<alpha)
#' }
#' 
#' ### Second, split the data frame into a list where each item is one regrnum
#' tmp <- split(rWs.d,as.factor(rWs.d$regrnum))
#' ### Third, apply the indivreg function to each item in the list
#' reg.d <- as.data.frame(do.call(rbind,lapply(tmp,FUN=indivreg)))
#' ### View (partially) the results
#' head(reg.d)
#' 
#' 
#' ## Ruffe Ws equation from Ogle and Winfield
#' # Summarize TL to set min and max to emp()
#' summary(rWs.d$tl)
#' # Find the EmP Ws equation
#' emp.res.d <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
#'   n.cutoff=4,cutoff.tail=TRUE)
#' # EmP model results
#' anova(emp.res.d)
#' summary(emp.res.d)
#' # Fitted model and residual plots
#' fitPlot(emp.res.d)
#' plot(emp.res.d$Ws$residuals~emp.res.d$Ws$fitted.values,pch=19)
#' abline(h=0,lty=3)
#' # Sample sizes
#' emp.res.d$pop.by.len
#' emp.res.d$ind.by.len
#' # Fitted model relative to mean weights by length categories
#' plot(emp.res.d)
#' 
#' # Validate (explore length-related biases)
#' # Perform validation calculations -- Willis method
#' emp.v.w <- wsValidate(emp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,type="Willis")
#' # Observed validation results
#' summary(emp.v.w)
#' emp.v.w
#' # See individual regressions -- NOT RUN
#' 
#' # Perform validation calculations -- weighted EmpQ method
#' emp.v.q <- wsValidate(emp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
#'   type="EmpQ",weighted=TRUE)
#' anova(emp.v.q)
#' fitPlot(emp.v.q)
#' 
#' 
#' ## Fit the RLP model
#' rlp.res.d <- rlp(reg.d$loga,reg.d$b,min=60,max=210,w=10)
#' # RLP model results
#' summary(rlp.res.d)
#' # Fitted model and residual plot
#' fitPlot(rlp.res.d)
#' plot(rlp.res.d$Ws$residuals~rlp.res.d$Ws$fitted.values,pch=19)
#' abline(h=0,lty=3)
#' # Show rlp model relative to all regression lines
#' plot(rlp.res.d)
#' 
#' # Validate (explore length-related biases)
#' # Perform validation calculations -- Willis method
#' rlp.v.w <- wsValidate(rlp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,type="Willis")
#' summary(rlp.v.w)
#' rlp.v.w
#' 
#' 
#' # Perform validation calculations -- weighted EmpQ method
#' rlp.v.q <- wsValidate(rlp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
#'   type="EmpQ",weighted=TRUE)
#' anova(rlp.v.q)
#' fitPlot(rlp.v.q)
#' 
#' 
#' ## "Fit" the Froese model
#' Froese.res.d <- FroeseWs(reg.d$loga,reg.d$b)
#' # Observe results
#' coef(Froese.res.d)
#' plot(Froese.res.d,min=55,max=200)
#' 
#' # Validate (explore length-related biases)
#' # Perform validation calculations -- Willis method
#' Froese.v.w <- wsValidate(Froese.res.d,rWs.v,"regrnum","tl","wt",min=60,max=190,w=10,type="Willis")
#' summary(Froese.v.w)
#' Froese.v.w
#' 
#' # Perform validation calculations -- EmpQ method
#' Froese.v.q <- wsValidate(Froese.res.d,rWs.v,"regrnum","tl","wt",min=60,max=190,
#'   w=10,type="EmpQ",use.means=TRUE,weighted=TRUE)
#' anova(Froese.v.q)
#' fitPlot(Froese.v.q,ylab="Standarded Mean Mean Weight")
#' 
NULL
