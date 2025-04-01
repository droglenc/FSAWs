#' @title Methods to assess length-bias in a proposed standard weight equation.
#' 
#' @description The Willis and empirical quantiles (EmpQ) methods to assess length-bias in a proposed standard weight equation.
#' 
#' @param object An object of class \code{RLP} or \code{EMP} returned from calling \code{rlp} or \code{emp} in the main function and an object of class class \code{empq} or \code{willis} (saved from the \code{wsValidate}) in the generic functions.
#' @param df A data frame that contains the length-weight data for each population.
#' @param pops A string or numeric that indicates which column in \code{df} contains the variable that identifies the different populations.
#' @param len A string or numeric that indicates which column in \code{df} contains the variable with the length data.
#' @param wt A string or numeric that indicates which column in \code{df} contains the variable with the weight data.
#' @param min A number that indicates the midpoint value of the smallest X-mm length category.
#' @param max A number that indicates the midpoint value of the largest X-mm length category.
#' @param w A number that indicates the widths for which to create length categories.
#' @param type A string that indicates which type of bias detection method should be used.
#' @param n.cutoff A numeric that indicates the minimum sample size in a length category that should be included in the EmpQ regression. Ignored if \code{type="Willis"}.
#' @param cutoff.tail A logical that indicates if all length categories larger than the lowest length category with a number of populations below \code{n.cutoff} should be excluded \code{=TRUE} or just those length categories with sample sizes lower than \code{n.cutoff}. Ignored if \code{type="Willis"}.
#' @param qtype Type of quantile method to use. See description of types of quantile calculation methods in \code{\link{quantile}}. Ignored if \code{use.means=TRUE}.
#' @param probs A number that indicates the probability of the quantile. Must be between 0 and 1. Ignored if \code{use.means=TRUE}.
#' @param use.means A logical that indicates whether mean mean weight rather than a quantile mean weight should be used in the EmpQ method.
#' @param quadratic A logical that indicates whether a quadratic regression should be fit in the EmpQ method. Ignored if \code{type="Willis"}.
#' @param weighted A logical that indicates whether the regression in the EmpQ method should be weighted by the number of populations present in each length category. Ignored if \code{type="Willis"}.
#' @param alpha A numeric that indicates the rejection criterion to be used in the Willis method. Ignored if \code{type="EmpQ"}.
#' @param x An object saved from the \code{wsValidate} call (i.e., of class \code{empq} or \code{willis}).
#' @param pch A single numeric that indicates what plotting character codes should be used for the points in \code{plot} or \code{fitPlot}.
#' @param col.pt A string used to indicate the color of the plotted points.
#' @param xlab A label for the x-axis of plot or \code{fitPlot}.
#' @param ylab A label for the y-axis of plot or \code{fitPlot}.
#' @param col.mdl A string that indicates the type of color to use for the standard length-weight regression line.
#' @param lwd.mdl A numeric that indicates the width of the line to use for the standard length-weight regression line.
#' @param lty.mdl A numeric that indicates the type of line to use for the standard length-weight regression line.
#' @param main A label for the main title of \code{fitPlot}.
#' @param \dots Additional arguments for methods.
#' 
#' @details The main function can be used to assess length-bias in a proposed standard weight equation using either the method of Willis \emph{et al.} (1991) (i.e., \code{type="Willis"}) or the empirical quantiles (EmpQ) method of Gerow \emph{et al.} (2004) (i.e., \code{type="EmpQ"}). The Willis method begins by regressing the relative weight computed from the candidate standard weight equation (supplied in \code{object}) for each individual in a population in the \code{df} data frame against length. This is repeated for each population in \code{df}. The number of positive and negative slopes from this regression that are statistically significant are counted and an exact binomial test is used to determine if there is a statistically equal number of each. If there is a statistically equal number of positive and negative significant slopes then the standard weight equation is said not to exhibit a length bias.
#' 
#' \code{print}ing the Willis results will show the results for the individual regressions and a table that shows the number of significant negative and positive regression slopes. \code{summary} for the Willis results also shows the number of significant negative and positive regression slopes and the results from the exact binomial tests (using \code{\link{binom.test}} for whether the number of negative and positive slopes is the same or not.
#' 
#' The EmpQ method is performed by (1) computing the mean actual weight per \code{w}-mm length category for each population, (2) computing the quantile given in \code{probs} (default is 75th) of mean actual weight per length category across all populations, (3) standardizing the quantile mean weights by dividing each by the standard weight for the midpoint of the length categories using the proposed standard weight equation, and (4) regressing the standardized quantile mean weights against the length category midpoints. The regression can either be quadratic (i.e., \code{quadratic=TRUE}) as proposed by Gerow \emph{et al.} (2004) or n-weighted (i.e., \code{weighted=TRUE}). In addition, length categories with fewer than \code{ncutoff} populations are eliminated (see \code{cutoff.tail} description above). A slope of zero for the relationship between standardized quantile mean weights and length category midpoints indicates that no length-based biases exist with the proposed standard weight equation.
#' 
#' For the EmpQ method, \code{coef}, \code{summary}, \code{anova}, and \code{predict} returns the typical results for the linear regression model used in the method. \code{fitPlot} shows the standardized quantile mean weights versus midpoint length category with the EmpQ method regression line overlaid.
#' 
#' @return If \code{type="Willis"} then a list is returned with the following items.
#'   \itemize{
#'   \item \code{res.ind} is a data.frame with the results of the individual regressions.
#'   \item \code{res.tbl} is a table summarizing the number of positive and negative significant slopes.
#'   \item \code{res.test} contains the results for the exact binomial test.
#' }
#' 
#' If \code{type="EmpQ"} then a list is returned with the following items:
#' \itemize{
#'   \item \code{n.by.pop} is a table of the number of populations represented in each length category.
#'   \item \code{lm.v} is the EmpQ regression model results.
#'   \item \code{regdata} is a data.frame used for the EmpQ regression.
#'   \item \code{quadratic} is a logical that indicates whether the quadratic regression was used.
#'   \item \code{weighted} is a logical that indicates whether a weighted regression was used.
#'   \item \code{probs} the numeric given in \code{probs}.
#' }
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @seealso \code{\link{rlp}} and \code{\link{emp}}.
#' 
#' @references Gerow, K.G., W.A. Hubert, R.C. Anderson-Sprecher. 2004. An alternative approach to detection of length-related biases in standard weight equations. North American Journal of Fisheries Management 24:903-910.
#' 
#' Willis, D.W., C.S. Guy, and B.R. Murphy. 1991. Development and evaluation of the standard weight (Ws) equation for yellow perch. North American Journal of Fisheries Management, 11:374-380.
#' 
#' @aliases wsValidate print.willis summary.willis anova.empq coef.empq summary.empq predict.empq fitPlot.empq
#' 
#' @keywords manip
#' 
#' @examples
#' ## See examples for RuffeWs()
#' 
#' @rdname wsValidate
#' @export
wsValidate <- function(object,df,pops,len,wt,min,max,w=10,type=c("EmpQ","Willis"),
                       n.cutoff=3,cutoff.tail=TRUE,qtype=8,probs=0.75,
                       use.means=FALSE,quadratic=TRUE,weighted=FALSE,alpha=0.05) {
  ## Internal functions
  compute.Ws <- function(object,vals) {
    if (!("FroeseWs" %in% class(object))) {
      ndf <- data.frame(log10(vals))
      names(ndf) <- names(object$Ws$model)[2]
      Ws <- 10^(stats::predict(object,ndf))
    } else Ws <- object$gm.a*vals^object$mn.b
    Ws  
  }
  
  EmpQ <- function(object,df,pops,len,wt,min,w,n.cutoff,cutoff.tail,
                   qtype,probs,quadratic,weighted) {
    # adds lwr bnd len cat to df -- not factor for midpoint correction next
    df$LCat <- FSA::lencat(df[,len],w=w,startcat=min-w/2,as.fact=FALSE)
    # converts lower bound of category to a midpoint value
    df$LCat <- df$LCat+w/2
    # finds n and mean for each popn and length category
    df1 <- stats::aggregate(stats::as.formula(paste(wt,"~",pops,"+LCat",sep="")),
                            data=df,FUN=function(x) cbind(length(x),mean(x)))
    # puts results into a useful data frame ... a hack
    df1 <- data.frame(as.matrix(df1))
    names(df1)[3:4] <- c("n","mn.W")
    # creates a factored version of LCat -- for summary tables
    df1$fLCat <- factor(df1$LCat)
    # find table of populations in each length category, remove those lower than
    #    ncutoff considering cutoff.tail
    # number of population means in each length category
    p.n.tbl <- table(df1$fLCat)               
    p.n.low <- which(p.n.tbl < n.cutoff)
    p.n.adeq <- which(p.n.tbl >= n.cutoff)
    if (cutoff.tail & length(p.n.low)>0) {
      p.n.low <- p.n.low[1]:p.n.low[length(p.n.low)]
      p.n.adeq <- p.n.adeq[1]:(p.n.low[1]-1)
    }
    names(p.n.tbl)[which(names(p.n.tbl) %in% names(p.n.tbl)[p.n.low])] <- paste(names(p.n.tbl)[p.n.low],"*",sep="")  # asterisk those not used
    # make new data frame with length categories deleted
    df2 <- droplevels(subset(df1,fLCat %in% names(p.n.tbl)[p.n.adeq]))
    # find the desired quantile (or mean) of mean W in each length category
    if (use.means) Wq <- tapply(df2$mn.W,df2$fLCat,mean)
    else Wq <- tapply(df2$mn.W,df2$fLCat,stats::quantile,probs=probs,type=qtype)
    regdf <- data.frame(midpt=as.numeric(names(Wq)),wq=Wq,
                        n=as.numeric(p.n.tbl[names(Wq)]))
    # add computed Ws & stndrdzd quantile (or mean) mean weight to df
    regdf$Ws <- compute.Ws(object,regdf$midpt)                                        
    regdf$Wr <- (regdf$wq/regdf$Ws)*100
    # Add regression of standardized quartile (or mean) mean weight on midpoint
    ifelse(weighted,lm.v <- stats::lm(Wr~midpt,weights=n,data=regdf),
           lm.v <- stats::lm(Wr~midpt,data=regdf)) 
    if (quadratic) lm.v <- stats::update(lm.v,.~.+I(midpt^2))
   # return lots of parts
    list(n.by.pop=p.n.tbl,lm.v=lm.v,regdata=regdf,
         quadratic=quadratic,weighted=weighted,probs=probs)
  } ## end of internal EmpQ function
   
  Willis <- function(object,df,pops,len,wt,alpha) {
    # modify database with standard weight and relative weight
    # Add reg of standardized quartile (or mean) mean weight on midpoint
    df$Ws <- compute.Ws(object,df[,len])                                              
    df$Wr <- (df[,wt]/df$Ws)*100
    # loop through regressions of Wr on length by "fishery" keeping track
    #   of sign of significant relationships.
    reg.nums <- sort(unique(df[,pops]))
    pop <- len.slope <- len.slope.p <- numeric(length(reg.nums))
    sig.slope <- logical(length(reg.nums))
    sign.slope <- character(length(reg.nums))
    for (i in 1:length(reg.nums)) {
      pop[i] <- reg.nums[i]
      df1 <- df[df[,pops]==reg.nums[i],]
      lm1 <- stats::lm(df1$Wr~df1[,len])
      len.slope[i] <- stats::coef(lm1)[2]
      len.slope.p[i] <- summary(lm1)$coefficients[2,"Pr(>|t|)"]
    }
    sig.slope <- len.slope.p < alpha
    sign.slope[sig.slope] <- sign(len.slope[sig.slope])
    sign.slope <- factor(sign.slope,levels=c("-1","","1"))
    res.ind <- data.frame(pop,slope=len.slope,p.value=len.slope.p,
                          significant=sig.slope,sign=sign.slope)
    # binomial test
    res.tbl <- table(sign.slope,exclude="")
    rownames(res.tbl) <- c("Negative","Positive")
    res.test <- stats::binom.test(res.tbl[1],sum(res.tbl))
    # return results
    list(res.ind=res.ind,res.tbl=res.tbl,res.test=res.test)
  }  ## end of internal Willis function 
  
  ## start of main function
  # attempting to get by bindings warning in RCMD CHECK
  fLCat <- n <- NULL
  type <- match.arg(type)
  # modify database by limiting to lengths within modeling end points
  df <- df[df[,len]>=min-w/2 & df[,len]<max+w/2,]
  if (type=="EmpQ") {
    res <- EmpQ(object,df,pops,len,wt,min,w,n.cutoff,cutoff.tail,
                qtype,probs,quadratic,weighted)
    class(res) <- c("empq")
  } else {
    res <- Willis(object,df,pops,len,wt,alpha)
    class(res) <- c("willis")
  }
  res
}

#' @rdname wsValidate
#' @export
print.willis <- function(x,...) {
  cat("Individual Regression Results.\n")
  print(x$res.ind)
  cat("\nSummary Table of Significant Slopes.\n")
  print(x$res.tbl)
}

#' @rdname wsValidate
#' @export
summary.willis <- function(object,...) {
  cat("\nSummary Table of Significant Slopes.\n")
  print(object$res.tbl)
  cat("\n")
  print(object$res.test)
}

#' @rdname wsValidate
#' @export
coef.empq <- function(object,...) {
  stats::coef(object$lm.v,...)
}

#' @rdname wsValidate
#' @export
summary.empq <- function(object,...) {
  cat("\nNumber of Populations in Each Length Category.\n")
  print(object$n.by.pop)
  cat("\n")
  summary(object$lm.v,...)
}

#' @rdname wsValidate
#' @export
anova.empq <- function(object,...) {
  stats::anova(object$lm.v,...)
}

#' @rdname wsValidate
#' @export
predict.empq <- function(object,...) {
  stats::predict(object$lm.v,...)
}

#' @rdname wsValidate
#' @export
fitPlot.empq <- function(object,pch=16,col.pt="black",
                         col.mdl="red",lwd.mdl=3,lty.mdl=1,
                         xlab="Midpoint Length Category",
                         ylab=paste("Standardized",100*object$probs,
                                    "Percentile Mean Weight"),
                         main="EmpQ Method",...) {
  graphics::plot(Wr~midpt,data=object$regdata,
                 xlab=xlab,ylab=ylab,main=main,
                 pch=pch,col=col.pt,...)
  if (!object$quadratic) 
    graphics::abline(object$lm.v,col=col.mdl,lwd=lwd.mdl,lty=lty.mdl)
    else {
      x <- seq(min(object$regdata$midpt),max(object$regdata$midpt),length.out=500)
      y <- stats::predict(object$lm.v,data.frame(midpt=x))
      graphics::lines(y~x,col=col.mdl,lwd=lwd.mdl,lty=lty.mdl)
    }
}
