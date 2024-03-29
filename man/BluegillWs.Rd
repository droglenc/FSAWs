% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BluegillWs.R
\docType{data}
\name{BluegillWs}
\alias{BluegillWs}
\title{Length-weight regression results for computing Ws for Bluegill.}
\format{
A data frame with 27 observations on the following 5 variables:
 \describe{ 
   \item{site}{Location of sample.}
   \item{state}{State where location is located.}
   \item{n}{Sample size in regression.}
   \item{log.a}{Intercept of regression.}
   \item{b}{Slope of regression.} 
 }
}
\source{
From Table 2 in Murphy, B.R., M.L. Brown, and T.A. Springer. 1990. Evaluation of the relative weight (Wr) index, with new applications to walleye. North American Journal of Fisheries Management, 10:85-97.
}
\description{
Length-weight regression results from a variety of populations used for computing the standard weight (Ws) equation for Bluegill (\emph{Lepomis macrochirus}).
}
\details{
Each row contains the intercept (\code{log.a}) and slope (\code{b}) results from fitting the \eqn{log_{10}(W) = log_{10}(a) + b log_{10}(L)} linear regression model to a population of \code{n} fish from the given location and state. Note that \eqn{W} is weight in grams and \eqn{L} is length in mm.
}
\examples{
str(BluegillWs)
head(BluegillWs)

## Recreate Murphy et al. (1990) results for bluegills
bg.rlp <- rlp(BluegillWs$log.a,BluegillWs$b,75,395,qtype=6)
coef(bg.rlp)
# compare to log.a=-5.385 and b=3.318

}
\seealso{
\code{WSlit}, \code{WSval} and \code{rlp} in \pkg{FSA}.
}
\keyword{datasets}
