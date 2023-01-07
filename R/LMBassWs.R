#' Length-weight regression results for computing Ws for Largemouth Bass.
#'
#' Length-weight regression results from a variety of populations used for computing the standard weight (Ws) equation for Largemouth Bass (\emph{Micropterus dolomieu}).
#'
#' Each row contains the intercept (\code{log.a}) and slope (\code{b}) results from fitting the \eqn{log_{10}(W) = log_{10}(a) + b log_{10}(L)} linear regression model to a population of \code{n} fish from the given location and state. Note that \eqn{W} is weight in grams and \eqn{L} is length in mm.
#'
#' @name LMBassWs
#' @docType data
#' @format A data frame with 16 observations on the following 5 variables:
#' \describe{ 
#'   \item{site}{Location of sample.}
#'   \item{state}{State where location is located.}
#'   \item{n}{Sample size in regression.}
#'   \item{log.a}{Intercept of regression.} 
#'   \item{b}{Slope of regression.} 
#' }
#' @seealso \code{\link{rlp}}.
#' @keywords datasets
#' @examples
#' str(LMBassWs)
#' head(LMBassWs)
#' ## See example for rlp() function
#'
NULL
