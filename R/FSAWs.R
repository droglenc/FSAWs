#' @title Functions for constructing and validating standard weight (Ws) equations for fish.
#' 
#' @description Functions for constructing and validating standard weight (Ws) equations for fish.  These functions were created for Ogle and Winfield (2009) but have been used in several other papers (see references below).  These functions are no longer actively maintained.
#'
#' @references
#' Ogle DH, Winfield IJ.  2009.  Ruffe length-weight relationships with a proposed standard weight equation.  North American Journal of Fisheries Management 29:850-85.
#' 
#' Sulun S, Baskurt S, Emiroglu O, Giannetto D, Tarkan AS, Agdamar S, Gaygusuz O, Dorak Z, Aydin H, Dicek A.  2014.  Development of empirical standard weight equation for Pursak chub \emph{Squalius pursakensis}, an endemic cyprinid of Northwest Anatolia.  Turkish Journal of Zoology.
#' 
#' Giannetto D, Carosi A, Franchi E, La Porta G, Lorenzoni M. 2012. Proposed standard weight (Ws) equation for European perch (\emph{Perca fluviatilis} Linnaeus, 1758).  Journal of Applied Ichthyology 28:34-39.
#' 
#' Giannetto D, Franchi E, Pompei L, Lorenzoni M, Porcellotti S, Tancioni L. 2012.  Proposed empirical standard weight equation for brook chub \emph{Squalius lucumonis}. North American Journal of Fisheries Management 32:428-435.
#' 
#' Giannetto D, Pompei L, Lorenzoni M, Tarkan AS. 2012. Empirical standard weight equation for the Aegean Chub \emph{Squalius fellowesii}, an endemic freshwater fish species of Western Anatolia, Turkey.  North American Journal of Fisheries Management 32:1102-1107. 
#' 
#' Lorenzoni M, Giannetto D, Maio G, Pizzul E, Pompei L, Turin P, Vincenzi S, Crivelli A. 2012. Empirical standard mass equation for \emph{Salmo marmoratus}. Journal of Fish Biology  81:2086-2091. 
#' 
#' Giannetto D, La Porta G, Maio G, Pizzul E, Turin P, Lorenzoni M. 2011. Proposed standard mass equations for European chub \emph{Leuciscus cephalus} in Italy. Journal of Fish Biology 78:1890-1899.
#' 
#' Giannetto D, PeDiCillo G. 2011. Proposed standard weight (Ws) equations for \emph{Telestes muticellus} (Bonaparte, 1837) in the Tiber River basin.  Cybium 35:141-147. 

#' @importFrom FSA fitPlot residPlot
#' @importFrom gdata drop.levels
#' @importFrom gplots rich.colors
#' @importFrom quantreg rq
#' 
#' @docType package
#' @name FSAWs
NULL
