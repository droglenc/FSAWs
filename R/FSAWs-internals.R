#'Internal functions used in FSAWs.
#'
#'Internal functions used in FSAWs
#'
#'@note Take note of the following uses:
#'\itemize{
#'  \item \code{lencatOLD} used in \code{\link{emp}} and \code{\link{wsValidate}}.
#'}
#'
#'@rdname FSAWs-internals
#'@keywords internal
#'@aliases .onAttach lencatOLD
#'

##################################################################
## Sends a start-up message to the console when the package is loaded.
##################################################################
.onAttach <- function(lib,pkg,...) {
  ## Get version number -- basically code from readPkgVersion in SweaveListingUtils
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),fields="Version")
  ## Send message
  msg <- paste("\n\n")
  msg <- paste(msg,"############################################\n")
  msg <- paste(msg,"##","FSAWs package, version",vers,"          ##\n")
  msg <- paste(msg,"##   by Derek H. Ogle, Northland College  ##\n")
  msg <- paste(msg,"##                                        ##\n")
  msg <- paste(msg,"## Type ?FSAWs for documentation.         ##\n")
  msg <- paste(msg,"## Type citation('FSAWs') for citation    ##\n")
  msg <- paste(msg,"##   please cite if used in publication.  ##\n")
  msg <- paste(msg,"############################################\n\n")
  packageStartupMessage(msg)
}


##################################################################
## Internal files used in emp, wsValidate                                     
##################################################################
lencatOLD <- function(df,cl,startcat=0,w=1,breaks=NULL,right=FALSE,vname=NULL,as.fact=TRUE,drop.levels=FALSE) {
  ## INTERNAL -- make.vname
  make.vname <- function(vname,df) {
    if (is.null(vname)) vname <- "LCat"                   # if no name given then default to "LCat"
    vnames <- c(vname,paste(vname,seq(1:100),sep=""))     # create list of names that includes vname & vname with numbers appended 
    ind <- which(vnames %in% names(df))                   # find first instance where names match
    if (length(ind)==0) vname <- vname                    # if no match then go with given vname
    else vname <- vnames[max(ind)+1]                    # if is match then go with name that is one index later in vnames
    vname
  }
  ## INTERNAL -- checkStartcatW
  checkStartcatW <- function(startcat,w,d) {
    # is w positive?
    if (w<=0) stop("\n Width value must be positive.",call.=FALSE)
    # is startcat positive? 
    if (startcat < 0) stop("\n Starting category values must be non-negative.",call.=FALSE)
    # is startcat less than minimum observation
    if (min(d,na.rm=TRUE) < startcat)
      stop("\n Starting category is larger than minimum observation.  Adjust the startcat that you used.",call.=FALSE)
    # get decimals for w and startcat
    # Used in next line to find number of decimals to use
    z <- c(1,0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001)
    # Determine number of decimals from width value
    wdec <- which(w/z>=1)[1]-1
    # Determine number of decimals from startcat value
    ifelse(startcat>0,scdec <- which(startcat/z>=1)[1]-1,scdec <- 0)
    # does w have more than (or equal) decimals as startcat 
    if (scdec>wdec) stop("\n Starting category value should not have more decimals than width value",call.=FALSE)
    # return decimals
    list(scdec=scdec,wdec=wdec)
  } ## end checkStartcatW
  
  if (is.null(breaks)) {
    decs <- checkStartcatW(startcat,w,df[,cl])  
    breaks <- seq(startcat,max(df[,cl],na.rm=TRUE)+w,w)                           # Creates cut breaks (one more than max so that max is included in a break)
    breaks <- round(breaks,decs$wdec)
  } else {
    if (min(df[,cl],na.rm=TRUE) < breaks[1]) stop(paste("Lowest break (",breaks[1],") is larger than minimum observation (",min(df[,cl]),").\n  Adjust the breaks you used.",sep=""),call.=FALSE)
    if (max(df[,cl],na.rm=TRUE) >= max(breaks)) {
      breaks <- c(breaks,1.1*max(df[,cl],na.rm=TRUE))
      warning(paste("Largest break (",breaks[length(breaks)-1],") was less than or equal to the maximum observation (",max(df[,cl]),").\n  A larger break (",breaks[length(breaks)],") was added.\n",sep=""),call.=FALSE)
    }
  }
  lcat <- breaks[cut(df[,cl],breaks,labels=FALSE,right=right,include.lowest=TRUE)]
  if (as.fact) {
    if (!drop.levels) lcat <- factor(lcat,levels=breaks[-length(breaks)])        # Generally don't drop levels but need to drop the "extra" last level in all cases
    else lcat <- factor(lcat)
  }
  nd <- data.frame(df,lcat)                                                      # Puts length class variable in data.frame
  names(nd)[dim(df)[2]+1] <- make.vname(vname,df)                                # Renames the new variable if so desired
  nd                                                                             # Returns the new data.frame
}
