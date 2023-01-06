data(RuffeWs)

## Create log10 TL and Wt
RuffeWs$logtl <- log10(RuffeWs$tl)
RuffeWs$logwt <- log10(RuffeWs$wt)

## Isolate development and validation data sets
rWs.d <- droplevels(subset(RuffeWs,use=="develop"))
str(rWs.d)

rWs.v <- droplevels(subset(RuffeWs,use=="validate"))
str(rWs.v)

## Compute regression results for all data sets in development set
# First, a function to loop through the Wt vs. TL regressions
loopreg <- function(d,alpha=0.05) {
  reg.nums <- sort(unique(d$regrnum))
  n <- minTL <- maxTL <- minWT <- maxWT <- r2 <- loga <- b <- numeric(length(reg.nums))
  reg.lbls <- country <- character(length(reg.nums))
  sigb3 <- logical(length(reg.nums))
  for (i in 1:length(reg.nums)) {
    df <- droplevels(subset(d,regrnum==reg.nums[i]))
    reg.lbls[i] <- levels(df$loc)
    minTL[i] <- min(df$tl)
    maxTL[i] <- max(df$tl)
    minWT[i] <- min(df$wt)
    maxWT[i] <- max(df$wt)
    lw.lm <- lm(logwt~logtl,data=df)
    n[i] <- dim(lw.lm$model)[1]
    r2[i] <- summary(lw.lm)$r.squared
    loga[i] <- coef(lw.lm)[1]
    b[i] <- coef(lw.lm)[2]
  }
  data.frame(reg.lbls,n,minTL,maxTL,minWT,maxWT,loga,b,sigb3,r.sq=round(r2,3))
}

# Second, do the looping and view the results
reg.d <- loopreg(rWs.d)
head(reg.d)

# Third, fit the five different Ws methods
ruf75 <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
             n.cutoff=4,cutoff.tail=TRUE)
ruf50 <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
             n.cutoff=4,cutoff.tail=TRUE,probs=0.5)
ruf75nq <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
               n.cutoff=4,cutoff.tail=TRUE,quadratic=FALSE)
ruf50nq <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
               n.cutoff=4,cutoff.tail=TRUE,probs=0.5,quadratic=FALSE)
rufFroese <- FroeseWs(reg.d$loga,reg.d$b)
