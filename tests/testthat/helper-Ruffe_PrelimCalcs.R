data(RuffeWs)
## Create log10 TL and Wt
RuffeWs$logtl <- log10(RuffeWs$tl)
RuffeWs$logwt <- log10(RuffeWs$wt)

## Isolate development and validation data sets
rWs.d <- droplevels(subset(RuffeWs,use=="develop"))
str(rWs.d)
rWs.v <- droplevels(subset(RuffeWs,use=="validate"))
str(rWs.v)

## Loop through all regressions
### First make a function that performs a regression on one regrnum
###   and creates a data.frame of desired results
indivreg <- function(d,alpha=0.05) {
  tmp.lm <- lm(logwt~logtl,data=d)
  tmp.smry <- summary(tmp.lm)
  tmp.cf <- tmp.smry$coefficients[,"Estimate"]
  tmp.bt3 <- abs((tmp.cf[[2]]-3)/tmp.smry$coefficients["logtl","Std. Error"])
  tmp.bp3 <- 2*stats::pt(tmp.bt3,df=tmp.smry$df[2],lower.tail=FALSE)
  data.frame(regrnum=unique(d$regrnum),reg.lbls=unique(d$loc),
             minTL=min(d$tl,na.rm=TRUE),maxTL=max(d$tl,na.rm=TRUE),
             minWT=min(d$wt,na.rm=TRUE),maxWT=max(d$wt,na.rm=TRUE),
             n=dim(tmp.lm$model)[1],r2=tmp.smry$r.squared,
             loga=tmp.cf[[1]],b=tmp.cf[[2]],sigb3=tmp.bp3<alpha)
}

### Second, split the data frame into a list where each item is one regrnum
tmp <- split(rWs.d,as.factor(rWs.d$regrnum))
### Third, apply the indivreg function to each item in the list
reg.d <- as.data.frame(do.call(rbind,lapply(tmp,FUN=indivreg)))
### View (partially) the results
head(reg.d)


# Fit the five different Ws methods
ruf75.rlp <- rlp(reg.d$loga,reg.d$b,min=60,max=210,w=10)
ruf50.rlp <- rlp(reg.d$loga,reg.d$b,min=60,max=210,w=10,probs=0.5)

ruf75.emp <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
                 n.cutoff=4,cutoff.tail=TRUE)
ruf50.emp <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
                 n.cutoff=4,cutoff.tail=TRUE,probs=0.5)

ruf75nq.emp <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
                   n.cutoff=4,cutoff.tail=TRUE,quadratic=FALSE)
ruf50nq.emp <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
                   n.cutoff=4,cutoff.tail=TRUE,probs=0.5,quadratic=FALSE)

ruf.Froese <- FroeseWs(reg.d$loga,reg.d$b)
