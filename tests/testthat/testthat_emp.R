
## Test Messages


## Test Output Types


## Validate Results
test_that("emp() match Gerow's Excel Tool results for Walleye", {
## Walleye Ws equation for comparison to Gerow's Excel Tool
##   Gerow's results were -- -4.624269, 2.477718, and 0.1461490 for the intercept,
##     linear term, and quadratic term for 75th percentile Ws equation
##   Gerow's results were -- -4.950281, 2.698470, and 0.1052352 for the intercept,
##     linear term, and quadratic term for 50th percentile Ws equation
  data(WalleyeGerowLW)
  # compare to Ws75 results
  wae1 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE)
  coef(wae1)
  # It appears that a difference from Gerow's work is that the quantiles computed
  #   here are different than his quantiles.  This can be seen by comparing the
  #   regdata results with the results in his 'summarized' worksheet.  From
  #   Gerow et al. (2005) it appears that he used 'qtype=9'; however, 'qtype=5'
  #   provides the closest values to his Excel worksheet.
  #
  # Thus for moving forward, comparisons will be made to our results ...
  #   WS75 --> -4.5846122, 2.4462199, 0.1521918
  #   WS50 --> -4.9013462, 2.6566370, 0.1138445

  wae75 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE)
  res <- coef(wae75)
  expect_equal(res[["(Intercept)"]],-4.5846122,tolerance=0.00000001)
  expect_equal(res[["logmidpt"]],2.4462199,tolerance=0.0000001)
  expect_equal(res[["I(logmidpt^2)"]],0.1521918,tolerance=0.0000001)

  wae50 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE,probs=0.5)
  res <- coef(wae50)
  expect_equal(res[["(Intercept)"]],-4.9013462,tolerance=0.00000001)
  expect_equal(res[["logmidpt"]],2.6566370,tolerance=0.00000001)
  expect_equal(res[["I(logmidpt^2)"]],0.1138445,tolerance=0.0000001)
})

test_that("emp() match Ogle & Winfield (2009) results for Ruffe", {
  source_test_helpers()
  
  ## Compare 75th percentile results to those in Table 2 of Ogle & Winfield
  res <- coef(ruf75)
  ## Compare sample sizes to that in Table 1 of Ogle & Winfield
  expect_equal(as.vector(ruf75$pop.by.len),
               c(63,69,84,88,84,84,77,60,48,31,24,22,12,8,4,3,1))
  expect_equal(as.vector(ruf75$ind.by.len),
               c(1317,1564,2111,2326,2041,1605,1367,969,634,434,206,152,65,29,9,3,1))
  expect_equal(res[["(Intercept)"]],-2.5800,tolerance=0.0001)
  expect_equal(res[["logmidpt"]],0.6210,tolerance=0.0001)
  expect_equal(res[["I(logmidpt^2)"]],0.6073,tolerance=0.0001)
  
  ## Compare 50th percentile results to those in Table 2 of Ogle & Winfield
  res <- coef(ruf50)
  expect_equal(res[["(Intercept)"]],-3.3524,tolerance=0.0001)
  expect_equal(res[["logmidpt"]],1.3969,tolerance=0.0001)
  expect_equal(res[["I(logmidpt^2)"]],0.4054,tolerance=0.0001)
  
  ## Compare 75th percentile (no quad) results to those in Table 2 of Ogle & Winfield
  res <- coef(ruf75nq)
  expect_equal(res[["(Intercept)"]],-5.0206,tolerance=0.0001)
  expect_equal(res[["logmidpt"]],3.0612,tolerance=0.0001)

  ## Compare 50th percentile (no quad) results to those in Table 2 of Ogle & Winfield
  res <- coef(ruf50nq)
  expect_equal(res[["(Intercept)"]],-4.9818,tolerance=0.0001)
  expect_equal(res[["logmidpt"]],3.0259,tolerance=0.0001)
})