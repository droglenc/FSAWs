
## Test Messages


## Test Output Types


## Validate Results
test_that("rlp() match Murphy et al. (1990) results for Largemouth Bass", {
  data(LMBassWs)
  lmb.rlp <- rlp(LMBassWs$log.a,LMBassWs$b,155,815,qtype=6)
  res <- coef(lmb.rlp)
  ## Both results are off by 1 unit in the thousandths place
  expect_equal(round(res[["(Intercept)"]],3)-0.001,-5.379)
  expect_equal(round(res[["logmidpt"]],3)+0.001,3.221)
})

test_that("rlp() match Murphy et al. (1990) results for Bluegill", {
  data(BluegillWs)
  bg.rlp <- rlp(BluegillWs$log.a,BluegillWs$b,75,395,qtype=6)
  res <- coef(bg.rlp)
  ## Both results are off by 1 or 2 units in the thousandths place
  expect_equal(round(res[["(Intercept)"]],3)+0.002,-5.385)
  expect_equal(round(res[["logmidpt"]],3)-0.001,3.318)
})

test_that("rlp() match Murphy et al. (1990) results for Walleye", {
  data(WalleyeWs)
  wae.rlp <- rlp(WalleyeWs$log.a,WalleyeWs$b,155,1045,qtype=6)
  res <- coef(wae.rlp)
  ## Both results are off 1 to 3 units in the hundredths place
  expect_equal(round(res[["(Intercept)"]],3)+0.034,-5.453)
  expect_equal(round(res[["logmidpt"]],3)-0.013,3.180)
})

test_that("rlp() match Ogle & Winfield (2009) results for Ruffe", {
  source_test_helpers()
  
  ## 75th percenile
  res <- coef(ruf75.rlp)
  ## Both results are equal
  expect_equal(res[["(Intercept)"]],-4.9588,tolerance=0.00001)
  expect_equal(res[["logmidpt"]],3.0286,tolerance=0.00001)

  ## 50th percenile
  res <- coef(ruf50.rlp)
  ## Both results are equal
  expect_equal(res[["(Intercept)"]],-4.9573,tolerance=0.00001)
  expect_equal(res[["logmidpt"]],3.0154,tolerance=0.00001)
})
