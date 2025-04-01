source_test_helpers()

## Test Messages


## Test Output Types


## Validate Results
test_that("wsValidate() match Ogle & Winfield (2009) results for Ruffe", {
  ## Compare 75th percentile RLP results to Table 2 of Ogle & Winfield
  ### Willis method
  res <- wsValidate(ruf75.rlp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                    type="Willis")
  tmp <- summary(res)
  expect_identical(tmp$statistic,c("number of successes"=15))
  expect_identical(tmp$parameter,c("number of trials"=20))
  expect_equal(tmp$p.value,c("Negative"=0.0414),tolerance=0.001)
  ### Weighted EmpQ method
  res <- wsValidate(ruf75.rlp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                    type="EmpQ",weighted=TRUE)
  tmp <- anova(res)
  expect_identical(tmp$Df,c(1L,1L,10L))
  expect_equal(tmp$`Pr(>F)`,c(0.3620,0.0034,NA),tolerance=0.001)
  
  ## Compare 50th percentile RLP results to Table 2 of Ogle & Winfield
  ### Willis method
  res <- wsValidate(ruf50.rlp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                    type="Willis")
  tmp <- summary(res)
  expect_identical(tmp$statistic,c("number of successes"=14))
  expect_identical(tmp$parameter,c("number of trials"=19))
  expect_equal(tmp$p.value,c("Negative"=0.0636),tolerance=0.001)
  ### Weighted EmpQ method
  res <- wsValidate(ruf50.rlp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                    type="EmpQ",weighted=TRUE,probs=0.5)
  tmp <- anova(res)
  expect_identical(tmp$Df,c(1L,1L,10L))
  expect_equal(tmp$`Pr(>F)`,c(0.2272,0.0750,NA),tolerance=0.0001)
  
  ## Compare 75th percentile results EMP results to Table 2 of Ogle & Winfield
  ### Willis method
  res <- wsValidate(ruf75.emp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                    type="Willis")
  tmp <- summary(res)
  expect_identical(tmp$statistic,c("number of successes"=15))
  expect_identical(tmp$parameter,c("number of trials"=22))
  expect_equal(tmp$p.value,c("Negative"=0.1338),tolerance=0.0001)
  ### Weighted EmpQ method
  res <- wsValidate(ruf75.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                    type="EmpQ",weighted=TRUE)
  tmp <- anova(res)
  expect_identical(tmp$Df,c(1L,1L,10L))
  expect_equal(tmp$`Pr(>F)`,c(0.4401,0.9174,NA),tolerance=0.0001)

  ## Compare 50th percentile results EMP results to Table 2 of Ogle & Winfield
  ### Willis method
  res <- wsValidate(ruf50.emp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                    type="Willis")
  tmp <- summary(res)
  expect_identical(tmp$statistic,c("number of successes"=12))
  expect_identical(tmp$parameter,c("number of trials"=20))
  expect_equal(tmp$p.value,c("Negative"=0.5034),tolerance=0.0001)
  ### Weighted EmpQ method
  res <- wsValidate(ruf50.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                    type="EmpQ",weighted=TRUE,probs=0.5)
  tmp <- anova(res)
  expect_identical(tmp$Df,c(1L,1L,10L))
  expect_equal(tmp$`Pr(>F)`,c(0.4838,0.9368,NA),tolerance=0.0001)
  
  ## Compare 75th percentile (no quad) EMP results to Table 2 of Ogle & Winfield
  res <- wsValidate(ruf75nq.emp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                    type="Willis")
  tmp <- summary(res)
  expect_identical(tmp$statistic,c("number of successes"=16))
  expect_identical(tmp$parameter,c("number of trials"=17))
  expect_equal(tmp$p.value,c("Negative"=0.0003),tolerance=0.001)
  ### Weighted EmpQ method
  res <- wsValidate(ruf75nq.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                    type="EmpQ",weighted=TRUE)
  tmp <- anova(res)
  expect_identical(tmp$Df,c(1L,1L,10L))
  expect_equal(tmp$`Pr(>F)`,c(0.5308,0.0024,NA),tolerance=0.0001)
  
  ## Compare 50th percentile (no quad) EMP results to Table 2 of Ogle & Winfield
  ### Willis method
  res <- wsValidate(ruf50nq.emp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                    type="Willis")
  tmp <- summary(res)
  expect_identical(tmp$statistic,c("number of successes"=14))
  expect_identical(tmp$parameter,c("number of trials"=19))
  expect_equal(tmp$p.value,c("Negative"=0.0636),tolerance=0.001)
  ### Weighted EmpQ method
  res <- wsValidate(ruf50nq.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                    type="EmpQ",weighted=TRUE,probs=0.5)
  tmp <- anova(res)
  expect_identical(tmp$Df,c(1L,1L,10L))
  expect_equal(tmp$`Pr(>F)`,c(0.3844,0.0685,NA),tolerance=0.001)
  
  ## Compare Froese results to Table 2 of Ogle & Winfield
  ### Willis method
  res <- wsValidate(ruf.Froese,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                    type="Willis")
  tmp <- summary(res)
  expect_identical(tmp$statistic,c("number of successes"=13))
  expect_identical(tmp$parameter,c("number of trials"=19))
  expect_equal(tmp$p.value,c("Negative"=0.1671),tolerance=0.001)
  ### Weighted EmpQ method
  res <- wsValidate(ruf.Froese,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                    type="EmpQ",weighted=TRUE,use.means=TRUE)
  tmp <- anova(res)
  expect_identical(tmp$Df,c(1L,1L,10L))
  expect_equal(tmp$`Pr(>F)`,c(0.4151,0.0011,NA),tolerance=0.0001)
})

test_that("wsValidate() using coefficients", {
  #===== RLP method
  res1 <- wsValidate(ruf75.rlp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                     type="Willis")
  res2 <- wsValidate(coef(ruf75.rlp),rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                     type="Willis")
  expect_identical(res1,res2)
  
  ## The object in call is different, so compared coef/anovas instead
  res1 <- wsValidate(ruf75.rlp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  res2 <- wsValidate(coef(ruf75.rlp),rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  expect_identical(coef(res1),coef(res2))
  expect_identical(anova(res1),anova(res2))
  
  
  ## Same as above, but entered coefficients rather than using coef()
  res1 <- wsValidate(ruf75.rlp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  res2 <- wsValidate(c(-4.958844,3.028595),rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  expect_equal(coef(res1),coef(res2),tolerance=0.00001)
  expect_equal(anova(res1),anova(res2),tolerance=0.0001)

  #===== EMP method (with quadratic regression)
  res1 <- wsValidate(ruf75.emp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                     type="Willis")
  res2 <- wsValidate(coef(ruf75.emp),rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                     type="Willis")
  expect_identical(res1,res2)
  
  ## The object in call is different, so compared coef/anovas instead
  res1 <- wsValidate(ruf75.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  res2 <- wsValidate(coef(ruf75.emp),rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  expect_identical(coef(res1),coef(res2))
  expect_identical(anova(res1),anova(res2))
  
  
  ## Same as above, but entered coefficients rather than using coef()
  res1 <- wsValidate(ruf75.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  res2 <- wsValidate(c(-2.5799710,0.6210216,0.6073453),rWs.v,"regrnum","tl","wt",
                     min=60,max=220,w=10,type="EmpQ",weighted=TRUE)
  expect_equal(coef(res1),coef(res2),tolerance=0.0001)
  expect_equal(anova(res1),anova(res2),tolerance=0.1)
  
  #===== EMP method (withOUT quadratic regression)
  res1 <- wsValidate(ruf75nq.emp,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                     type="Willis")
  res2 <- wsValidate(coef(ruf75nq.emp),rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
                     type="Willis")
  expect_identical(res1,res2)
  
  ## The object in call is different, so compared coef/anovas instead
  res1 <- wsValidate(ruf75nq.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  res2 <- wsValidate(coef(ruf75nq.emp),rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  expect_identical(coef(res1),coef(res2))
  expect_identical(anova(res1),anova(res2))
  
  
  ## Same as above, but entered coefficients rather than using coef()
  res1 <- wsValidate(ruf75nq.emp,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
                     type="EmpQ",weighted=TRUE)
  res2 <- wsValidate(c(-5.020624,3.061168),rWs.v,"regrnum","tl","wt",
                     min=60,max=220,w=10,type="EmpQ",weighted=TRUE)
  expect_equal(coef(res1),coef(res2),tolerance=0.000001)
  expect_equal(anova(res1),anova(res2),tolerance=0.00001)
})