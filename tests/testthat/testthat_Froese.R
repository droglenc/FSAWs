
## Test Messages


## Test Output Types


## Validate Results

test_that("FroeseWs() match Ogle & Winfield (2009) results for Ruffe", {
  source_test_helpers()
  
  res <- coef(ruf.Froese)
  ## Both results are equal (note that Ogle & Winfield report log10(a))
  expect_equal(log10(res[["gm.a"]]),-4.9416,tolerance=0.00001)
  expect_equal(res[["mn.b"]],3.0050,tolerance=0.00001)
})
