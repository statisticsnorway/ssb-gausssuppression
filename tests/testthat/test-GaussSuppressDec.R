
test_that("GaussSuppressDec and more", {
  set.seed(123)
  z2 <- SSBtoolsData("z2")
  z2$y1 <- runif(nrow(z2))
  z2$y2 <- runif(nrow(z2))
  printInc <- FALSE
  
  # Error here if overlapping freqVar, numVar, weightVar not treated correctly.
  a <- GaussSuppressDec(z2, dimVar = c("region", "fylke", "kostragr", "hovedint"), 
                        freqVar = "ant", protectZeros = FALSE, maxN = 2, 
                        numVar = c("y1", "y2", "ant"), 
                        weightVar = "y1", printInc = printInc)
  
  # Recalculate suppression from decimals. "kostragr" not included. 
  b <- SuppressionFromDecimals(a[a$isInner, ], 
                               hierarchies = SSBtools::FindDimLists(z2[c("region", "fylke", "hovedint")]), 
                               freqVar = "ant", decVar = "freqDec", printInc = printInc)
  
  # Special case where all suppressions found in suppressedData.   
  d <- AdditionalSuppression(z2, suppressedData = a, dimVar = c("region", "fylke", "hovedint"), 
                             freqVar = "ant", maxN = 20, singleton = NULL, printInc = printInc)
  
  # Check that b and d are identical after sorting 
  expect_identical(range(diff(sort(SSBtools::Match(b[names(d)], d)))), c(1L, 1L))
  
})
