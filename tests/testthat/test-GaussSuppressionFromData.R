
printInc <- FALSE

test_that("GaussSuppressionFromData works", {
  expect_equal(which(GaussSuppressionFromData(SSBtoolsData("z1"), 1:2, 3, printInc = printInc)$suppressed), c(12, 13, 22, 23, 42, 43))
})


# Sample with seed inside test_that do not work
z3 <- SSBtoolsData("z3")
mm <- SSBtools::ModelMatrix(z3[, 1:6], crossTable = TRUE, sparse = FALSE)
x <- mm$modelMatrix  
k <- 1:20000
set.seed(123)
sample_k <- sample(k)
x[k] <- x[sample_k]


test_that("Advanced with integer overflow", {
  skip("Strange behaviour. Test works, but not when run inside Check package")
  
  a <- GaussSuppressionFromData(z3, c(1:6), 7, x = mm$modelMatrix , crossTable = mm$crossTable, maxN = 5, printInc = printInc)
  expect_identical(sum(which(a$suppressed)), 599685L)
  
  # This test involves integer overflow in AnyProportionalGaussInt  
  a <- GaussSuppressionFromData(z3, c(1:6), 7, x = x, crossTable = mm$crossTable, printInc = printInc)
  expect_identical(sum(which(a$suppressed)), 525957L)
  
  # This test involves integer overflow in AnyProportionalGaussInt  
  a <- GaussSuppressionFromData(z3, c(1:6), 7, x = x, crossTable = mm$crossTable, protectZeros = FALSE, secondaryZeros = TRUE, printInc = printInc)
  expect_identical(sum(which(a$suppressed)), 411693L)
  
  # This test involves all ways of updating A$r[[i]], A$x[[i]], B$r[[i]], B$x[[i]]  (Including integer overflow)
  a <- GaussSuppressionFromData(z3, c(1:6), 7, x = x, crossTable = mm$crossTable, protectZeros = FALSE, secondaryZeros = TRUE, testMaxInt = 10, printInc = printInc)
  expect_identical(sum(which(a$suppressed)), 411693L)
  
  a <- GaussSuppressionFromData(z3, c(1:6), 7, x = x, crossTable = mm$crossTable, protectZeros = FALSE, secondaryZeros = TRUE, allNumeric = TRUE, printInc = printInc)
  expect_identical(sum(which(a$suppressed)), 411693L)
  
  # This test involves TRUE return in AnyProportionalGaussInt after ReduceGreatestDivisor (identical length 3 vectors)
  x[, 201:300] <- round(0.6 * x[, 201:300] + 0.6 * x[, 301:400])
  a <- GaussSuppressionFromData(z3, c(1:6), 7, x = x, crossTable = mm$crossTable, printInc = printInc)
  expect_identical(sum(which(a$suppressed)), 576555L)
  
})


test_that("structuralEmpty and removeEmpty", {
  expect_warning(a1 <- GaussSuppressionFromData(z3[100:300, ], 1:6, 7, printInc = printInc))
  a2 <- GaussSuppressionFromData(z3[100:300, ], 1:6, 7, printInc = printInc, structuralEmpty = TRUE)
  a3 <- GaussSuppressionFromData(z3[100:300, ], 1:6, 7, printInc = printInc, removeEmpty = TRUE)
  k <- a1$suppressed != a2$suppressed
  expect_equal(a1[!k, ], a3)
  expect_equal(a2[!k, ], a3)
  expect_equal(unique(a1[k, "freq"]), 0)
})



test_that("extend0 and various hierarchy input", {
  z2 <- SSBtoolsData("z2")
  dimLists <- SSBtools::FindDimLists(z2[, -5])
  hi <- list(c("region", "fylke", "kostragr"), hovedint = dimLists$hovedint)
  
  a1 <- GaussSuppressionFromData(z2, 1:4, 5, printInc = printInc)
  a2 <- GaussSuppressionFromData(z2, freqVar = "ant", hierarchies = dimLists, printInc = printInc)
  a3 <- GaussSuppressionFromData(z2, freqVar = "ant", hierarchies = hi, printInc = printInc)
  
  expect_identical(a1, a2)
  expect_identical(a3, a2)
  
  z2_ <- z2[z2$ant != 0, ]
  
  a1 <- GaussSuppressionFromData(z2_, 1:4, 5, extend0 = TRUE, output = "publish_inner", printInc = printInc)
  
  expect_identical(a1$publish, a2)
  
  a2 <- GaussSuppressionFromData(z2_, freqVar = "ant", hierarchies = dimLists, extend0 = TRUE, output = "publish_inner", printInc = printInc)
  a3 <- GaussSuppressionFromData(z2_, freqVar = "ant", hierarchies = hi, extend0 = TRUE, output = "publish_inner", printInc = printInc)
  
  if (FALSE) { # Include code that shows differences 
    tail(a1$inner)
    tail(a2$inner)
    tail(a3$inner)
  }
  
  expect_identical(a1$publish, a2$publish)
  expect_identical(a3$publish, a2$publish)
  
  expect_equal(a1$inner[names(a2$inner)], a2$inner, ignore_attr = TRUE)
  expect_equal(a3$inner[names(a2$inner)], a2$inner, ignore_attr = TRUE)
  
  a1_ <- GaussSuppressionFromData(z2_, 1:4, 5, extend0 = "all", output = "publish_inner", printInc = printInc)
  a2_ <- GaussSuppressionFromData(z2_, freqVar = "ant", hierarchies = dimLists, extend0 = "all", output = "publish_inner", printInc = printInc)
  a3_ <- GaussSuppressionFromData(z2_, freqVar = "ant", hierarchies = hi, extend0 = "all", output = "publish_inner", printInc = printInc)
  
  expect_identical(a1, a1_)
  expect_identical(a2, a2_)
  expect_identical(a3, a3_)
  
  z2__ <- z2_[z2_$hovedint != "trygd", ]
  
  a2 <- GaussSuppressionFromData(z2__, freqVar = "ant", hierarchies = dimLists, extend0 = "all", output = "publish_inner", printInc = printInc)
  a3 <- GaussSuppressionFromData(z2__, freqVar = "ant", hierarchies = hi, extend0 = "all", output = "publish_inner", printInc = printInc)
  
  expect_identical(a3$publish, a2$publish)
  expect_equal(a3$inner[names(a2$inner)], a2$inner, ignore_attr = TRUE)
  
  expect_identical(lapply(c(a2, a3), dim), lapply(c(a2_, a3_), dim))
  
  z2___ <- z2__[z2__$fylke != 10, ]
  
  a2_ <- GaussSuppressionFromData(z2___, freqVar = "ant", hierarchies = dimLists, extend0 = "all", output = "publish_inner", printInc = printInc)
  a3_ <- GaussSuppressionFromData(z2___, freqVar = "ant", hierarchies = hi, extend0 = "all", output = "publish_inner", printInc = printInc)
  
  expect_identical(lapply(a2, dim), lapply(a2_, dim))
  
  expect_true(nrow(a3_$inner) < nrow(a3$inner))
  expect_true(nrow(a3_$publish) < nrow(a3$publish))
})

