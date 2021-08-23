
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
  a <- GaussSuppressionFromData(z3, c(1:6), 7, x = mm$modelMatrix , crossTable = mm$crossTable, printInc <- FALSE, maxN = 5, printInc = printInc)
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
