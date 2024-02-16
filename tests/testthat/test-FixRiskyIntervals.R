
#for( lpPackage in c("lpSolve", "Rsymphony", "Rglpk", "highs"))
  
  
for( lpPackage in "Rsymphony")  
if (require(lpPackage, character.only = TRUE, quietly = TRUE)) {
  test_that("FixRiskyIntervals", {
    z3 <- SSBtoolsData("z3")
    upper <- z3$region %in% LETTERS
    z3$region[upper] <- paste0(z3$region[upper], 2)
    z3$region[!upper] <- paste0(toupper(z3$region[!upper]), 1)
    set.seed(123)
    z3$value <- rnorm(nrow(z3))^2
    set.seed(123)
    s <- sample.int(nrow(z3), size = 400)
    f <- ~(region + fylke) * mnd2 + kostragr * hovedint * mnd
    b <- SuppressDominantCells(z3[s, ], numVar = "value", n = 1:2, k = c(70, 95), 
                               formula = f, lpPackage =  lpPackage , 
                               output = GaussSuppression:::OutputFixRiskyIntervals,
                               rangePercent = 200,  
                               rangeMin = 1)
    expect_equal(as.vector(table(b$suppressed_integer)), c(130L, 39L, 33L, 37L))
  })
} 
