test_that("FixRiskyIntervals", {
  lpPackage <- "Rglpk"
  skip_if_not_installed(lpPackage)
  if (require(lpPackage, character.only = TRUE, quietly = TRUE)) {
    z3 <- SSBtoolsData("z3")
    upper <- z3$region %in% LETTERS
    z3$region[upper] <- paste0(z3$region[upper], 2)
    z3$region[!upper] <- paste0(toupper(z3$region[!upper]), 1)
    set.seed(123)
    z3$value <- rnorm(nrow(z3))^2
    set.seed(123)
    s <- sample.int(nrow(z3), size = 400)
    f <- ~(region + fylke) * mnd2 + kostragr * hovedint * mnd
    co <- capture.output({
      b <- SuppressDominantCells(z3[s, ], numVar = "value", n = 1:2, k = c(70, 95), 
                                 formula = f, lpPackage =  lpPackage , 
                                 rangePercent = 50,  
                                 rangeMin = 1)
    })
    expect_equal(as.vector(table(b$suppressed_integer)), c(164L, 39L, 33L, 3L))
  }
})
