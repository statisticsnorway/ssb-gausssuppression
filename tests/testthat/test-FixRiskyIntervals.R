test_that("FixRiskyIntervals", {
  lpPackage <- "Rglpk"
  skip_if_not_installed(lpPackage)
  if (requireNamespace(lpPackage, quietly = TRUE)) {
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
    
    
    f1 <- ~(age_l + age_m) * (lms_l) 
    f2 <- ~(age_l + age_m) * (hst_l + hst_m) 
    f3 <- ~(age_l + age_m) * (fst_l + fst_m) 
    f4 <- ~(lms_l + lms_h) * (hst_l + hst_m)
    d53 <- readRDS(testthat::test_path("testdata", "d53.rds"))
    z <- SSBtools::MakeMicro(d53, "freq")
    z <- z[z$sex == 2, ]
    set.seed(123)
    z$char <- sample(paste0("char", seq_len(nrow(z)/2)), nrow(z), replace = TRUE)
    z$value <- abs(rnorm(nrow(z)))^(2)
    
    out = capture.output({a = SuppressDominantCells(data = z,
                                   dominanceVar = "value",
                                   contributorVar = "char",
                                   formula = list(list(formula = f1),
                                                  list(formula = f2),
                                                  list(formula = f3),
                                                  list(formula = f4)), 
                                   pPercent = 50, 
                                   loProtectionLimit = 2, 
                                   rangeMin =3,
                                   protectionIntervals = TRUE,
                                   lpPackage = "lpSolve")})
    
    
    cout <- c("141: 1 new, (278.8) 129-136+130+", 
              "129: 2 new, (256.6) 128+103+102+101+1-52-77-90+78-85+", 
              "78: 3 new, (120.3) 77+53-66-72-75+73-74-", 
              "74: 4 new, (112.3) 73+72+67+66+54+53+52+2-28-41+", 
              "35: 5 new, (44.03) 34-", 
              "34: 6 new, (42.78) 31+30+29-", 
              "29: 7 new, (34.37) 28+3-16-23-26+24+", 
              "23: 8 new, (26.67) 17+16+4+3+2-", 
              "2: 9 new, (3.530) 1-", 
              "1: 10 new, (2.487) 1+")
    
    expect_equal(trimws(out[grep("new",out)]), cout)
    
    expect_equal(as.vector(table(a$suppressed_integer)), 
                 c(243L, 103L, 135L, 10L, 6L))
    
  }
})

