
printInc <- FALSE

test_that("Wrappers", {
  dataset <- SSBtoolsData("magnitude1")
  dataset$seq2 <- (1:nrow(dataset)-10)^2
  
  # Table 3 in vignette  
  a1 <- SuppressFewContributors(data=dataset, 
                          numVar = "value", 
                          dimVar= c("sector4", "geo"), 
                          maxN=1,
                          printInc = printInc)
  
  a2 <- SuppressFewContributors(data=dataset, 
                                numVar = c("seq2", "value"),  
                                dimVar = c("sector4", "geo"), 
                                maxN=1,
                                remove0 = "value",
                                candidatesVar = "value",
                                printInc = printInc)
  
  expect_identical(a1[names(a1)], a2[names(a1)])
  
  
  
  a3 <- SuppressFewContributors(data=dataset, 
                                numVar = c("seq2", "value"),  
                                dimVar= c("sector4", "geo"), 
                                maxN=1,
                                remove0 = "value",
                                candidatesVar = "seq2",
                                printInc = printInc)
  
  expect_false(all(a1$suppressed == a3$suppressed))
  
  a4 <- SuppressFewContributors(data=dataset, 
                                dimVar = c("sector4", "geo"), 
                                maxN=1,
                                remove0 = "seq2",
                                candidatesVar = "value",
                                printInc = printInc)
  
  
  expect_false(all(a1$nAll == a4$nAll))
  
  expect_identical(a1[c("primary", "suppressed")], a4[c("primary", "suppressed")])
  
  
  # A test of removeCodes in CandidatesNum with multiple charVar
  dataset$char2 <- dataset$company
  dataset$char2[7:15] <- "a"
  a5 <- SuppressFewContributors(data=dataset, 
                                dimVar = c("sector4", "geo"), 
                                maxN=1,
                                numVar = "value",
                                contributorVar = c("company", "char2"),
                                removeCodes = list(company = c("B"), 
                                                   char2 = c("B","a")),
                                printInc = printInc)
  
  # FALSE when removeCodesForCandidates = FALSE
  expect_false(a5[a5$sector4 == "Entertainment" & a5$geo == "Iceland", "suppressed"])
  
  
  
  # Table 3 in vignette  
  b1 <- SuppressDominantCells(data=dataset, 
                        numVar = "value", 
                        dimVar= c("sector4", "geo"), 
                        n = 1, k = 80, allDominance = TRUE,
                        printInc = printInc)
  
  b2 <- SuppressDominantCells(data=dataset, 
                              numVar = c("seq2", "value"), 
                              dimVar= c("sector4", "geo"), 
                              n = 1, k = 80, allDominance = TRUE,
                              candidatesVar = "value",
                              dominanceVar = "value",
                              printInc = printInc)
  
  expect_identical(b1[names(b1)], b2[names(b1)])
  
  
  b3 <- SuppressDominantCells(data=dataset, 
                              numVar = c("seq2", "value"), 
                              dimVar= c("sector4", "geo"), 
                              n = 1, k = 80, allDominance = TRUE,
                              candidatesVar = "seq2",
                              dominanceVar = "value",
                              printInc = printInc)
  
  expect_false(all(b1$suppressed == b3$suppressed))
  
  
  dataset$value2 <- dataset$value
  dataset$value2[dataset$sector4 == "Governmental"] <- 0
  dataset$value2[dataset$sector4 == "Agriculture"] <- 0
  b4 <- SuppressDominantCells(data=dataset, 
                              numVar = "value2", 
                              dimVar= c("sector4", "geo"), 
                              n = 1, k = 70, allDominance = TRUE,
                              singletonZeros = TRUE,
                              printInc = printInc)
  expect_true(b4[b4$sector4 == "Governmental" & b4$geo == "Total", "suppressed"])
  # With singletonZeros = FALSE, the result is FALSE 
  # and revealing suppressed 0 cells is easy since Total=0  
  
  # If this is 12L, see code in max_contribution where do_drop0 = FALSE 
  # Then the counting of contributors that includes 0s must be done differently
  expect_identical(max(b4$n_contr), 20L)
  
  dataset$company2 <- dataset$company 
  dataset$company2[2] <- "B2"
  
  
  SDC <- function( ...) {
    SuppressDominantCells(data=dataset, 
                          numVar = c("seq2", "value"), 
                          dimVar= c("sector4", "geo"), 
                          k = c(70, 80), allDominance = TRUE,
                          singletonMethod = "none",
                          candidatesVar = "value",
                          dominanceVar = "value",
                          printInc = printInc, 
                          ...)
    
  }
  
  k <- SDC(contributorVar = "company")
  
  
  k0 <- SDC(removeCodes = "B", 
            contributorVar = "company")
  
  k1 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2")
  
  k3 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2",
            removeCodesFraction = c(1, 1))

  expect_identical(k0, k1)
  expect_identical(k0, k3)

  
  k4 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2",
            removeCodesFraction = 0)
  
  k5 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2",
            removeCodesFraction = c(0,0))
  
  expect_identical(k4,k5)
  expect_equal(sum(abs(k4[6] - k[6])< 0.001), 11)
  
  
  k6 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2",
            removeCodesFraction = c(0,1))
  
  B2impact <- unique(as.vector(as.matrix(k4[which((1/k4[6]-  1/k6[6]) > 0.001), 1:2])))
  expect_setequal(B2impact, c("Total", "Agriculture", "Portugal"))
  
  AgriculturePortugal <- which(k1$sector4 == "Agriculture" & k1$geo == "Portugal")
  expect_equal(k1[AgriculturePortugal, ], k6[AgriculturePortugal, ])
  
  
  k7 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2",
            removeCodesFraction = 0.45)
  
  k8 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2",
            removeCodesFraction = c(0.45,0.45))
  
  expect_identical(k7,k8)
  
  ranges <- c(range((1/k4[6]-  1/k7[6])/(1/k4[6]-  1/k1[6]), na.rm = TRUE),
              range((1/k4[7]-  1/k7[7])/(1/k4[7]-  1/k1[7]), na.rm = TRUE))
  
  expect_equal(ranges, rep(0.45, 4))
  
  
  expect_warning({k11 <- SDC(contributorVar = "company", 
                             protectZeros = TRUE, 
                             removeCodes = c("A", "B", "C"))})
  
  k12 <- SDC(contributorVar = "company", 
             protectZeros = TRUE, 
             removeCodes = c("A", "B", "C"),
             structuralEmpty = TRUE)
  
  expect_equal(sum(k11$primary), nrow(k11))

  
  expect_equal(sum(k12$primary), sum(k12[6]))
  
  expect_warning({k13 <- SDC(protectZeros = TRUE, 
                  removeCodes = 3:4)})
  
  k14 <- SDC(protectZeros = TRUE, 
             removeCodes = 3:4,
             structuralEmpty = TRUE)

  expect_true(all(k13$primary[k13[6]==0]))
  expect_true(!all(k14$primary[k14[6]==0]))
  
  
  #############################################
  #  With negative values and several 0's 
  ############################################
  
  dataset$y <- dataset$value * rep_len(c(0, -1, 1, -1, 1, 1, 0), 20)
  dataset$w <- 1:nrow(dataset)
  dataset$f_company2 <- factor(dataset$company2)
  dataset$i_company2 <- as.integer(dataset$f_company2)
  
  
  # max abs sums
  asum_n <- function(x, n) sum(sort(x, decreasing = TRUE)[seq_len(min(n, length(x)))])
  asum1 <- function(x) asum_n(abs(x), 1)
  asum2 <- function(x) asum_n(abs(x), 2)
  
  which.max2 <- function(x) {
    x[which.max(x)] <- NA
    which.max(x)
  }
  
  fraction <- c(0.3, 0.7)
  
  f <- function(g, y, w, fr) { 
    a <- aggregate(list(y = y, wy = y * w), list(g = g), sum)
    a2r <- a[!(a[, 1] %in% c(2, 3)), 2]
    a3r <- a[!(a[, 1] %in% c(2, 3)), 3]
    a2na <- a[, 2]
    a2na[a[, 1] %in% c(2, 3)] <- NA
    
    fra <- rep(1, nrow(a))
    fra[a[, 1] == 2] <- 1 - fraction[1]
    fra[a[, 1] == 3] <- 1 - fraction[2]
    
    c( sum1 <- asum1(a[,2]),
       sum2 <- asum2(a[,2]),
       sum1r <- asum1(a2r),
       sum2r <- asum2(a2r),
       sum = sum(abs(a[,2])),
       wsum = sum(abs(a[,3])),
       sumr = sum(abs(a2r)),
       wsumr = sum(abs(a3r)),
       fsum = sum(fra*abs(a[,2])),
       fwsum = sum(fra*abs(a[,3])),
       max1 = c(a[which.max(abs(a[,2])),1], NA)[1],
       max2 = c(a[which.max2(abs(a[,2])),1], NA)[1],
       max1r = c(a[which.max(abs(a2na)),1], NA)[1],
       max2r = c(a[which.max2(abs(a2na)),1], NA)[1],
       n  =  sum(!is.na(a2na)),  
       n0 =  sum(!is.na(a2na)  & a[,2] == 0)
       )
  }

  
  # Calculate everything in a different way via model_aggregate()
  k <- SSBtools::model_aggregate(dataset, 
                                 formula = ~sector4 * eu + geo * sector2, 
      fun_vars = list(`sum1,sum2,sum1r,sum2r,sum,wsum,sumr,wsumr,fsum,fwsum,max1,max2,max1r,max2r,n,n0` = list(f = c("i_company2", "y", "w"))),
      fun = c(f = f)
      )
  
  for (var in c("max1", "max2", "max1r", "max2r")) {
    k[[var]] <- levels(dataset$f_company2)[k[[var]]]
  }
  
  SDC2 <- function( ...) {
    SuppressDominantCells(data=dataset, 
                          contributorVar = "company2",
                          formula = ~sector4*eu + geo*sector2, 
                          k = c(95, 99), allDominance = TRUE,
                          singletonMethod = "none",
                          dominanceVar = "y",
                          printInc = printInc, 
                          ...)
    
  }
  
  g0 <- SDC2()
  g1 <- SDC2(sWeightVar = "w")
  g2 <- SDC2(removeCodes = c("B", "B2"))
  g3 <- SDC2(removeCodes = c("B", "B2"), sWeightVar = "w")
  g4 <- SDC2(removeCodes = c("B", "B2"), removeCodesFraction = fraction)
  g5 <- SDC2(removeCodes = c("B", "B2"), sWeightVar = "w", removeCodesFraction = fraction)

  expect_equal(g0[["max1contributor"]], k[["max1"]])
  expect_equal(g0[["max2contributor"]], k[["max2"]])
  expect_equal(g1[["max1contributor"]], k[["max1"]])
  expect_equal(g1[["max2contributor"]], k[["max2"]])
  expect_equal(g2[["max1contributor"]], k[["max1r"]])
  expect_equal(g2[["max2contributor"]], k[["max2r"]])
  expect_equal(g3[["max1contributor"]], k[["max1r"]])
  expect_equal(g3[["max2contributor"]], k[["max2r"]])
  expect_equal(g4[["max1contributor"]], k[["max1r"]])
  expect_equal(g4[["max2contributor"]], k[["max2r"]])
  expect_equal(g5[["max1contributor"]], k[["max1r"]])
  expect_equal(g5[["max2contributor"]], k[["max2r"]])
  
  
  expect_equal(g0[["dominant1"]], k[["sum1"]]/k[["sum"]])
  expect_equal(g0[["dominant2"]], k[["sum2"]]/k[["sum"]])
  expect_equal(g1[["dominant1"]], k[["sum1"]]/k[["wsum"]])
  expect_equal(g1[["dominant2"]], k[["sum2"]]/k[["wsum"]])
  expect_equal(g2[["dominant1"]], k[["sum1r"]]/(k[["sumr"]]+1e-100))
  expect_equal(g2[["dominant2"]], k[["sum2r"]]/(k[["sumr"]]+1e-100))
  expect_equal(g3[["dominant1"]], k[["sum1r"]]/(k[["wsumr"]]+1e-100))
  expect_equal(g3[["dominant2"]], k[["sum2r"]]/(k[["wsumr"]]+1e-100))
  expect_equal(g4[["dominant1"]], k[["sum1r"]]/(k[["fsum"]]+1e-100))
  expect_equal(g4[["dominant2"]], k[["sum2r"]]/(k[["fsum"]]+1e-100))
  expect_equal(g5[["dominant1"]], k[["sum1r"]]/(k[["fwsum"]]+1e-100))
  expect_equal(g5[["dominant2"]], k[["sum2r"]]/(k[["fwsum"]]+1e-100))
  
  expect_equal(g2[["n_contr"]], k[["n"]])
  expect_equal(g2[["n_non0_contr"]], k[["n"]] - k[["n0"]])
  expect_equal(g3[["n_contr"]], k[["n"]])
  expect_equal(g3[["n_non0_contr"]], k[["n"]] - k[["n0"]])
  expect_equal(g4[["n_contr"]], k[["n"]])
  expect_equal(g4[["n_non0_contr"]], k[["n"]] - k[["n0"]])
  expect_equal(g5[["n_contr"]], k[["n"]])
  expect_equal(g5[["n_non0_contr"]], k[["n"]] - k[["n0"]])
  
})
