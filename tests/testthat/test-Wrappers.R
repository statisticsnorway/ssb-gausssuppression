
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
  
  #k2 <- SDC(removeCodes = c("B", "B2"), 
  #          contributorVar = "company2",
  #          removeCodesFraction = NULL)
  
  k3 <- SDC(removeCodes = c("B", "B2"), 
            contributorVar = "company2",
            removeCodesFraction = c(1, 1))

  expect_identical(k0, k1)
  #expect_identical(k0, k2)
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
  
  
  #k9 <- SDC(removeCodes = c(3, 10:15), 
  #          removeCodesFraction = NULL)
  
  #k10 <- SDC(removeCodes = c(3, 10:15),
  #           removeCodesFraction = rep(1, 7))
  
  #expect_identical(k9,k10)
  
  
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
  
  
})
