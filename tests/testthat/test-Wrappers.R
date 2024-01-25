
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
  
  
  
  
})
