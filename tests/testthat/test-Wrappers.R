
printInc <- FALSE

test_that("Wrappers", {
  dataset <- SSBtoolsData("magnitude1")
  dataset$seq2 <- (1:nrow(dataset)-9.9)^2
  
  # Table 3 in vignette  
  a1 <- SuppressFewContributors(data=dataset, 
                          numVar = "value", 
                          dimVar= c("sector4", "geo"), 
                          maxN=1)
  
  a2 <- SuppressFewContributors(data=dataset, 
                                numVar = c("seq2", "value"),  
                                dimVar= c("sector4", "geo"), 
                                maxN=1,
                                candidatesVar = "value")
  
  expect_identical(a1[names(a1)], a2[names(a1)])
  
  
  
  a3 <- SuppressFewContributors(data=dataset, 
                                numVar = c("seq2", "value"),  
                                dimVar= c("sector4", "geo"), 
                                maxN=1,
                                candidatesVar = "seq2")
  
  expect_false(all(a1$suppressed == a3$suppressed))
  
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
})
