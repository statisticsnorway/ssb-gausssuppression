

TestTwoWay = function(data, freqVar,  hierarchies, printInc = FALSE,  ...){
  data$en <- 1
  out    <- GaussSuppressionFromData(data, freqVar = freqVar, hierarchies = hierarchies, printInc = printInc, ...)
  
  outCol <- GaussSuppressionTwoWay(data, freqVar = freqVar, hierarchies = c(hierarchies, en = "rowFactor"), colVar = unique(names(hierarchies)), printInc = printInc, ...)
  outRow <- GaussSuppressionTwoWay(data, freqVar = freqVar, hierarchies = c(hierarchies, en = "rowFactor"), colVar = "en", printInc = printInc, ...)

  maCol <- SSBtools::Match(out, outCol[, names(out)])
  maRow <- SSBtools::Match(out, outRow[, names(out)])
  
  c(sum(is.na(maCol)), sum(is.na(maRow)))
}


test_that("Ok GaussSuppressionTwoWay special cases", {
  z <- SSBtools::SSBtoolsData("z3")
  z <- z[z$kostragr %in% c("100","400"), ]
  z$en <- 1
  
  dimListsA <- SSBtools::FindDimLists(z[, 1:6])
  
  set.seed(123)
  
  z <- z[sample(nrow(z),50),]
  
  a <- TestTwoWay(z, freqVar = "ant", hierarchies = dimListsA, removeEmpty = FALSE, 
                  singletonMethod = "anySum", protectZeros = FALSE,  
                  hidden = function(freq, ...){freq==10},
                  forced = function(freq, ...){freq==14}, 
                  forcedInOutput = FALSE)
  
  expect_identical(a, c(0L, 0L))
  
})
