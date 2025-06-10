#' @name parameter_linkedGauss
#' 
#' @title About the linkedGauss parameter
#' 
#' @description 
#' Possible non-NULL values of `linkedGauss` are `"global"`, `"local"`, `"consistent"` and `"back-tracking"`.
#' Associated parameters that can be set are `recordAware` and the two  
#' back-tracking parameters `sequential` and `iterBackTracking`.
#' 
#' 
#' @examples
#' 
#' for (linkedGauss in c("global", "local", "consistent", "back-tracking")) {
#'    cat("\n======================  linkedGauss = ", linkedGauss, "=================\n")
#'    output <- SuppressDominantCells(SSBtoolsData("magnitude1"), 
#'                formula = list(table_1 = ~(geo + eu) * sector2, 
#'                               table_2 = ~eu:sector4 - 1,
#'                               table_3 = ~(geo + eu) + sector4 - 1), 
#'                dominanceVar = "value", pPercent = 10, contributorVar = "company", 
#'                singletonMethod = "none", linkedGauss = linkedGauss)
#'    print(output)
#'    cat("#suppressed = ", sum(output$suppressed), "\n")
#' }
NULL