#' @name parameter_linkedGauss
#' 
#' @title About the linkedGauss parameter
#' 
#' @examples
#' 
#' for (linkedGauss in c("global", "consistent", "local")) {
#'    cat("\n======================  linkedGauss = ", linkedGauss, "=================\n")
#'    output <- tables_by_formulas(SSBtoolsData("magnitude1"),
#'                    table_fun = SuppressDominantCells, 
#'                    table_formulas = list(table_1 = ~region * sector2, 
#'                                         table_2 = ~region1:sector4 - 1, 
#'                                         table_3 = ~region + sector4 - 1), 
#'                    substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
#'                    collapse_vars = list(sector = c("sector2", "sector4")), 
#'                    dominanceVar  = "value", pPercent = 10, contributorVar = "company",
#'                    singletonMethod = "none", linkedGauss = linkedGauss)     
#'    print(output)  
#'    cat("#suppressed = ", sum(output$suppressed), "\n")
#' }
NULL