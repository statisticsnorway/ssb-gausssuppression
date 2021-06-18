#' Suppression of directly-disclosive cells
#' 
#' Function for suppressing disclosive cells in frequency tables. The method
#' detects and primary suppresses directly-disclosive cells with the
#' \link[SSBtools]{FindDisclosiveCells} function, and applies a secondary suppression
#' using Gauss suppression (see \link{GaussSuppressionFromData}).
#' 
#' Currently, the method has no support for hierarchical data.
#'
#' @param data the input data
#' @param dimVar character vector containing variable names for the output table
#' @param freqVar variable name containing frequency counts
#' @param ... optional parameters that can be passed to the primary suppression
#' method. See \link[SSBtools]{FindDisclosiveCells} for details.
#'
#' @importFrom SSBtools FindDisclosiveCells
#' @return data.frame containing the result of the suppression
#' @export
#'
#' @examples
#' tex <- data.frame(v1 = rep(c('a', 'b', 'c'), times = 4),
#'                   v2 = c('i','i', 'i','h','h','h','i','i','i','h','h','h'),
#'                   v3 = c('y', 'y', 'y', 'y', 'y', 'y','z','z', 'z', 'z', 'z', 'z'),
#'                   freq = c(0,0,5,0,2,3,1,0,3,1,1,2))
#' SuppressDirectDisclosure(tex, c("v1", "v2", "v3"), "freq")
#' SuppressDirectDisclosure(tex, c("v1", "v2", "v3"), "freq", coalition = 2, unknown.threshold = 10)
                  
SuppressDirectDisclosure <- function(data, dimVar, freqVar, ...) {
  
  mm <- SSBtools::ModelMatrix(data, dimVar = dimVar, crossTable = TRUE)
  
  if (ncol(mm$crossTable) < length(dimVar))
    stop("Hierarchies have been detected. This method does not currently support hierarchical data.")
  
  GaussSuppressionFromData(data, dimVar, freqVar, 
                           primary = SSBtools::FindDisclosiveCells, 
                           x = mm$modelMatrix, crossTable = mm$crossTable,
                           singleton = NULL,
                           secondaryZeros = T,
                           ...)
  
}
