#' Suppression of directly-disclosive cells
#' 
#' Function for suppressing directly-disclosive cells in frequency tables. The method
#' detects and primary suppresses directly-disclosive cells with the
#' \link[SSBtools]{FindDisclosiveCells} function, and applies a secondary suppression
#' using Gauss suppression (see \link{GaussSuppressionFromData}).
#' 
#' SuppressDirectDisclosure has no support for hierarchical data. 
#' SuppressDirectDisclosure2 has, but is less general in other ways.
#'
#' @param data the input data
#' @param dimVar main dimensional variables for the output table
#' @param freqVar variable containing frequency counts
#' @param ... optional parameters that can be passed to the primary suppression
#' method. See \link[SSBtools]{FindDisclosiveCells} for details.
#' In the case of SuppressDirectDisclosure2, `...` are parameters to GaussSuppressionFromData. 
#' @param coalition numeric variable, parameter for primary suppression. Default value is 1.
#' @param secondaryZeros logical or numeric value for secondary suppression. If logical, it is converted to resp numeric value (0 or 1). If numeric, it describes the largest number that is prioritized over zeroes in secondary suppression. Default value is equal to coalition.
#' @param candidates function parameter for gauss suppression.
#'
#' @importFrom SSBtools FindDisclosiveCells
#' @return data.frame containing the result of the suppression
#' @export
#' 
#' @author Daniel Lupp
#'
#' @examples
#' tex <- data.frame(v1 = rep(c('a', 'b', 'c'), times = 4),
#'                   v2 = c('i','i', 'i','h','h','h','i','i','i','h','h','h'),
#'                   v3 = c('y', 'y', 'y', 'y', 'y', 'y','z','z', 'z', 'z', 'z', 'z'),
#'                   freq = c(0,0,5,0,2,3,1,0,3,1,1,2))
#' SuppressDirectDisclosure(tex, c("v1", "v2", "v3"), "freq")
#' SuppressDirectDisclosure(tex, c("v1", "v2", "v3"), "freq", coalition = 2, unknown.threshold = 10)
#' 
#' z3 <- SSBtools::SSBtoolsData("z3")
#' a1 <- SuppressDirectDisclosure(z3, c(1, 4, 5), 7)
#' a2 <- SuppressDirectDisclosure2(z3, c(1, 4, 5), 7)
#' b1 <- try(SuppressDirectDisclosure(z3, 1:6, 7))
#' b2 <- SuppressDirectDisclosure2(z3, 1:6, 7)
#' b3 <- SuppressDirectDisclosure2(z3, freqVar = 7, 
#'                formula = ~region * hovedint * mnd2 + (fylke + kostragr) * hovedint * mnd)
                  
SuppressDirectDisclosure <- function(data, dimVar, freqVar,
                                     coalition = 1,
                                     secondaryZeros = coalition,
                                     candidates = DirectDisclosureCandidates,
                                     ...) {
  
  mm <- SSBtools::ModelMatrix(data, dimVar = dimVar, crossTable = TRUE)
  
  if (ncol(mm$crossTable) < length(dimVar))
    stop("Try SuppressDirectDisclosure2? - Hierarchies have been detected. This method does not currently support hierarchical data.")
  if (is.logical(secondaryZeros)) {
    if (secondaryZeros) secondaryZeros <- coalition
    else secondaryZeros <- 0
  }
  GaussSuppressionFromData(data, dimVar, freqVar, 
                           primary = SSBtools::FindDisclosiveCells, 
                           x = mm$modelMatrix, crossTable = mm$crossTable,
                           protectZeros = FALSE,
                           secondaryZeros = secondaryZeros,
                           candidates = candidates,
                           coalition = coalition,
                           ...)
  
}

DirectDisclosureCandidates <- function(freq, x, secondaryZeros, weight, ...) {
  if(is.null(weight))
    weight <- 1
  else{
    if(min(weight)<0){
      weight[weight<0] = 0
      warning("Negative weights treated as zero")
    }
    if(min(weight)==0){
      weight <- weight + max(weight)*1E-20
    } 
  }
  tie <- as.matrix(Matrix::crossprod(x, x %*% ((freq+1)*weight)))
  tie <- tie/max(tie)
  freqOrd <- sapply(freq, function(x) {
                  if (x == 0) return(secondaryZeros)
                  if (x <= secondaryZeros) return(x-1)
                  return(x)
                  })
  if (!secondaryZeros) {
      freqOrd[freq == 0] <- 0.01 + max(freqOrd) + freqOrd[freq == 0]  
  }
  candidates <- order(freqOrd, decreasing = TRUE)
  candidates
}
