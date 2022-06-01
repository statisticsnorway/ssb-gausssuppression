#' CandidatesDefault
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#' 
#' This main part of this function orders the indices decreasingly according to `freq` or, 
#' when `weight` is non-NULL,  `(freq+1)*weight`. Ties are handled by prioritizing output cells 
#' that are calculated from many input cells. In addition, zeros are handled according to parameter `secondaryZeros`. 
#'
#' @param freq Vector of output frequencies 
#' @param x The model matrix
#' @param secondaryZeros When `TRUE`, cells with zero frequency or value are prioritized to 
#'        be published so that they are not secondary suppressed.
#'        This is achieved by this function by having the zero frequency indices first in the retuned order.
#' @param weight Vector of output weights
#' @param ... Unused parameters 
#'
#' @return candidates, \code{\link{GaussSuppression}} input 
#' @export
#' @keywords internal
CandidatesDefault <- function(freq, x, secondaryZeros = FALSE, weight, ...) {
  
  if(is.null(secondaryZeros)) stop("A non-NULL value of secondaryZeros is required.")
  
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
  freqOrd <- ((freq+1)*weight + 0.99 * tie)[, 1, drop = TRUE]
  if (!secondaryZeros) {
    freqOrd[freq == 0] <- 0.01 + max(freqOrd) + freqOrd[freq == 0]
  }
  candidates <- order(freqOrd, decreasing = TRUE)
  candidates
}