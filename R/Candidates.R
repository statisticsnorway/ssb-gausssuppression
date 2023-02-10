#' Candidates functions
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#' 
#' `CandidatesDefault` orders the indices decreasingly according to `freq` or, 
#' when `weight` is non-NULL,  `(freq+1)*weight`. Ties are handled by prioritizing output cells 
#' that are calculated from many input cells. In addition, zeros are handled according to parameter `secondaryZeros`. 
#' When `freq` is negative (special hierarchy), `abs(freq)*weight` is used.  
#'
#' `CandidatesNum` orders the indices decreasingly according to absolute values of the numeric variable (according to  `abs(num[[1]])`).
#' In practice this is done by running `CandidatesDefault` with manipulated weights.
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
  freq1weight <- FreqPlus1(freq)*weight   # As (freq+1)*weight with treatment of negative freq
  tie <- abs(as.matrix(Matrix::crossprod(x, x %*% (freq1weight))))
  tie <- tie/max(tie)
  freqOrd <- (abs(freq1weight) + 0.99 * tie)[, 1, drop = TRUE]
  if (!secondaryZeros) {
    freqOrd[freq == 0] <- 0.01 + max(freqOrd) + freqOrd[freq == 0]
  }
  candidates <- order(freqOrd, decreasing = TRUE)
  candidates
}


#' @rdname CandidatesDefault
#' @param num Data frame of output aggregates calculated from `numVar`. When several variables, only first is used. 
#' @export
CandidatesNum <- function(secondaryZeros = FALSE, freq = NULL, num, weight, ...) {
  if (ncol(num) > 1){
    warning("Multiple numVar were supplied, only the first is used in candidates function.")
  }
  if (!length(freq)) {
    freq <- rep(1L, nrow(num))
  }
  newWeight <- abs(num[[1]]/FreqPlus1(freq))
  if (!is.null(weight)) {
    newWeight <- newWeight * weight
  }
  CandidatesDefault(weight = newWeight, freq = freq, secondaryZeros = secondaryZeros, ...)
}



# Add ones, but not for negative numbers 
# FreqPlus1(-5:5)
FreqPlus1 <- function(freq) {
  freq <- freq + 1L
  if (min(freq) <= 0) {
    freqNonPos <- freq <= 0
    freq[freqNonPos] <- freq[freqNonPos] - 1L
  }
  freq
}






