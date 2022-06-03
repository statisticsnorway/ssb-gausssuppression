#' SingletonDefault
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#' 
#' This function marks input cells as singletons according to the input frequencies. 
#' Zeros are set to singletons when `protectZeros` or `secondaryZeros` is `TRUE`. 
#' Otherwise, ones are set to singletons.
#'
#' @param data  Input data, possibly pre-aggregated within `GaussSuppressionFromData`  
#' @param freqVar A single variable holding counts (input to `GaussSuppressionFromData`)
#' @param protectZeros Suppression parameter (see `GaussSuppressionFromData`)
#' @param secondaryZeros Suppression parameter (see `GaussSuppressionFromData`)
#' @param ... Unused parameters 
#'
#' @return singleton, \code{\link{GaussSuppression}} input 
#' @export
#' @keywords internal
SingletonDefault <- function(data, freqVar, protectZeros, secondaryZeros, ...) {
  
  if(is.null(protectZeros))   stop("A non-NULL value of protectZeros is required.")
  if(is.null(secondaryZeros)) stop("A non-NULL value of secondaryZeros is required.")
  
  if (protectZeros | secondaryZeros){ 
    return(data[[freqVar]] == 0)
  }
  data[[freqVar]] == 1
}
