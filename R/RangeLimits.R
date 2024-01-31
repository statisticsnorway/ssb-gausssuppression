
#' Default range limit function
#'
#' Preliminary function
#' 
#' @param ... Unused parameters 
#' @param rangePercent rangePercent
#' @param rangeMin rangeMin 
#' @param primary primary 
#' @param num num 
#' @param dominanceVar dominanceVar
#' @param intervalVar intervalVar
#'
#' @return matrix with named columns  
#' 
#' @export
#' 
#' @examples
#' dat <- SSBtoolsData("magnitude1")
#' dat["num2"] <- 1:nrow(dat)
#' 
#' RangeOutputFunction <- GaussSuppression:::RangeOutputFunction
#' 
#' SuppressDominantCells(data = dat, 
#'     numVar = "value", 
#'     formula = ~sector2 * geo + sector4 * eu, 
#'     contributorVar = "company", 
#'     n = 1:2, k = c(80, 99), 
#'     output = RangeOutputFunction, rangePercent = 10, rangeMin = 1)
#' 
#' SuppressDominantCells(data = dat, 
#'     numVar = c("value", "num2"), 
#'     formula = ~sector2 * geo + sector4 * eu, 
#'     contributorVar = "company", 
#'     n = 1:2, k = c(80, 99), 
#'     output = RangeOutputFunction, 
#'     intervalVar = c("value","freq", "num2"), 
#'     rangePercent = c(10, 10, 30), rangeMin = c(1, 0.2222, 2.222))
#' 
RangeLimitsDefault <- function(..., 
                                  rangePercent,
                                  rangeMin,
                                  primary, 
                                  num, 
                                  freq,
                                  freqVar,
                                  dominanceVar = NULL, 
                                  intervalVar = NULL) {
  if (is.null(intervalVar)) {
    if (is.null(dominanceVar)) {
      if (ncol(num) == 0) {
        intervalVar <- freqVar
      } else {
        intervalVar <- names(num)[1]
      }
    } else {
      intervalVar <- dominanceVar
    }
  }
  
  rangePercent <- rep_len(rangePercent, length(intervalVar))
  rangeMin <- rep_len(rangeMin, length(intervalVar))
  
  rangeLimits <- matrix(0, nrow(num), length(intervalVar))
  colnames(rangeLimits) <- intervalVar
  
  for (i in seq_along(intervalVar)) {
    if (intervalVar[i] == freqVar) {
      z <- freq
    } else {
      z <- num[[intervalVar[i]]]
    }
    z[!primary] <- NA
    rangeLimits[, i] <- z * rangePercent[i]/100
    rangeLimits[, i][rangeLimits[, i] < rangeMin[i]] <- rangeMin[i]
  }
  rangeLimits
}


RangeOutputFunction <- function(...) {
  rangeLimits <- RangeLimitsDefault(..., primary = primary, num = num, freq = freq, freqVar = freqVar)
  colnames(rangeLimits) <- paste("rlim", colnames(rangeLimits), sep = "_")
  num <- cbind(num, as.data.frame(rangeLimits))
  environment(TailGaussSuppressionFromData) <- environment()
  return(TailGaussSuppressionFromData(...))
}