

# SuppressDominantCells(data = SSBtoolsData("magnitude1"), 
#                       numVar = "value", 
#                       formula = ~sector2 * geo + sector4 * eu, 
#                       contributorVar = "company", 
#                       n = 1:2, k = c(80, 99), 
#                       output = GaussSuppression:::OutputFixRiskyIntervals,
#                       rangePercent = 150,
#                       rangeMin = 1,
#                       lpPackage = "Rsymphony")


OutputFixRiskyIntervals <- function(..., 
                            minVal = NULL, 
                            allInt = FALSE,
                            gaussIFix = FALSE, # = gaussI-input to FixRiskyIntervals
                            maxIterInterval = 50,
                            allIntervals = TRUE) {
  
  if (is.null(lpPackage)) {
    lpPackage <- "lpSolve"
  }
  
  if (!is.null(forced)) {
    stop("forced when interval iteration not implemented")
  }
  
  rangeLimits <- RangeLimitsDefault(..., primary = primary, num = num, freq = freq, freqVar = freqVar)
  
  if (ncol(rangeLimits) != 1) {
    stop("Only single intervalVar implemented")
  }
  
  intervalVar <- colnames(rangeLimits)
  colnames(rangeLimits) <- paste("rlim", colnames(rangeLimits), sep = "_")
  num <- cbind(num, as.data.frame(rangeLimits))
  
  if (intervalVar == c(freqVar, "")[1]) { # since freqVar may be NULL
    z <- freq
  } else {
    z <- num[[intervalVar]]
  }
  
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  suppressed[secondary] <- TRUE
  suppressed[hidden] <- TRUE     # in interval computation, hidden similar to  secondary
  
  suppressed_integer <- rep(0L, m)
  suppressed_integer[primary] <- 1L
  suppressed_integer[secondary] <- 2L
  
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed,
    minVal = minVal,
    allInt = allInt,
    lpPackage = lpPackage
  )
  
  gauss_ranges <- gauss_intervals[, 2] - gauss_intervals[, 1]
  risky <- (gauss_ranges - rangeLimits[, 1]) < 0
  risky[!primary] <- FALSE
  
  colnames(gauss_intervals) <- paste(colnames(gauss_intervals), "1", sep = "_")
  num <- cbind(num, as.data.frame(gauss_intervals))
  
  
  newPrimary <- FixRiskyIntervals(
    x = x,
    z = z,
    primary = risky, 
    suppressed = suppressed,
    candidates = candidates, 
    minVal = minVal,
    allInt = allInt,
    gaussI = gaussIFix,
    lpPackage = lpPackage,
    rangeLimits =  rangeLimits[risky, 1]
  )
  
  primary2 <- primary
  primary2[newPrimary] <- TRUE
  
  suppressed_integer[newPrimary] <- 3L
  suppressed1 <- suppressed
  suppressed2 <- suppressed1
  suppressed2[primary2] <- TRUE
  suppressed <- suppressed2
  
  # To calls to avoid possible error:  argument "whenEmptyUnsuppressed" matched by multiple actual arguments 
  if(hasArg("whenEmptyUnsuppressed") | !structuralEmpty){
    secondary <- GaussSuppression(x = x, candidates = candidates, primary = suppressed, forced = forced, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, xExtraPrimary = xExtraPrimary, 
                                  unsafeAsNegative = TRUE, ...)
  } else {
    secondary <- GaussSuppression(x = x, candidates = candidates, primary = suppressed, forced = forced, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, whenEmptyUnsuppressed = NULL, xExtraPrimary = xExtraPrimary, 
                                  unsafeAsNegative = TRUE, ...)
  }
  
  suppressed_integer[secondary] <- 4L
  
  
  suppressed[secondary] <- TRUE
  
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed,
    minVal = minVal,
    allInt = allInt,
    lpPackage = lpPackage
  )
  
  
  gauss_ranges <- gauss_intervals[, 2] - gauss_intervals[, 1]
  risky <- (gauss_ranges - rangeLimits[, 1]) < 0
  risky[!primary] <- FALSE
  
  if (any(is.na(risky))) {
    warning(paste("Missing values in final risk calculation"))
    risky[is.na(risky)] <- FALSE
  }
  
  if(any(risky)){
    warning(paste("Still", sum(risky) ,"risky (Algorithm may iterate in the future)."))
  }
  
  num <- cbind(num, as.data.frame(gauss_intervals))
  
  num <- cbind(num, suppressed_integer = suppressed_integer)
  
  secondary <- which(suppressed & !primary)   # Suppressed is re-calculated in TailGaussSuppressionFromData
  
  environment(TailGaussSuppressionFromData) <- environment()
  return(TailGaussSuppressionFromData(...))
}




# To avoid check problems
utils::globalVariables(c("structuralEmpty"))



