



OutputIntervalIteration <- function(..., 
                            minVal = NULL, 
                            allInt = FALSE,
                            maxIterInterval = 100) {
  
  if (is.null(lpPackage)) {
    lpPackage <- "lpSolve"
  }
  
  if (!is.null(forced)) {
    stop("forced when interval iteration not implemented")
  }
  
  rangeLimits <- RangeLimitsDefault(..., primary = primary, num = num, freq = freq, freqVar = freqVar)
  
  if (ncol(rangeLimits) != 1) {
    Stop("Only single intervalVar implemented")
  }
  
  intervalVar <- colnames(rangeLimits)
  colnames(rangeLimits) <- paste("rlim", colnames(rangeLimits), sep = "_")
  num <- cbind(num, as.data.frame(rangeLimits))
  
  if (intervalVar == freqVar) {
    z <- freq
  } else {
    z <- num[[intervalVar]]
  }
  
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  suppressed[hidden] <- TRUE     # in interval computation, hidden similar to  secondary
  
  suppressed_numeric <- rep(0, m)
  suppressed_numeric[primary] <- 0.5
  
  primary_i <- primary
  risky <- primary
  
  
  for(i in seq_len(maxIterInterval)){
    cat("\n\n===================================================================\n")
    cat("========  Interval iteration  ", i, "    ==============================\n")
    cat("===================================================================\n\n\n")
    if(i>1){              
      # To calls to avoid possible error:  argument "whenEmptyUnsuppressed" matched by multiple actual arguments 
      if(hasArg("whenEmptyUnsuppressed") | !structuralEmpty){
        secondary <- GaussSuppression(x = x, candidates = candidates, primary = primary_i, forced = forced, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, xExtraPrimary = xExtraPrimary, 
                                      unsafeAsNegative = TRUE, ...)
      } else {
        secondary <- GaussSuppression(x = x, candidates = candidates, primary = primary_i, forced = forced, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, whenEmptyUnsuppressed = NULL, xExtraPrimary = xExtraPrimary, 
                                      unsafeAsNegative = TRUE, ...)
      }
    }
    suppressed[secondary] <- TRUE
    suppressed_numeric[secondary] = i
    
    gauss_intervals <- ComputeIntervals(
      x = x,
      z = z,
      primary = risky,
      suppressed = suppressed,
      minVal = minVal,
      allInt = allInt,
      lpPackage = lpPackage
    )
    gauss_ranges <- gauss_intervals[, 2] - gauss_intervals[, 1]
    risky_old <- risky
    risky <- (gauss_ranges - rangeLimits[, 1]) < 0
    risky[!risky_old] <- FALSE
    
    primary_from_risky <- PrimaryFromRiskyDefault(x = x, y = z, risky = which(risky), candidates[candidates %in% which(!suppressed)])
    
    primary_i <- suppressed
    primary_i[primary_from_risky] <- TRUE
    suppressed[primary_i] <- TRUE
    
    suppressed_numeric[primary_from_risky] <- i + 0.5
    
    if (!length(primary_from_risky)) {
      break
    }
    
  }
  
  if (length(primary_from_risky)) {
    warning("maxIterInterval reached")
  }
  
  secondary <- which(suppressed & !primary)
  
  num <- cbind(num, as.data.frame(gauss_intervals))
  num <- cbind(num, suppressed_numeric = suppressed_numeric)
  
  environment(TailGaussSuppressionFromData) <- environment()
  return(TailGaussSuppressionFromData(...))
}
